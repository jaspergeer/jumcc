{-
 - StackSim.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module StackSim where

import ASTGen
    ( CType(..),
      Var(..),
      Stat(WhileS, VarDeclS, IfS))

data StkFrame = StkFrame String [(Var, Int, CType)] Int deriving Show
newtype StackSim = StackSim [StkFrame] deriving Show

stkSimEmpty :: StackSim
stkSimEmpty = StackSim []

stkSimQuery :: StackSim -> Var-> Int
stkSimQuery (StackSim stk) = _ssq stk where
    _ssq [] (Var n) = error $ "couldn't recognize symbol " ++ n
    _ssq (x@(StkFrame _ entries size):xs) v = case _frmsrch v entries of
        Just (n,_) -> n
        Nothing -> stkSimQuery (StackSim xs) v + size

stkSimQueryType :: StackSim -> Var -> CType
stkSimQueryType (StackSim stk) = _ssqt stk where
    _ssqt [] (Var n) = error $ "couldn't recognize symbol " ++ n
    _ssqt (x@(StkFrame _ entries size):xs) v = case _frmsrch v entries of
        Just (_,t) -> t
        Nothing -> stkSimQueryType (StackSim xs) v

_frmsrch :: Var -> [(Var, Int, CType)] -> Maybe (Int, CType)
_frmsrch x [] = Nothing
_frmsrch x (y@(key,val,typ):ys) = if x == key then Just (val,typ) else _frmsrch x ys

stkSimPeekType :: StackSim -> CType
stkSimPeekType (StackSim ((StkFrame _ ((_, _, t):xs) _):ys)) = t
stkSimPeekType _ = error "can't peek: no entries on stack"

stkSimPushFrame :: StackSim -> String -> StackSim
stkSimPushFrame (StackSim stk) n = StackSim (StkFrame n [] 0:stk)

stkSimPopFrame :: StackSim -> StackSim
stkSimPopFrame (StackSim (x:xs)) = StackSim xs
stkSimPopFrame (StackSim []) = error "can't pop frame: stack empty"

stkSimPush :: StackSim -> (Var, CType) -> StackSim
stkSimPush (StackSim (f:fs)) x = StackSim (_spush f x:fs) where
    _spush (StkFrame n f s) (v, typ) = StkFrame n ((v, 0, typ):map (\(x,y,z) -> (x, y + 1, z)) f) (s + 1)
stkSimPush (StackSim []) _ = error "can't push value: stack empty"

stkSimPop :: StackSim -> StackSim
stkSimPop (StackSim ((StkFrame n (f:fs) s):stk)) = StackSim (StkFrame n (_spop fs) (s - 1):stk) where
    _spop ((x, y, z):xs) = (x, y - 1, z):_spop xs
    _spop [] = []
stkSimPop (StackSim _) = error "can't pop value: stack empty"

typeSize :: CType -> Int
typeSize (Ptr _) = 1
typeSize U8 = 1
typeSize U32 = 1
typeSize (Arr n typ) = 1 + n