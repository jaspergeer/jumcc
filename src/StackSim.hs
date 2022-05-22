module StackSim where

import ASTGen
    ( CType(..),
      Var(..),
      Stat(WhileS, VarDeclS, IfS),
      Param(Param))

data StkFrame = StkFrame String [(Var, Int)] Int deriving Show
newtype StackSim = StackSim [StkFrame] deriving Show

stkSimEmpty :: StackSim
stkSimEmpty = StackSim []

stkSimQuery :: StackSim -> Var-> Int
stkSimQuery (StackSim stk) = _ssq stk where
    _ssq [] (Var n) = error $ "couldn't recognize symbol " ++ n
    _ssq (x@(StkFrame _ entries size):xs) v = case lookup v entries of
        Just n -> n
        Nothing -> stkSimQuery (StackSim xs) v + size

stkSimPushFrame :: StackSim -> String -> StackSim
stkSimPushFrame (StackSim stk) n = StackSim (StkFrame n [] 0:stk)

stkSimPopFrame :: StackSim -> StackSim
stkSimPopFrame (StackSim (x:xs)) = StackSim xs
stkSimPopFrame (StackSim []) = error "can't pop frame: stack empty"

stkSimPush :: StackSim -> (Var, CType) -> StackSim
stkSimPush (StackSim (f:fs)) x = StackSim (_spush f x:fs) where
    _spush (StkFrame n f s) (v, typ) = StkFrame n ((v, 0):map (\(x,y) -> (x, y + typeSize typ)) f) (s + typeSize typ)
stkSimPush (StackSim []) _ = error "can't push value: stack empty"

stkSimPop :: StackSim -> StackSim
stkSimPop (StackSim ((StkFrame n (f:fs) s):stk)) = StackSim (StkFrame n (_spop fs) (s - 1):stk) where
    _spop ((x, y):xs) = (x, y - 1):_spop xs
    _spop [] = []
stkSimPop (StackSim _) = error "can't pop value: stack empty"

typeSize :: CType -> Int
typeSize (Ptr _) = 1
typeSize CInt = 1
typeSize CChar = 1
typeSize (Arr n typ) = n * typeSize typ