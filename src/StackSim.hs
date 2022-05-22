module StackSim where

import ASTGen
    ( CType(..),
      Var(..),
      Stat(WhileS, VarDeclS, IfS),
      Param(Param))

data StkFrame = StkFrame [(Var, Int)] Int deriving Show
newtype StackSim = StackSim [StkFrame] deriving Show

stkFrameEmpty :: StkFrame
stkFrameEmpty = StkFrame [] 0

stkSimEmpty :: StackSim
stkSimEmpty = StackSim []

stkSimQuery :: StackSim -> Var-> Int
stkSimQuery (StackSim stk) = _ssq stk where
    _ssq [] (Var n) = error $ "couldn't recognize symbol " ++ n
    _ssq (x@(StkFrame entries size):xs) v = case lookup v entries of
        Just n -> n
        Nothing -> stkSimQuery (StackSim xs) v + size

stkSimPushFrame :: StackSim -> StackSim
stkSimPushFrame (StackSim stk) = StackSim (stkFrameEmpty:stk)

stkSimPopFrame :: StackSim -> StackSim
stkSimPopFrame (StackSim (x:xs)) = StackSim xs
stkSimPopFrame (StackSim []) = error "can't pop frame: stack empty"

stkSimPush :: StackSim -> (Var, CType) -> StackSim
stkSimPush (StackSim (f:fs)) x = StackSim (_ssp f x:fs) where
    _ssp (StkFrame f s) (v, typ) = StkFrame ((v, 0):map (\(x,y) -> (x, y + typeSize typ)) f) (s + typeSize typ)
stkSimPush (StackSim []) _ = error "can't push value: stack empty"

stkSimPop :: StackSim -> StackSim
stkSimPop (StackSim ((StkFrame (f:fs) s):stk)) = StackSim (StkFrame fs s:stk)
stkSimPop (StackSim _) = error "can't pop value: stack empty"

typeSize :: CType -> Int
typeSize (Ptr _) = 1
typeSize CInt = 1
typeSize CChar = 1
typeSize (Arr n typ) = n * typeSize typ