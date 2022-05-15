module SymbTab where

import ASTGen
import AsmGen

-- map of variables to 
data STable = STable [(Var, Integer)] Integer STable | SEmpty deriving Show

sTableQuery :: STable -> Var-> Integer
sTableQuery SEmpty (Var x) = error $ "couldn't recognize symbol " ++ x
sTableQuery (STable table offset parent) x = case lookup x table of
    Just n -> n
    Nothing -> sTableQuery parent x + offset

sTVisitFuncDecl :: FuncDecl -> STable
sTVisitFuncDecl = _stvfd SEmpty

_stvfd :: STable -> FuncDecl -> STable
_stvfd SEmpty x = _stvfd (STable [] 0 SEmpty) x
_stvfd (STable table offset parent) (FuncDecl x y ((Param _ var):xs) z) =
    _stvfd (STable ((var, offset):table) (offset + 1) parent) (FuncDecl x y xs z)
_stvfd x (FuncDecl _ _ [] _) = x

sTVisitStat :: STable -> Stat -> STable
sTVisitStat (STable table offset parent) (VarDeclS _ var) = STable ((var, offset):table) (offset + 1) parent
sTVisitStat x@(STable _ offset _) (IfS _ _) = STable [] offset x
sTVisitStat x@(STable _ offset _) (WhileS _ _) = STable [] offset x
sTVisitStat x _ = x