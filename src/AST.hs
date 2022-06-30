module AST where

import CType ( CType )
import Var ( Var )

newtype AST = AST [ExtDecl] deriving Show
data ExtDecl = FuncDefn CType String [VarDecl] [Stat]
            | FuncDecl CType String [VarDecl] deriving Show
data FuncCall = FuncCall String [Expr] deriving Show
data VarDecl = VarDecl CType Var deriving Show
data Stat = ReturnS Expr
        | AssignS Expr Expr
        | VarDeclS VarDecl Expr
        | ArrDeclS VarDecl [Expr]
        | FuncCallS FuncCall
        | IfS Expr [Stat]
        | WhileS Expr [Stat]
        | Out Expr
        | Break
        deriving Show
data Expr = IntE Int
        | CharE Char
        | VarE Var
        | RefE Var
        | Neg Expr
        | Lno Expr
        | No Expr
        | Dref Expr
        | FunE FuncCall
        | Ge Expr Expr
        | Le Expr Expr
        | Eq Expr Expr
        | Ne Expr Expr
        | Gt Expr Expr
        | Lt Expr Expr
        | Or Expr Expr
        | And Expr Expr
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Band Expr Expr
        | Bor Expr Expr
        | Xor Expr Expr
        | Mod Expr Expr
        | Str String
        | In
        | CastE CType Expr
        deriving Show