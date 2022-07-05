module AST where

import CType ( CType )
import ASTUtils ( Identifier ) 

newtype AST = AST [ExtDecl] deriving Show
data ExtDecl = FuncDefn CType Identifier [VarDecl] [Stat]
            | FuncDecl CType Identifier [VarDecl] deriving Show
data FuncCall = FuncCall Identifier [Expr] deriving Show
data VarDecl = VarDecl CType Identifier deriving Show
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
        | VarE Identifier
        | RefE Identifier
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