module Parser where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec

type Identifier = String
type Constant = Int
data BinOp = Add | Sub | Mul | Div | Gt | Ge | Lt | Le | Eq | Ne deriving Show

cSpecSymbs      = "(){}[];,"
cIdentBegin     = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar      = cIdentBegin ++ ['0'..'9']
cKeywords       = ["if","while","return","else"]
cTypes          = ["int", "char"]
cBinOpBegin     = "+-*/=><!"
cBinOp          = ["==", ">", "<", "!=", ">=", "<=", "+", "-", "*", "/"]

-- AST Structure --
data Program = Prog FunctionDecl deriving Show
data FunctionDecl = FuncDecl Identifier [Statement] deriving Show
data Statement = NoOp
                | Return Expression
                | Assign Variable Expression
                deriving Show
data Expression = ConstExpr Constant
                | VarExpr Variable
                | BinExpr Expression BinOp Expression
                deriving Show
data Variable = Var Identifier deriving Show

-- variable :: Parser Variable
-- variable = do

constant :: Parser Constant
constant = do
    n <- many1 digit
    return (read n)

identifier :: Parser Identifier
identifier = many1 (oneOf cIdentChar)

binop :: Parser BinOp
binop = do
    o <- oneOf cBinOpBegin
    p <- many (char '=')
    case o:p of
        ">"     -> return Gt
        ">="    -> return Ge
        "<"     -> return Lt
        "<="    -> return Le
        "=="    -> return Eq
        "!="    -> return Ne
        "+"     -> return Add
        "-"     -> return Sub
        "*"     -> return Mul
        "/"     -> return Div
        _       -> fail "could not parse binary operator"