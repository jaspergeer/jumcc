module ASTGen where

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
data Program = Program FuncDecl deriving Show
data FuncDecl = FuncDecl Identifier [Stat] deriving Show
data Stat = NoOp
        | Return Expr
        | Assign Var Expr
                deriving Show
data Expr = ConstExpr Constant
        | VarExpr Var
        | GeExpr Expr Expr
        | LeExpr Expr Expr
        | EqExpr Expr Expr
        | NeExpr Expr Expr
        | GtExpr Expr Expr
        | LtExpr Expr Expr
        | AddExpr Expr Expr
        | SubExpr Expr Expr
        | MulExpr Expr Expr
        | DivExpr Expr Expr
        deriving Show
data Var = Var Identifier deriving Show

-- variable :: Parser Variable
-- variable = do

parend :: Parser a -> Parser a
parend x = between (char '(' <* spaces) (char ')') x <* spaces

brackd :: Parser a -> Parser a
brackd x = between (char '[' <* spaces) (char ']') x <* spaces 

braced :: Parser a -> Parser a
braced x = between (char '{' <* spaces) (char '}') x <* spaces

constant :: Parser Constant
constant = read <$> many1 digit <* spaces

identifier :: Parser Identifier
identifier = many1 (oneOf cIdentChar) <* spaces

constExpr :: Parser Expr
constExpr = ConstExpr <$> constant <* spaces

expr :: Parser Expr
expr = chainl1 term op
    where op = try (GeExpr <$ string ">=")
            <|> try (LeExpr <$ string "<=")
            <|> try (EqExpr <$ string "==")
            <|> try (NeExpr <$ string "!=")
            <|> GtExpr <$ string ">"
            <|> LtExpr <$ string "<"
            <|> AddExpr <$ string "+"
            <|> SubExpr <$ string "-"
            <|> MulExpr <$ string "*"
            <|> DivExpr <$ string "/"

term :: Parser Expr
term = try constExpr
    <|> try (parend expr)