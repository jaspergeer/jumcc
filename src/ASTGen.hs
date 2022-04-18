module ASTGen where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec

cSpecSymbs      = "(){}[];,"
cIdentBegin     = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar      = cIdentBegin ++ ['0'..'9']
cKeywords       = ["if","while","return","else"]
cTypes          = ["int", "char"]
cBinOpBegin     = "+-*/=><!"
cBinOp          = ["==", ">", "<", "!=", ">=", "<=", "+", "-", "*", "/"]

-- AST Structure --
data Program = Program FuncDecl deriving Show
data FuncDecl = FuncDecl Ident [Stat] deriving Show
data Stat = Return Expr
        | Assign Var Expr
                deriving Show
data Expr = IntExpr Int
        | CharExpr Char
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
newtype Var = Var Ident deriving Show
newtype Ident = Ident String deriving Show
newtype StrLit = StrLit String deriving Show

-- variable :: Parser Variable
-- variable = do

parend :: Parser a -> Parser a
parend x = between (char '(' <* spaces) (char ')') x <* spaces

brackd :: Parser a -> Parser a
brackd x = between (char '[' <* spaces) (char ']') x <* spaces 

braced :: Parser a -> Parser a
braced x = between (char '{' <* spaces) (char '}') x <* spaces

squoted :: Parser a -> Parser a
squoted x = between (char '\'' <* spaces) (char '\'') x <* spaces

dquoted :: Parser a -> Parser a
dquoted x = between (char '"' <* spaces) (char '"') x <* spaces

-- Primitives --

intLiteral :: Parser Int
intLiteral = read <$> many1 digit <* spaces

strLiteral :: Parser StrLit
strLiteral = StrLit <$> dquoted (many anyChar ) <* spaces

charLiteral :: Parser Char
charLiteral = squoted anyChar <* spaces

identifier :: Parser Ident
identifier = Ident <$> many1 (oneOf cIdentChar) <* spaces

var :: Parser Var
var = Var <$> identifier <* spaces

-- Expressions --

intExpr :: Parser Expr
intExpr = IntExpr <$> intLiteral <* spaces

charExpr :: Parser Expr
charExpr = CharExpr <$> charLiteral <* spaces

varExpr :: Parser Expr
varExpr = VarExpr <$> var

expr :: Parser Expr
expr = chainl1 term op
    where op = try (GeExpr <$ string ">=" <* spaces)
            <|> try (LeExpr <$ string "<=" <* spaces)
            <|> try (EqExpr <$ string "==" <* spaces)
            <|> try (NeExpr <$ string "!=" <* spaces)
            <|> GtExpr <$ string ">" <* spaces
            <|> LtExpr <$ string "<" <* spaces
            <|> AddExpr <$ string "+" <* spaces
            <|> SubExpr <$ string "-" <* spaces
            <|> MulExpr <$ string "*" <* spaces
            <|> DivExpr <$ string "/" <* spaces

term :: Parser Expr
term = try intExpr
    <|> try charExpr
    <|> try varExpr
    <|> try (parend expr)

