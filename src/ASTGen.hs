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

-- AST structure --

newtype Program = Program [FuncDecl] deriving Show
data FuncDecl = FuncDecl CType String [Param] [Stat] deriving Show
data FuncCall = FuncCall String [Expr] deriving Show
data Param = Param CType Var deriving Show
data Stat = ReturnS Expr
        | AssignS Var Expr
        | VarDeclS CType Var
        | FuncCallS FuncCall
        | IfS Expr [Stat]
        | WhileS Expr [Stat]
        deriving Show
data Expr = IntE Int
        | CharE Char
        | VarE Var
        | FunE FuncCall
        | Ge Expr Expr
        | Le Expr Expr
        | Eq Expr Expr
        | Ne Expr Expr
        | Gt Expr Expr
        | Lt Expr Expr
        | Add Expr Expr
        | Sub Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        deriving Show
newtype Var = Var String deriving (Show, Eq)
newtype StrLit = StrLit String deriving Show
data CType = CInt 
        | CChar 
        | Ptr CType
        | Arr Int CType
        deriving Show

-- left recursion --

leftRec :: (Stream s m t)
        => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
leftRec p op = rest =<< p where
    rest x = do f <- op
                rest (f x)
            <|> return x

-- between wrappers --

parend :: Parser a -> Parser a
parend x = between (char '(' <* spaces) (char ')') x <* spaces

brackd :: Parser a -> Parser a
brackd x = between (char '[' <* spaces) (char ']') x <* spaces 

braced :: Parser a -> Parser a
braced x = between (char '{' <* spaces) (char '}') x <* spaces

squoted :: Parser a -> Parser a
squoted x = between (char '\'') (char '\'') x <* spaces

dquoted :: Parser a -> Parser a
dquoted x = between (char '"') (char '"') x <* spaces

-- primitives --

intLiteral :: Parser Int
intLiteral = read <$> many1 digit <* spaces

strLiteral :: Parser StrLit
strLiteral = StrLit <$> dquoted (many anyChar ) <* spaces

charLiteral :: Parser Char
charLiteral = squoted anyChar <* spaces

identifier :: Parser String
identifier = many1 (oneOf cIdentChar) <* spaces

var :: Parser Var
var = Var <$> identifier <* spaces

cType :: Parser CType
cType = try (leftRec b ptr)
    <|> try b
    where
        ptr = Ptr <$ spaces <* char '*' <* spaces
        b = try (CInt <$ string "int" <* spaces)
            <|> try (CChar <$ string "char" <* spaces)

-- expressions --

intExpr :: Parser Expr
intExpr = IntE <$> intLiteral <* spaces

charExpr :: Parser Expr
charExpr = CharE <$> charLiteral <* spaces

varExpr :: Parser Expr
varExpr = VarE <$> var

expr :: Parser Expr
expr = try (chainl1 term op)
    <|> try term where
    op = try (Ge <$ string ">=" <* spaces)
            <|> try (Le <$ string "<=" <* spaces)
            <|> try (Eq <$ string "==" <* spaces)
            <|> try (Ne <$ string "!=" <* spaces)
            <|> Gt <$ string ">" <* spaces
            <|> Lt <$ string "<" <* spaces
            <|> Add <$ string "+" <* spaces
            <|> Sub <$ string "-" <* spaces
            <|> Mul <$ string "*" <* spaces
            <|> Div <$ string "/" <* spaces
    term = try (FunE <$> funcCall)
        <|> try intExpr
        <|> try charExpr
        <|> try varExpr
        <|> try (parend expr)

-- statements --

statement :: Parser Stat
statement = try ifElseS <* spaces
        <|> try whileS <* spaces
        <|> try returnStat <* spaces
        <|> try assignStat <* spaces
        <|> try varDeclStat <* spaces
        <|> try (FuncCallS <$> funcCall)

returnStat :: Parser Stat
returnStat = ReturnS <$> (string "return" *> spaces *> expr <* spaces <* char ';')

assignStat :: Parser Stat
assignStat = AssignS <$> var <* spaces <*> (char '=' *> spaces *> expr <* char ';')

varDeclStat :: Parser Stat
varDeclStat = do 
    t <- cType
    n <- var
    a <- arr t
    spaces
    char ';'
    return $ VarDeclS a n where
        arr t = try ( Arr <$> brackd intLiteral <*> arr t)
            <|> return t

ifElseS :: Parser Stat
ifElseS = IfS <$> (string "if" *> spaces *> parend expr) <*> braced (many statement)

whileS :: Parser Stat
whileS = WhileS <$> (string "while" *> spaces *> parend expr) <*> braced (many statement)

-- functions --

funcDecl :: Parser FuncDecl
funcDecl = FuncDecl <$> cType <*> identifier <*> parend (param `sepBy` (char ',' <* spaces)) <* spaces <*> braced (many statement) where
    param = Param <$> cType <*> var <* spaces

funcCall :: Parser FuncCall
funcCall = FuncCall <$> identifier <*> parend (expr `sepBy ` (char ',' <* spaces))

-- program --

program :: Parser Program
program = Program <$> many1 funcDecl