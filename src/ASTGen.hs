{-
 - ASTGen.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module ASTGen where

import Text.Parsec.String (Parser)
import Text.Parsec.Char ( char, digit, oneOf, spaces, string )
import Text.Parsec
    ( ParsecT,
      Stream,
      char,
      digit,
      oneOf,
      spaces,
      string,
      between,
      chainl1,
      many1,
      sepBy,
      (<|>),
      many,
      try )
import Text.Parsec.Expr (buildExpressionParser)

cSpecSymbs :: String
cSpecSymbs      = "(){}[];,"
cIdentBegin :: [Char]
cIdentBegin     = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar :: [Char]
cIdentChar      = cIdentBegin ++ ['0'..'9']
cKeywords :: [String]
cKeywords       = ["if","while","return","else"]
cTypes :: [String]
cTypes          = ["int", "char"]
strLitChar :: [Char]
strLitChar = ' ':'!' : ['#'..'&'] ++ ['('..'~']

-- AST structure

newtype Program = Program [ExtDecl] deriving Show
data ExtDecl = FuncDecl CType String [VarDecl] [Stat] deriving Show
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
        deriving Show
newtype Var = Var String deriving (Show, Eq)
data CType = U32
        | U8
        | Ptr CType
        | Arr Int CType
        deriving Show

-- left recursion

leftRec :: (Stream s m t)
        => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
leftRec p op = rest =<< p where
    rest x = do f <- op
                rest (f x)
            <|> return x

-- between wrappers

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

-- primitives

intLiteral :: Parser Int
intLiteral = read <$> many1 digit <* spaces

strLiteral :: Parser [Expr]
strLiteral =  do
        s <- dquoted (many (CharE <$> cChar)) <* spaces
        return (s ++ [CharE '\0'])

charLiteral :: Parser Char
charLiteral = squoted cChar

cChar :: Parser Char
cChar = try ('\0' <$ string "\\0")
        <|> try ('\n' <$ string "\\n")
        <|> try ('"' <$ string "\\\"")
        <|> try ('\'' <$ string "\'")
        <|> try (oneOf strLitChar)

identifier :: Parser String
identifier = many1 (oneOf cIdentChar) <* spaces

var :: Parser Var
var = Var <$> identifier <* spaces

cType :: Parser CType
cType = try (leftRec b ptr)
    <|> try b
    where
        ptr = Ptr <$ spaces <* char '*' <* spaces
        b = try (U32 <$ string "int" <* spaces)
            <|> try (U8 <$ string "char" <* spaces)

-- expressions

intExpr :: Parser Expr
intExpr = IntE <$> intLiteral <* spaces

charExpr :: Parser Expr
charExpr = CharE <$> charLiteral <* spaces

varExpr :: Parser Expr
varExpr = VarE <$> var

inExpr :: Parser Expr
inExpr = In <$ string "inb()"

binExpr :: Parser Expr
binExpr = try (chainl1 prefixExpr op)
        <|> prefixExpr where
    op = try (Add <$ string "+" <* spaces)
            <|> try (Sub <$ string "-" <* spaces)
            <|> try (Mul <$ string "*" <* spaces)
            <|> try (Div <$ string "/" <* spaces)
            <|> try (Band <$ string "&" <* spaces)
            <|> try (Bor <$ string "|" <* spaces)
            <|> try (Xor <$ string "^" <* spaces)
            <|> try (Mod <$ string "%" <* spaces)

relExpr :: Parser Expr
relExpr = try (chainl1 binExpr op)
        <|> binExpr where
    op = try (Or <$ string "||" <* spaces)
            <|> try (And <$ string "&&" <* spaces)
            <|> try (Ge <$ string ">=" <* spaces)
            <|> try (Le <$ string "<=" <* spaces)
            <|> try (Eq <$ string "==" <* spaces)
            <|> try (Ne <$ string "!=" <* spaces)
            <|> try (Gt <$ string ">" <* spaces)
            <|> try (Lt <$ string "<" <* spaces)


primExpr :: Parser Expr
primExpr = try inExpr
        <|> try (FunE <$> funcCall)
        <|> try intExpr
        <|> try charExpr
        <|> try varExpr
        <|> try (parend relExpr)

prefixExpr :: Parser Expr
prefixExpr = try (Neg <$> (string "-" *> primExpr))
        <|> try (No <$> (string "!" *> primExpr))
        <|> try (Lno <$> (string "~" *> primExpr))
        <|> try (Dref <$> (string "*" *> primExpr))
        <|> try (RefE <$> (string "&" *> var))
        <|> try postfixExpr

postfixExpr :: Parser Expr
postfixExpr = try (do
        l <- primExpr
        r <- brackd relExpr
        return $ Dref (Add l r))
        <|> try primExpr

expr :: Parser Expr
expr = try (Str <$> dquoted (many cChar) <* spaces)
    <|> try relExpr

-- statements

statement :: Parser Stat
statement = try outS <* spaces
        <|> try ifElseS <* spaces
        <|> try whileS <* spaces
        <|> try returnStat <* spaces
        <|> try declStat <* spaces
        <|> try assignStat <* spaces
        <|> try (FuncCallS <$> funcCall <* spaces <* char ';' <* spaces)
        <|> try (Break <$ string "break" <* spaces <* char ';' <* spaces)

returnStat :: Parser Stat
returnStat = ReturnS <$> (string "return" *> spaces *> relExpr <* spaces <* char ';')

assignStat :: Parser Stat
assignStat = AssignS <$> relExpr <* spaces <*> (char '=' *> spaces *> relExpr <* char ';')

varDecl :: Parser VarDecl
varDecl = do
    typ <- try (U32 <$ string "int" <* spaces)
            <|> try (U8 <$ string "char" <* spaces)
    ptrs <- many (spaces *> string "*" *> spaces)
    v <- var
    a <- arr typ
    spaces
    return $ VarDecl (ptr ptrs a) v where
            arr t = try (Arr <$> brackd intLiteral <*> arr t)
                <|> try (Arr 0 <$ string "[" <* spaces <* string "]" <*> arr t)
                <|> return t
            ptr (x:xs) t = ptr xs (Ptr t)
            ptr [] t = t

declStat :: Parser Stat
declStat = do
    v <- varDecl
    spaces
    case v of
        (VarDecl (Arr _ _) _) -> try (arrDeclInit v)
                                <|> try (arrDeclNoInit v)
        _ -> try (varDeclInit v)
            <|> try (varDeclNoInit v)

varDeclInit :: VarDecl -> Parser Stat
varDeclInit x = VarDeclS x <$> (string "=" *> spaces *> relExpr <* spaces <* string ";")

varDeclNoInit:: VarDecl -> Parser Stat
varDeclNoInit x = VarDeclS x (IntE 0) <$ spaces <* string ";"

arrDeclInit :: VarDecl -> Parser Stat
arrDeclInit x = try (ArrDeclS x <$> (string "=" *> spaces *> strLiteral) <* spaces <* string ";")
        <|> try (ArrDeclS x <$> (string "=" *> spaces *> braced (relExpr `sepBy` (char ',' <* spaces)) <* spaces <* string ";"))

arrDeclNoInit:: VarDecl -> Parser Stat
arrDeclNoInit x = ArrDeclS x [] <$ spaces <* string ";"

ifElseS :: Parser Stat
ifElseS = IfS <$> (string "if" *> spaces *> parend relExpr <* spaces) <*> braced (many statement)

whileS :: Parser Stat
whileS = WhileS <$> (string "while" *> spaces *> parend relExpr <* spaces) <*> braced (many statement)
outS :: Parser Stat
outS = Out <$> (string "outb" *> spaces *> parend relExpr <* spaces <* char ';')

-- functions

extDecl :: Parser ExtDecl
extDecl = funcDecl

funcDecl :: Parser ExtDecl
funcDecl = FuncDecl <$> cType <*> identifier <*> parend (varDecl `sepBy` (char ',' <* spaces)) <* spaces <*> braced (many statement)

funcCall :: Parser FuncCall
funcCall = FuncCall <$> identifier <*> parend (expr `sepBy ` (char ',' <* spaces))

-- program

program :: Parser Program
program = Program <$> many1 (spaces *> extDecl <* spaces)
