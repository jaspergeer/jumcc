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
    op = Add <$ string "+" <* spaces
            <|> Sub <$ string "-" <* spaces
            <|> Mul <$ string "*" <* spaces
            <|> Div <$ string "/" <* spaces
            <|> Band <$ string "&" <* spaces
            <|> Bor <$ string "|" <* spaces
            <|> Xor <$ string "^" <* spaces
            <|> Mod <$ string "%" <* spaces

relExpr :: Parser Expr
relExpr = try (chainl1 binExpr op)
        <|> binExpr where
    op = Gt <$ string ">" <* spaces
            <|> Lt <$ string "<" <* spaces
            <|> Or <$ string "||" <* spaces
            <|> And <$ string "&&" <* spaces
            <|> Ge <$ string ">=" <* spaces
            <|> Le <$ string "<=" <* spaces
            <|> Eq <$ string "==" <* spaces
            <|> Ne <$ string "!=" <* spaces
           

primExpr :: Parser Expr
primExpr = try inExpr
        <|> try (FunE <$> funcCall)
        <|> try intExpr
        <|> try charExpr
        <|> try varExpr
        <|> try (parend expr)

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
        r <- brackd expr
        return $ Dref (Add l r))
        <|> try primExpr

expr :: Parser Expr
expr = try relExpr

-- statements

statement :: Parser Stat
statement = try outS <* spaces
        <|> try ifElseS <* spaces
        <|> try whileS <* spaces
        <|> try returnStat <* spaces
        <|> try assignStat <* spaces
        <|> try declStat <* spaces
        <|> try (FuncCallS <$> funcCall <* spaces <* char ';' <* spaces)
        <|> try (Break <$ string "break" <* spaces <* char ';' <* spaces)

returnStat :: Parser Stat
returnStat = ReturnS <$> (string "return" *> spaces *> expr <* spaces <* char ';')

assignStat :: Parser Stat
assignStat = AssignS <$> expr <* spaces <*> (char '=' *> spaces *> expr <* char ';')

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
varDeclInit x = VarDeclS x <$> (string "=" *> spaces *> expr <* spaces <* string ";")

varDeclNoInit:: VarDecl -> Parser Stat
varDeclNoInit x = VarDeclS x (IntE 0) <$ spaces <* string ";"

arrDeclInit :: VarDecl -> Parser Stat
arrDeclInit x = try (ArrDeclS x <$> (string "=" *> spaces *> strLiteral) <* spaces <* string ";")
        <|> try (ArrDeclS x <$> (string "=" *> spaces *> braced (expr `sepBy` (char ',' <* spaces)) <* spaces <* string ";"))

arrDeclNoInit:: VarDecl -> Parser Stat
arrDeclNoInit x = ArrDeclS x [] <$ spaces <* string ";"

ifElseS :: Parser Stat
ifElseS = IfS <$> (string "if" *> spaces *> parend expr <* spaces) <*> braced (many statement)

whileS :: Parser Stat
whileS = WhileS <$> (string "while" *> spaces *> parend expr <* spaces) <*> braced (many statement)
outS :: Parser Stat
outS = Out <$> (string "outb" *> spaces *> parend expr <* spaces <* char ';')

-- functions

extDecl :: Parser ExtDecl
extDecl = funcDecl

funcDecl :: Parser ExtDecl
funcDecl = FuncDecl <$> cType <*> identifier <*> parend (varDecl `sepBy` (char ',' <* spaces)) <* spaces <*> braced (many statement)

funcCall :: Parser FuncCall
funcCall = FuncCall <$> identifier <*> parend (expr `sepBy ` (char ',' <* spaces))

-- program

program :: Parser Program
program = Program <$> many1 extDecl