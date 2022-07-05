{-
 - ASTGen.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT License
 -}

module ASTGen where

import Text.Parsec.String (Parser)
import Text.Parsec.Char ( char, digit, oneOf, spaces, string, noneOf )
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
      try, getPosition, SourcePos, setPosition )
import Text.Parsec.Expr (buildExpressionParser)
import CType ( CType(..) )
import AnnAST
    ( AExpr(..),
      AStat(..),
      AVarDecl(..),
      AFuncCall(..),
      AExtDecl(..),
      AnnAST(..))
import ASTUtils
import Text.Parsec.Pos ( SourcePos, newPos )

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


annotate :: (SourcePos -> t) -> (t -> Parser a) -> Parser a
annotate f p = do
        pos <- getPosition
        p $ f pos

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

strLiteral :: Parser [AExpr]
strLiteral =  do
        s <- dquoted (many (annotate ACharE (<$> cChar))) <* spaces
        pos <- getPosition
        return (s ++ [ACharE pos '\0'])

charLiteral :: Parser Char
charLiteral = squoted cChar

cChar :: Parser Char
cChar = try ('\0' <$ string "\\0")
        <|> try ('\n' <$ string "\\n")
        <|> try ('"' <$ string "\\\"")
        <|> try ('\'' <$ string "\'")
        <|> try (oneOf strLitChar)

identifier :: Parser Identifier
identifier = many1 (oneOf cIdentChar) <* spaces

cType :: Parser CType
cType = try (leftRec b ptr)
    <|> try b
    where
        ptr = Ptr <$ spaces <* char '*' <* spaces
        b = try (Int <$ string "int" <* spaces)
            <|> try (Char <$ string "char" <* spaces)

funcCall :: Parser AFuncCall
funcCall = do
        pos <- getPosition
        AFuncCall pos <$> identifier <*> parend (expr `sepBy ` (char ',' <* spaces))

-- expressions

intExpr :: Parser AExpr
intExpr = annotate AIntE (<$> (intLiteral <* spaces))

charExpr :: Parser AExpr
charExpr = annotate ACharE (<$> (charLiteral <* spaces))

varExpr :: Parser AExpr
varExpr = annotate AVarE (<$> identifier)

inExpr :: Parser AExpr
inExpr = annotate AIn (<$ string "inb()")

castExpr :: Parser AExpr
castExpr = do
        pos <- getPosition
        typ <- parend cType
        spaces
        ACastE pos typ <$> prefixExpr

binExpr :: Parser AExpr
binExpr = try (chainl1 prefixExpr op)
        <|> prefixExpr where
    op = do
        pos <- getPosition
        try (AAdd pos <$ string "+" <* spaces)
            <|> try (ASub pos <$ string "-" <* spaces)
            <|> try (AMul pos <$ string "*" <* spaces)
            <|> try (ADiv pos <$ string "/" <* spaces)
            <|> try (ABand pos <$ string "&" <* spaces)
            <|> try (ABor pos <$ string "|" <* spaces)
            <|> try (AXor pos <$ string "^" <* spaces)
            <|> try (AMod pos <$ string "%" <* spaces)

relExpr :: Parser AExpr
relExpr = try (chainl1 binExpr op)
        <|> binExpr where
    op = do
        pos <- getPosition
        try (AOr pos <$ string "||" <* spaces)
            <|> try (AAnd pos <$ string "&&" <* spaces)
            <|> try (AGe pos <$ string ">=" <* spaces)
            <|> try (ALe pos <$ string "<=" <* spaces)
            <|> try (AEq pos <$ string "==" <* spaces)
            <|> try (ANe pos <$ string "!=" <* spaces)
            <|> try (AGt pos <$ string ">" <* spaces)
            <|> try (ALt pos <$ string "<" <* spaces)


primExpr :: Parser AExpr
primExpr = try inExpr
        <|> try (annotate AFunE (<$> funcCall))
        <|> try intExpr
        <|> try charExpr
        <|> try varExpr
        <|> try (parend relExpr)

prefixExpr :: Parser AExpr
prefixExpr = try (annotate ANeg (<$> (string "-" *> primExpr)))
        <|> try (annotate ANo (<$> (string "!" *> primExpr)))
        <|> try (annotate ALno (<$> (string "~" *> primExpr)))
        <|> try (annotate ADref (<$> (string "*" *> primExpr)))
        <|> try (annotate ARefE (<$> (string "&" *> identifier)))
        <|> try castExpr
        <|> try postfixExpr

postfixExpr :: Parser AExpr
postfixExpr = try (do
        pos <- getPosition
        l <- primExpr
        r <- brackd relExpr
        return $ ADref pos (AAdd pos l r))
        <|> try primExpr

expr :: Parser AExpr
expr = try (annotate AStr (<$> (dquoted (many cChar) <* spaces)))
    <|> try relExpr

-- statements

statement :: Parser AStat
statement = try outS <* spaces
        <|> try ifElseS <* spaces
        <|> try whileS <* spaces
        <|> try returnStat <* spaces
        <|> try declStat <* spaces
        <|> try assignStat <* spaces
        <|> try (annotate AFuncCallS (<$> (funcCall <* spaces <* char ';' <* spaces)))
        <|> try (annotate ABreak (<$ (string "break" <* spaces <* char ';' <* spaces)))

returnStat :: Parser AStat
returnStat = annotate AReturnS (<$> (string "return" *> spaces *> relExpr <* spaces <* char ';'))

assignStat :: Parser AStat
assignStat = do
        pos <- getPosition
        AAssignS pos <$> relExpr <* spaces <*> (char '=' *> spaces *> relExpr <* char ';')

varDecl :: Parser AVarDecl
varDecl = do
    pos <- getPosition
    typ <- try (Int <$ string "int" <* spaces)
            <|> try (Char <$ string "char" <* spaces)
    ptrs <- many (spaces *> string "*" *> spaces)
    v <- identifier
    a <- arr typ
    spaces
    return $ AVarDecl pos (ptr ptrs a) v where
            arr t = try (Arr <$> brackd intLiteral <*> arr t)
                <|> try (Arr 0 <$ string "[" <* spaces <* string "]" <*> arr t)
                <|> return t
            ptr (x:xs) t = ptr xs (Ptr t)
            ptr [] t = t

declStat :: Parser AStat
declStat = do
    v <- varDecl
    spaces
    case v of
        (AVarDecl _ (Arr _ _) _ ) -> try (arrDeclInit v)
                                <|> try (arrDeclNoInit v)
        _ -> try (varDeclInit v)
            <|> try (varDeclNoInit v)

varDeclInit :: AVarDecl -> Parser AStat
varDeclInit x = do
        pos <- getPosition
        AVarDeclS pos x <$> (string "=" *> spaces *> relExpr <* spaces <* string ";")

varDeclNoInit:: AVarDecl -> Parser AStat
varDeclNoInit x = do
        pos <- getPosition
        AVarDeclS pos x (AIntE pos 0) <$ spaces <* string ";"

arrDeclInit :: AVarDecl -> Parser AStat
arrDeclInit x = do
        pos <- getPosition
        _parseArr pos where
                _parseArr pos = try (AArrDeclS pos x <$> (string "=" *> spaces *> strLiteral) <* spaces <* string ";")
                        <|> try (AArrDeclS pos x <$> (string "=" *> spaces *> braced (relExpr `sepBy` (char ',' <* spaces)) <* spaces <* string ";"))


arrDeclNoInit:: AVarDecl -> Parser AStat
arrDeclNoInit x = do
        pos <- getPosition
        AArrDeclS pos x [] <$ spaces <* string ";"

ifElseS :: Parser AStat
ifElseS = do
        pos <- getPosition
        AIfS pos <$> (string "if" *> spaces *> parend relExpr <* spaces) <*> braced (many statement)

whileS :: Parser AStat
whileS = do
        pos <- getPosition
        AWhileS pos <$> (string "while" *> spaces *> parend relExpr <* spaces) <*> braced (many statement)

outS :: Parser AStat
outS = annotate AOut (<$> (string "outb" *> spaces *> parend relExpr <* spaces <* char ';'))

-- external declarations

extDecl :: Parser AExtDecl
extDecl = try funcDefn
        <|> try funcDecl

funcDefn :: Parser AExtDecl
funcDefn = do
        pos <- getPosition
        AFuncDefn pos <$> cType <*> identifier <*> parend (varDecl `sepBy` (char ',' <* spaces)) <* spaces <*> braced (many statement)

funcDecl :: Parser AExtDecl
funcDecl = do
        pos <- getPosition
        AFuncDecl pos <$> cType <*> identifier <*> parend (varDecl `sepBy` (char ',' <* spaces)) <* spaces <* char ';'

-- program

annAST :: Parser AnnAST
annAST = AnnAST <$> many1 (spaces *> try (many (spaces *> sourcePosDirective *> spaces) *> extDecl) <* spaces)

-- directives

sourcePosDirective :: Parser ()
sourcePosDirective = do
        srcName <- string "!source_pos " *> dquoted (many (noneOf "\""))
        line <- read <$> many1 digit
        col <- spaces *> (read <$> many1 digit) <* string "\n"
        setPosition $ newPos srcName line col
