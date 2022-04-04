module Parser where
import Lexer

type Identifier = String
data Op = Add | Sub | Mult | Div deriving Show

-- AST Structure --
data Program = Prog FunctionDecl deriving Show
data FunctionDecl = FuncDecl Identifier [Statement] deriving Show
data Statement = NoOp | Return Expression | Assign Variable Expression deriving Show
data Expression = ConstExpr Integer | VarExpr Variable | BinExpr Expression Op Expression deriving Show
data Variable = Var Identifier deriving Show

parseFunctionDecl :: [Token] -> Maybe ([Token], FunctionDecl)
parseFunctionDecl [] = Nothing
parseFunctionDecl (Identifier fType:Identifier fName:SpecSymb '(':SpecSymb ')':SpecSymb '{':xs) =
    case parseStatementList xs [] of
        (toParse, statList) -> if head toParse == SpecSymb '}' then Just (tail toParse, FuncDecl fName statList) else Nothing
parseFunctionDecl x = Nothing 

parseStatementList :: [Token] -> [Statement] -> ([Token], [Statement])
parseStatementList [] x = ([], x)
parseStatementList toks accum = case parseStatement toks of
    Nothing -> (toks, accum)
    Just (toParse, stat) -> parseStatementList toParse (accum ++ [stat])

parseStatement :: [Token] -> Maybe ([Token], Statement)
parseStatement [] = Nothing
parseStatement (SpecSymb ';':xs) = Just (xs, NoOp)
parseStatement (Identifier "return":xs) =
    case parseExpression xs of
        Nothing -> Nothing
        Just (toParse, expr) -> if head toParse == SpecSymb ';' then Just (tail toParse, Return expr) else Nothing
parseStatement x = Nothing

parseExpression :: [Token] -> Maybe ([Token], Expression)
parseExpression [] = Nothing
parseExpression (Int x:xs) = Just (xs, ConstExpr x)
parseExpression x = Nothing

parser :: [Token] -> Maybe Program
parser [] = error "couldn't parse"
parser toks = case parseFunctionDecl toks of
    Just (_, funcDecl) -> Just (Prog funcDecl)
    Nothing -> Nothing