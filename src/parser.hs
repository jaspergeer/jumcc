module Parser where
import Lexer (Token (Identifier), lexer)

type Identifier = String

-- AST Structure --
data Program = Program FunctionDecl
data FunctionDecl = FunctionDecl Identifier Statement
data Statement = Return Expression
data Expression = LiteralInt Int

parseFunctionDecl :: [Token] -> Maybe FunctionDecl
parseFunctionDecl [] = Nothing

parser :: [Token] -> Program
parser [] = error "couldn't parse"