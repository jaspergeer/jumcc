module Lexer where
data Token = Keyword     String
           | Identifier  String
           | Constant    Integer
           | String      String
           | SpecSymb    Char
           | Operator    Char
           deriving (Show, Eq)

c_SPECSYMB = "(){}[];,"
c_IDENT_START = '_':['a'..'z'] ++ ['A'..'Z']
c_IDENT_CHAR = c_IDENT_START ++ ['0'..'9']
c_KEYWORDS = ["if","while","return","else"]
c_TYPES = ["int", "char"]
c_OPERATOR_START = "=><*/&!+-" -- not supported - "^%~|.?:"
c_OPERATORS = ["=","==",">","<","<=",">=","!=",
               "*","/","+","-","!", "&"]


lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` c_SPECSYMB       = SpecSymb x:lexer xs
    | x == '"'                  = lexer xs -- parse string
    | x `elem` ['1'..'9']       = lexer xs -- parse num
    | x `elem` c_OPERATOR_START = lexer xs -- parse operator
    | x `elem` c_IDENT_START    = lexer xs -- parse identifier
    | otherwise                 = lexer xs