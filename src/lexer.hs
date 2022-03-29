module Lexer where
data Token = Keyword     String
           | Identifier  String
           | Constant    Integer
           | String      String
           | SpecSymb    Char
           | Operator    Char
           deriving (Show, Eq)

cSpecSymbs = "(){}[];,"
cIdentBegin = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar = cIdentBegin ++ ['0'..'9']
cKeywords = ["if","while","return","else"]
cTypes = ["int", "char"]
cOperatorBegin = "=><*/&!+-" -- not supported - "^%~|.?:"
cOperators = ["=","==",">","<","<=",">=","!=",
               "*","/","+","-","!", "&"]


lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` cSpecSymbs       = SpecSymb x:lexer xs
    | x == '"'                  = lexer xs -- parse string
    | x `elem` ['1'..'9']       = lexer xs -- parse num
    | x `elem` cOperatorBegin   = lexer xs -- parse operator
    | x `elem` cIdentBegin      = lexer xs -- parse identifier
    | otherwise                 = lexer xs