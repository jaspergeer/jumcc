module Lexer where
data Token = Keyword    String
           | Identifier String
           | Int        Integer
           | Char       Char
           | String     String
           | SpecSymb   Char
           | Operator   String
           deriving (Show, Eq)

cSpecSymbs      = "(){}[];,"
cIdentBegin     = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar      = cIdentBegin ++ ['0'..'9']
cKeywords       = ["if","while","return","else"]
cTypes          = ["int", "char"]
cOperatorBegin  = "=><*/&!+-" -- not supported - "^%~|.?:"
cOperators      = ["=","==",">","<","<=",">=","!=","*","/","+","-","!", "&"]

-- first parameter is accumulator
lexParseString :: String -> String -> [Token]
lexParseString t [] = error "couldn't parse string literal"
lexParseString tokStr (x:xs)
    | tokStr `elem` cKeywords   = Keyword tokStr:lexer (x:xs)
    | tokStr `elem` cTypes      = Keyword tokStr:lexer (x:xs)
    | x == '\\'                 = lexParseString (tokStr ++ [toEscapeSeq [x,head xs]]) (tail xs)
    | x == '\"'                 = String tokStr:lexer xs
    | otherwise                 = lexParseString (tokStr ++ [x]) xs

lexParseNumConst :: String -> String -> [Token]
lexParseNumConst t [] = error "couldn't parse numeric literal"
lexParseNumConst tokStr (x:xs)
    | x `elem` ['0'..'9']   = lexParseNumConst (tokStr ++ [x]) xs
    | otherwise             = Int (read tokStr):lexer (x:xs)

lexParseOperator :: Char -> String -> [Token]
lexParseOperator t [] = error "couldn't parse operator"
lexParseOperator tokChar (x:xs)
    | [tokChar,x] `elem` cOperators = Operator [tokChar,x]:lexer xs
    | otherwise                     = Operator [tokChar]:lexer (x:xs)

lexParseIdentifier :: String -> String -> [Token]
lexParseIdentifier t [] = error "couldn't parse identifier"
lexParseIdentifier tokStr (x:xs)
    | x `elem` cIdentChar   = lexParseIdentifier (tokStr ++ [x]) xs
    | otherwise             = Identifier tokStr:lexer (x:xs) 

lexParseCharConst :: String -> [Token]
lexParseCharConst [] = error "couldn't parse char literal"
lexParseCharConst [t] = error "couldn't parse char literal"
lexParseCharConst (x:y:ys)
    | ys /= [] && head ys == '\''   = Char (toEscapeSeq [x,y]):lexer (tail ys)
    | y == '\''                     = Char x:lexer ys
    | otherwise                     = error "couldn't parse char literal"

toEscapeSeq :: String -> Char
toEscapeSeq escStr
    | escStr == "\\a"   = '\a'
    | escStr == "\\b"   = '\b'
    | escStr == "\\e"   = '\ESC'
    | escStr == "\\f"   = '\f'
    | escStr == "\\n"   = '\n'
    | escStr == "\\r"   = '\r'
    | escStr == "\\t"   = '\t'
    | escStr == "\\v"   = '\v'
    | escStr == "\\\\"  = '\\'
    | escStr == "\\\'"  = '\''
    | escStr == "\\\""  = '\"'
    | escStr == "\\?"   = '?'
    | otherwise         = error "couldn't parse escape sequence"

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x `elem` cSpecSymbs       = SpecSymb x:lexer xs
    | x == '\"'                 = lexParseString "" xs
    | x `elem` ['1'..'9']       = lexParseNumConst [x] xs
    | x == '\''                 = lexParseCharConst xs
    | x `elem` cOperatorBegin   = lexParseOperator x xs
    | x `elem` cIdentBegin      = lexParseIdentifier [x] xs
    | otherwise                 = lexer xs