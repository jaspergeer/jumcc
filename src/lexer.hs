module Lexer where

data Token = Keyword    String
           | Identifier String
           | Constant   Integer
           | String     String
           | SSymbol    Char
           | Operator   Char

lexer :: String -> [Token]
lexer [] = []
