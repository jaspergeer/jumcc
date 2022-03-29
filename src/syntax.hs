module Syntax where

type Identifier = String

data Expression = Exp Int deriving (Show)
data Statement = Stat Expression
data CompoundStatement = CompStat [Statement]
data FunctionDefinition = SimpleFuncDef String CompoundStatement
data Program = Prog [FunctionDefinition]