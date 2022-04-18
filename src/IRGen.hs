module IRGen where
import ASTGen

type IRVarName = String
type IRFrame = Int

data IRVal = IRConstVal Int | IRVarVal IRVar
data IRVar = IRVar IRVarName IRFrame
data IRExtDecl = IRFuncDecl

data IRStmt = IRAssignStmt IRVar IRVal IRStmt
            | IRReturnStmt IRVal