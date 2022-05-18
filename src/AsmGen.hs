module AsmGen where
import ASTGen
import SymbTab
import GHC.Char
import Data.Char (digitToInt)

-- === Register Conventions === --
-- r0 - zero register
-- r1 - return address/return value
-- r2 - stack pointer
-- r3 - binop left
-- r4 - binop right/result
-- r5,r6,r7 - temps

data AsmReg = R0 | R1 | R2 | R3 | R4 | R6 | R7 deriving Enum
data AsmProg = AsmProg [String] STable Integer


visitProgram :: Program -> AsmProg
visitProgram _ = AsmProg [] SEmpty 0

visitFuncDecl :: AsmProg -> FuncDecl -> AsmProg
visitFuncDecl prog@(AsmProg asm table label) (FuncDecl _ ident params body) = AsmProg [] SEmpty 0

visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = prog

visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr prog@(AsmProg asm table label) (IntE x) = AsmProg (asm ++ ["r1 := " ++ show x]) table label
visitExpr prog@(AsmProg asm table label) (CharE x) = AsmProg (asm ++ ["r1 := " ++ show (digitToInt x)]) table label
