module AsmGen where
import ASTGen
import StackSim
import GHC.Char
import Data.Char (digitToInt)
import Text.Parsec

-- === Register Conventions === --
-- r0 - zero register
-- r1 - return address/return value
-- r2 - stack pointer
-- r3 - binop left
-- r4 - binop right/result
-- r5,r6,r7 - temps

data AsmProg = AsmProg [String] StackSim Int deriving Show

visitProgram :: Program -> Either ParseError AsmProg
visitProgram (Program extdecls) = return $ visitExtDeclList (AsmProg 
                                        [".section init",
                                         ".zero r0",
                                         ".temps r5,r6,r7",
                                         "r0 := 0",
                                         "r2 := callstack",
                                         "goto _start linking r1",
                                         "halt",
                                         ".section data",
                                         ".space 100000",
                                         "callstack:",
                                         ".section text",
                                         "_start:",
                                         "push r1 on stack r2",
                                         "goto main linking r1",
                                         "ouput r1",
                                         "pop r4 off stack r2",
                                         "goto r4"]
                                            (stkSimPushFrame stkSimEmpty) 0) extdecls

visitExtDeclList :: AsmProg -> [ExtDecl] -> AsmProg
visitExtDeclList prog [] = prog
visitExtDeclList prog (x:xs) = case visitExtDecl prog x of
    result -> visitExtDeclList result xs

visitExtDecl :: AsmProg -> ExtDecl -> AsmProg
visitExtDecl (AsmProg asm sim label) (FuncDecl _ ident params body) = case _ved (stkSimPush (stkSimPushFrame sim) (Var ".ret", CInt)) params of
    result -> case visitStatList (AsmProg (asm ++ [ident ++ ":"] ++ ["push r1 on stack r2"] ++ ["push r0 on stack r2" | c <- params ]) result label) body of
        AsmProg asm1 sim1@(StackSim stk@((StkFrame (_:fs) _):_)) label1 -> AsmProg (asm1 ++ ["pop stack r2" | c <- fs] ++ ["goto r4"] ++ ["pop r4 off stack r2"]) (stkSimPopFrame sim1) label1
--visitExtDecl (AsmProg asm sim label) (GlobDecl typ var) = AsmProg asm (stkSimPush sim (var, typ)) label

_ved :: StackSim -> [Param] -> StackSim
_ved sim ((Param typ v):ps) = stkSimPush (_ved sim ps) (v, typ)
_ved sim [] = sim

visitStatList :: AsmProg -> [Stat] -> AsmProg
visitStatList prog [] = prog
visitStatList prog (x:xs) = case visitStat prog x of
    result -> visitStatList result xs

visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = case visitExpr prog exp of
    (AsmProg asm table label) -> AsmProg (asm ++ ["pop r1 off stack r2"]) table label
visitStat (AsmProg asm sim label) (VarDeclS typ var) = AsmProg (asm ++ ["push r0 on stack r2"]) (stkSimPush sim (var, typ)) label

visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr (AsmProg asm sim label) exp = case _vexpr (AsmProg asm (stkSimPushFrame sim) label) exp of
    (AsmProg asm1 sim1@(StackSim ((StkFrame (_:fs) _):_)) label1) -> AsmProg (asm1 ++ ["pop r4 off stack r2"]
                                                                ++ ["pop stack r2" | c <- fs]
                                                                ++ ["push r4 on stack r2"]) (stkSimPopFrame sim1) label1

_vexpr :: AsmProg -> Expr -> AsmProg
_vexpr (AsmProg asm sim label) (IntE x) = AsmProg (asm ++ ["r4 := " ++ show x] ++ ["push r4 on stack r2"]) (stkSimPush sim (Var ".val", CInt)) label
_vexpr (AsmProg asm sim label) (CharE x) = AsmProg (asm ++ ["r4 := " ++ show (digitToInt x)] ++ ["push r4 on stack r2"]) (stkSimPush sim (Var ".val", CChar)) label
_vexpr (AsmProg asm sim label) (VarE var) = AsmProg (asm ++ ["r4 := m[r0][" ++ "r2 + " ++ show (stkSimQuery sim var) ++ "]"]
                                                                ++ ["push r4 on stack r2"]) (stkSimPush sim (Var ".val", CInt)) label
