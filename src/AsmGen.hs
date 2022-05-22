{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AsmGen where
import ASTGen
import StackSim
import GHC.Char
import Data.Char (digitToInt, ord)
import Text.Parsec

-- Function Labels - F_(name)
-- Other Labels - L(number)

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
                                         ".section text"]
                                            (stkSimPushFrame stkSimEmpty) 0) extdecls

visitExtDeclList :: AsmProg -> [ExtDecl] -> AsmProg
visitExtDeclList prog [] = prog
visitExtDeclList prog (x:xs) = case visitExtDecl prog x of
    result -> visitExtDeclList result xs

visitExtDecl :: AsmProg -> ExtDecl -> AsmProg
visitExtDecl prog@(AsmProg asm sim label) (FuncDecl _ ident params body) = case stkSimPush (_sspa (stkSimPushFrame sim) params) (Var ".ret", CInt) of
    result -> case visitStatList (AsmProg (asm ++ ["F_" ++ ident ++ ":"] ++ ["push r1 on stack r2"]) result label) body of
        AsmProg asm1 sim1@(StackSim stk@((StkFrame (_:fs) _):_)) label1 -> AsmProg (asm1 ++ replicate (length fs - length params) "pop stack r2"
                                                                                        ++ ["pop r4 off stack r2"]
                                                                                        ++ replicate (length params) "pop stack r2"
                                                                                        ++ ["goto r4"] ) (stkSimPopFrame sim1) label1
visitExtDecl (AsmProg asm sim label) (GlobDecl typ var) = AsmProg asm (stkSimPush sim (var, typ)) label

-- push args onto stack sim --
_sspa :: StackSim -> [Param] -> StackSim
_sspa sim ((Param typ v):ps) = stkSimPush (_sspa sim ps) (v, typ)
_sspa sim [] = sim

visitStatList :: AsmProg -> [Stat] -> AsmProg
visitStatList prog [] = prog
visitStatList prog (x:xs) = case visitStat prog x of
    result -> visitStatList result xs

visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = case visitExpr prog exp of
    (AsmProg asm table label) -> AsmProg (asm ++ ["pop r1 off stack r2"]) table label
visitStat (AsmProg asm sim label) (VarDeclS typ var) = AsmProg (asm ++ ["push r0 on stack r2"]) (stkSimPush sim (var, typ)) label

visitFuncCall :: AsmProg -> FuncCall -> AsmProg
visitFuncCall prog (FuncCall ident args) = case _passargs prog args of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["goto F_" ++ ident ++ " linking r1"]) sim label

_passargs :: AsmProg -> [Expr] -> AsmProg
_passargs prog x = _pa prog (reverse x) where
    _pa prog (x:xs) = case _vexpr prog x of
        prog1 -> _pa prog1 xs
    _pa prog [] = prog


visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr (AsmProg asm sim label) exp = case _vexpr (AsmProg asm (stkSimPushFrame sim) label) exp of
    (AsmProg asm1 sim1@(StackSim ((StkFrame (_:fs) _):_)) label1) -> AsmProg asm1 (stkSimPopFrame sim1) label1

_vexpr :: AsmProg -> Expr -> AsmProg
-- stack-push operations --
_vexpr (AsmProg asm sim label) (IntE x) = AsmProg (asm ++ ["push " ++ show x ++" on stack r2"]) (stkSimPush sim (Var ".val", CInt)) label
_vexpr (AsmProg asm sim label) (CharE x) = AsmProg (asm ++ ["push " ++ show (ord x) ++ " on stack r2"]) (stkSimPush sim (Var ".val", CChar)) label
_vexpr (AsmProg asm sim label) (VarE var) = AsmProg (asm ++ ["r4 := m[r0][" ++ "r2 + " ++ show (stkSimQuery sim var) ++ "]"]
                                                    ++ ["push r4 on stack r2"]) (stkSimPush sim (Var ".val", CInt)) label
_vexpr prog (FunE call) = case visitFuncCall prog call of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["push r1 on stack r2"]) (stkSimPush sim (Var ".func", CInt)) label
-- unary operations --
_vexpr prog (Neg x) = case _vexpr prog x of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["r4 = -r4"]
                                            ++ ["push r4 on stack r2"]) sim label
_vexpr prog (Lno x) = case _vexpr prog x of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["r4 = ~r4"]
                                            ++ ["push r4 on stack r2"]) sim label
_vexpr prog (No x) = case _vexpr prog x of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["if (r4 == 0) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) sim (label + 2)
-- relational operations --
_vexpr prog (Le x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <=s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Lt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Ge x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >=s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Gt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Eq x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 == r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Ne x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 != r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (Or x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 | r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
_vexpr prog (And x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 & r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) (stkSimPop sim) (label + 2)
-- binary operations --
_vexpr prog (Sub x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 - r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Add x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 + r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Div x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 / r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Mul x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 * r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Bor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 | r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Xor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 xor r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Mod x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 mod r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label
_vexpr prog (Band x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 & r4"]
                                            ++ ["push r4 on stack r2"]) (stkSimPop sim) label