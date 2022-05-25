{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AsmGen where
import ASTGen
import StackSim
import GHC.Char
import Data.Char (ord)
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
                                            (stkSimPushFrame stkSimEmpty ".global") 0) extdecls

visitExtDeclList :: AsmProg -> [ExtDecl] -> AsmProg
visitExtDeclList prog [] = prog
visitExtDeclList prog (x:xs) = case visitExtDecl prog x of
    result -> visitExtDeclList result xs

visitExtDecl :: AsmProg -> ExtDecl -> AsmProg
visitExtDecl prog@(AsmProg asm sim label) (FuncDecl _ ident params body) = case stkSimPush (_sspa (stkSimPushFrame sim ident) params) (Var ".ret",U32) of
    result -> case visitStatList (AsmProg (asm ++ ["F_" ++ ident ++ ":"] ++ ["push r1 on stack r2"]) result label) body of
        AsmProg asm1 sim1@(StackSim stk@((StkFrame _ (_:fs) size):_)) label1 -> AsmProg (asm1
                                                                                        ++ ["F_END_" ++ ident ++ ":"]
                                                                                        ++ replicate (size - (length params + 1)) "pop stack r2"
                                                                                        ++ ["pop r4 off stack r2"]
                                                                                        ++ replicate (length params) "pop stack r2"
                                                                                        ++ ["goto r4"] ) (stkSimPopFrame sim1) label1
visitExtDecl (AsmProg asm sim label) (GlobDecl typ var) = AsmProg asm (stkSimPush sim (var,typ)) label

-- push args onto stack sim --
_sspa :: StackSim -> [VarDecl] -> StackSim
_sspa sim ((VarDecl t v):ps) = stkSimPush (_sspa sim ps) (v,t)
_sspa sim [] = sim

visitStatList :: AsmProg -> [Stat] -> AsmProg
visitStatList prog [] = prog
visitStatList prog (x:xs) = case visitStat prog x of
    result -> visitStatList result xs

visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = case visitExpr prog exp of
    (AsmProg asm sim@(StackSim ((StkFrame sfid _ _):xs)) label) -> AsmProg (asm ++ ["pop r1 off stack r2"]
                                                                                ++ ["goto F_END_" ++ sfid]) (stkSimPop sim) label
visitStat prog (VarDeclS (VarDecl typ var) init) = case visitExpr prog init of
    (AsmProg asm sim label) -> AsmProg asm (stkSimPush (stkSimPop sim) (var, typ)) label
visitStat prog (ArrDeclS (VarDecl (Arr size typ) var) []) = case pushExprSeq prog (replicate size (IntE 0)) of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var,Ptr typ)) label
visitStat prog (ArrDeclS (VarDecl typ var) init) = case pushExprSeq prog init of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var,Ptr typ)) label
visitStat prog (IfS cond body) = case visitExpr prog cond of
    (AsmProg asm sim@(StackSim ((StkFrame sfid _ _):_)) label) -> case visitStatList (AsmProg (asm
                                                                                                ++ ["pop r4 off stack r2"]
                                                                                                ++ ["if (r4 == 0) goto L" ++ show label])
                                                                                        (stkSimPushFrame (stkSimPop sim) sfid) (label + 1)) body of
        AsmProg asm1 sim1@(StackSim ((StkFrame _ _ size):_)) label1 -> AsmProg (asm1
                                                                                    ++ replicate size "pop stack r2"
                                                                                    ++ ["L" ++ show label ++ ":"]) (stkSimPopFrame sim1) label1
visitStat prog (Out exp) = case visitExpr prog exp of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["output r4"]) (stkSimPop sim) label
visitStat prog (AssignS (VarE var) exp) = case visitExpr prog exp of
    (AsmProg asm sim label) -> case stkSimPop sim of
        sim1 -> AsmProg (asm
                            ++ ["pop r4 off stack r2"]
                            ++ ["m[r0][r2 + " ++ show (stkSimQuery sim1 var) ++ "] := r4"]) sim1 label
visitStat prog (AssignS (Dref addr) exp) = case visitExpr prog addr of
    prog1 -> case visitExpr prog1 exp of
        (AsmProg asm sim label) -> AsmProg (asm 
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["m[r0][r3] := r4"]) (stkSimPop (stkSimPop sim)) label
visitStat prog (FuncCallS call) = case visitFuncCall prog call of
    (AsmProg asm sim label) -> AsmProg asm (stkSimPop sim) label;
visitStat prog@(AsmProg asm sim label) (WhileS cond body) = case visitExpr (AsmProg (asm ++ ["L" ++ show label ++ ":"]) sim (label + 1)) cond of
    (AsmProg asm1 sim1@(StackSim ((StkFrame sfid _ _):_)) label1) -> case visitStatList (AsmProg (asm1
                                                                                                ++ ["pop r4 off stack r2"]
                                                                                                ++ ["if (r4 == 0) goto L" ++ show label1])
                                                                                        (stkSimPushFrame (stkSimPop sim1) sfid) (label1 + 1)) body of
        AsmProg asm2 sim2@(StackSim ((StkFrame _ _ size):_)) label2 -> AsmProg (asm2
                                                                                    ++ replicate size "pop stack r2"
                                                                                    ++ ["goto L" ++ show label]
                                                                                    ++ ["L" ++ show label1 ++ ":"]) (stkSimPopFrame sim2) label2

-- getAddress :: AsmProg -> Expr -> AsmProg
-- getAddress prog (Dref exp) = case visitExpr prog exp of
--     (AsmProg asm sim label) -> AsmProg asm (stkSimPush (stkSimPop sim) (Var ".addr", U32)) label
-- getAddress prog (VarE var exp) = case visitExpr prog exp of
--     (AsmProg asm sim label) -> case stkSimQuery sim var of
--         offset -> AsmProg (asm
--                             ++ ["r4 := m[r0][r2 + " ++ show offset ++ "]"]
--                             ++ ["pop r3 off stack r2"]
--                             ++ ["r4 := r4 + r3"]
--                             ++ ["push r4 on stack r2"]) (stkSimPush (stkSimPop sim) (Var ".addr", U32)) label

visitFuncCall :: AsmProg -> FuncCall -> AsmProg
visitFuncCall prog (FuncCall ident args) = case pushExprSeq prog args of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["goto F_" ++ ident ++ " linking r1"]) sim label

pushExprSeq :: AsmProg -> [Expr] -> AsmProg
pushExprSeq prog x = _pes prog (reverse x) where
    _pes prog (x:xs) = case visitExpr prog x of
        prog1 -> _pes prog1 xs
    _pes prog [] = prog

visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr (AsmProg asm sim label) exp = case _vexpr (AsmProg asm (stkSimPushFrame sim ".expr") label) exp of
    (AsmProg asm1 sim1@(StackSim ((StkFrame _ (_:fs) _):_)) label1) -> AsmProg asm1 (stkSimPush (stkSimPopFrame sim1) (Var ".result", U32)) label1

_vexpr :: AsmProg -> Expr -> AsmProg
-- stack-push operations --
_vexpr (AsmProg asm sim label) (IntE x) = AsmProg (asm ++ ["push " ++ show x ++" on stack r2"]) (stkSimPush sim (Var ".int", U32)) label
_vexpr (AsmProg asm sim label) (CharE x) = AsmProg (asm ++ ["push " ++ show (ord x) ++ " on stack r2"]) (stkSimPush sim (Var ".char", U8)) label
_vexpr (AsmProg asm sim label) (VarE var) = case  AsmProg (asm
                                                    ++ ["r4 := m[r0][" ++ "r2 + " ++ show (stkSimQuery sim var) ++ "]"])
                                                    (stkSimPush sim (Var ".var", stkSimQueryType sim var)) label of
                                                        prog -> enforceTypeSize prog
_vexpr (AsmProg asm sim label) In = AsmProg (asm
                                                ++ ["r4 := input()"]
                                                ++ ["push r4 on stack r2"]) (stkSimPush sim (Var ".in", U8)) label
_vexpr prog (FunE call) = case visitFuncCall prog call of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["push r1 on stack r2"]) (stkSimPush sim (Var ".func", U32)) label -- TODO symbol table?
-- unary operations --
_vexpr prog (Neg x) = case _vexpr prog x of
    (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["r4 := -r4"]) sim label of
                                                prog -> enforceTypeSize prog
_vexpr prog (Lno x) = case _vexpr prog x of
    (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["r4 := ~r4"]) sim label of
                                                prog -> enforceTypeSize prog
_vexpr prog (No x) = case _vexpr prog x of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r4 off stack r2"]
                                            ++ ["if (r4 == 0) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"]) sim (label + 2)
_vexpr prog (Dref addr) = case _vexpr prog addr of
    (AsmProg asm sim label) -> case AsmProg (asm
                                        ++ ["pop r4 off stack r2"]
                                        ++ ["r4 := m[r0][r4]"])
                                        (stkSimPush (stkSimPop sim) (Var ".val", dref (stkSimPeekType sim))) label of
                                            prog1 -> enforceTypeSize prog1

-- relational operations --
_vexpr prog (Le x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <=s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".le", U8)) (label + 2)
_vexpr prog (Lt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".lt", U8)) (label + 2)
_vexpr prog (Ge x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >=s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".ge", U8)) (label + 2)
_vexpr prog (Gt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >s r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".gt", U8)) (label + 2)
_vexpr prog (Eq x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 == r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".eq", U8)) (label + 2)
_vexpr prog (Ne x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 != r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".ne", U8)) (label + 2)
_vexpr prog (Or x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 | r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".or", U8)) (label + 2)
_vexpr prog (And x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 & r4) goto L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto L" ++ show (label + 1)]
                                            ++ ["L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ ["L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (Var ".and", U8)) (label + 2)
-- binary operations --
_vexpr prog (Sub x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 - r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Add x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 + r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Div x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 / r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Mul x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 * r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Bor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 | r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Xor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 xor r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Mod x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 mod r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)
_vexpr prog (Band x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg asm sim label) -> case AsmProg (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 & r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (Var ".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg asm1 sim2 label)

-- assumes r4 contains the value we are enforcing type size for --
enforceTypeSize :: AsmProg -> AsmProg
enforceTypeSize prog@(AsmProg asm sim label) = case stkSimPeekType sim of
    U8 -> AsmProg (asm ++ ["r4 := r4 mod 256"]
                        ++ ["push r4 on stack r2"]) sim label
    _ -> AsmProg (asm ++ ["push r4 on stack r2"]) sim label

evalTypes :: CType -> CType -> CType
evalTypes U8 U8 = U8
evalTypes p@(Ptr _) _ = p
evalTypes _ p@(Ptr _) = p
evalTypes (Arr _ x) _ = Ptr x
evalTypes _ (Arr _ x) = Ptr x
evalTypes _ _ = U32

dref :: CType -> CType
dref (Ptr x) = x
dref (Arr _ x) = x
dref x = error ("type " ++ show x ++ " cannot be dereferenced")