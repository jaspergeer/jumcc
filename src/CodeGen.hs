{-
 - CodeGen.hs
 - Author: Jasper Geer, jasper.geer@gmail.com
 - Copyright (c) 2022 Jasper Geer
 - Licensed under the MIT license
 -}

module CodeGen where
import AST
    ( Expr(..),
      Stat(..),
      VarDecl(..),
      FuncCall(..),
      ExtDecl(FuncDefn, FuncDecl),
      AST(..) )
import CType ( CType(..), dref )
import StackSim
    ( StackSim(..),
      StkFrame(StkFrame),
      stkSimEmpty,
      stkSimQuery,
      stkSimQueryType,
      stkSimPeekType,
      stkSimPushFrame,
      stkSimPopFrame,
      stkSimPush,
      stkSimPop )
import Data.Char (ord)
import Text.Parsec ( ParseError )

-- Label Convention --
-- Function Labels - F_(identifier), F_END_(identifier)
-- Loop Labels - WHILE_(integer), WHILE_END_(integer)
-- Other Labels - L_(integer)

-- Register Convention --
-- r0 - zero register
-- r1 - return address/return value
-- r2 - stack pointer
-- r3,r4 - volatiles
-- r5,r6,r7 - temps

data AsmProg = AsmProg String [String] StackSim Int deriving Show

-- generate asm for a AST
visitAST :: AST -> String -> AsmProg
visitAST (AST extdecls) pname = case visitExtDeclList (AsmProg pname
                                        [".section init",
                                         ".zero r0",
                                         ".temps r5,r6,r7",
                                         "r0 := 0",
                                         "r2 := callstack",
                                         ".section text"]
                                            (stkSimPushFrame stkSimEmpty ".global") 0) extdecls of
                                                (AsmProg pname asm sim label) -> AsmProg pname (".section rodata" : asm) sim label

visitExtDeclList :: AsmProg -> [ExtDecl] -> AsmProg
visitExtDeclList prog [] = prog
visitExtDeclList prog (x:xs) = case visitExtDecl prog x of
    result -> visitExtDeclList result xs

-- generate asm for an external declaration
visitExtDecl :: AsmProg -> ExtDecl -> AsmProg
visitExtDecl prog@(AsmProg pname asm sim label) (FuncDefn _ ident params body) = case stkSimPush (pushArgs (stkSimPushFrame sim (".func_" ++ ident)) params) (".ret",Int) of
    result -> case visitStatList (AsmProg pname (asm
                                                    ++ ["F_" ++ ident ++ ":"]
                                                    ++ ["push r1 on stack r2"]) result label) body of
                                                    (AsmProg pname asm1 sim1 label1) -> AsmProg pname asm1 (stkSimPopFrame sim1) label1
visitExtDecl prog (FuncDecl {}) = prog

-- push sequence of arguments onto stack simulation
pushArgs :: StackSim -> [VarDecl] -> StackSim
pushArgs sim ((VarDecl t v):ps) = stkSimPush (pushArgs sim ps) (v,t)
pushArgs sim [] = sim

-- generate asm for a list of statements
visitStatList :: AsmProg -> [Stat] -> AsmProg
visitStatList prog [] = prog
visitStatList prog (x:xs) = case visitStat prog x of
    result -> visitStatList result xs

-- gets the size of the current function stack frame
getFuncSize :: StackSim -> Int
getFuncSize (StackSim ((StkFrame ('.':'f':'u':'n':'c':_) _ size):xs)) = size;
getFuncSize (StackSim ((StkFrame _ _ size):xs)) = size + getFuncSize (StackSim xs);
getFuncSize _ = error "fatal error: function stack frame not found"

-- generate asm for a statement
visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = case visitExpr prog exp of
    (AsmProg pname asm sim label) -> case stkSimPop sim of
        sim1 -> case stkSimQuery sim1 ".ret" of
            retPos-> AsmProg pname (asm ++ ["pop r1 off stack r2"]
                                        ++ ["r2 := r2 + " ++ show retPos]
                                        ++ ["pop r4 off stack r2"]
                                        ++ ["r2 := r2 + " ++ show (getFuncSize sim1 - (retPos + 1))]
                                        ++ ["goto r4"]) sim1 label
visitStat prog (VarDeclS (VarDecl typ var) init) = case visitExpr prog init of
    (AsmProg pname asm sim label) -> AsmProg pname asm (stkSimPush (stkSimPop sim) (var, typ)) label
visitStat (AsmProg pname asm sim label) (ArrDeclS (VarDecl (Arr size typ) var) []) = AsmProg pname (asm ++ ["r2 := r2 - " ++ show size] ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var, Arr size typ)) label
visitStat prog (ArrDeclS (VarDecl (Arr size typ) var) init) = case pushExprSeq prog init of
    (AsmProg pname asm sim label) -> AsmProg pname (asm ++ ["r2 := r2 - " ++ show (length init - size)] ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var, Arr (length init - size) typ)) label
visitStat prog (IfS cond body) = case visitExpr prog cond of
    (AsmProg pname asm sim label) -> case visitStatList (AsmProg pname (asm ++ ["pop r4 off stack r2"]
                                                                            ++ ["if (r4 == 0) goto " ++ pname ++ "L" ++ show label])
                                                                        (stkSimPushFrame (stkSimPop sim) ".if") (label + 1)) body of
        AsmProg pname asm1 sim1@(StackSim ((StkFrame _ _ size):_)) label1 -> AsmProg pname (asm1
                                                                                 ++ ["r2 := r2 + " ++ show size]
                                                                                 ++ [pname ++ "L" ++ show label ++ ":"]) (stkSimPopFrame sim1) label1
        _ -> error "fatal error: stack sim empty"
visitStat prog (Out exp) = case visitExpr prog exp of
    (AsmProg pname asm sim label) -> AsmProg pname (asm ++ ["pop r4 off stack r2"]
                                            ++ ["output r4"]) (stkSimPop sim) label
visitStat prog (AssignS (VarE var) exp) = case visitExpr prog exp of
    (AsmProg pname asm sim label) -> case stkSimPop sim of
        sim1 -> AsmProg pname (asm
                            ++ ["pop r4 off stack r2"]
                            ++ ["m[r0][r2 + " ++ show (stkSimQuery sim1 var) ++ "] := r4"]) sim1 label
visitStat prog (AssignS (Dref addr) exp) = case visitExpr prog addr of
    prog1 -> case visitExpr prog1 exp of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["m[r0][r3] := r4"]) (stkSimPop (stkSimPop sim)) label
visitStat prog (FuncCallS call) = visitFuncCall prog call
visitStat prog@(AsmProg pname asm sim label) (WhileS cond body) = case visitExpr (AsmProg pname (asm ++ [pname ++ "WHILE_" ++ show label ++ ":"]) sim (label + 1)) cond of
    (AsmProg pname asm1 sim1@(StackSim ((StkFrame sfid _ _):_)) label1) -> case visitStatList (AsmProg pname (asm1
                                                                                                ++ ["pop r4 off stack r2"]
                                                                                                ++ ["if (r4 == 0) goto " ++ pname ++ "WHILE_END_" ++ show label])
                                                                                        (stkSimPushFrame (stkSimPop sim1) (".while_" ++ show label)) label1) body of
        AsmProg pname asm2 sim2@(StackSim ((StkFrame _ _ size):_)) label2 -> AsmProg pname (asm2
                                                                                    ++ ["r2 := r2 + " ++ show size]
                                                                                    ++ ["goto " ++ pname ++ "WHILE_" ++ show label]
                                                                                    ++ [pname ++ "WHILE_END_" ++ show label ++ ":"]) (stkSimPopFrame sim2) label2
        _ -> error "fatal error: stack sim empty"
    _ -> error "fatal error: stack sim empty"
visitStat (AsmProg pname asm sim@(StackSim stk@((StkFrame _ _ size):_)) label) Break = AsmProg pname (asm
                                                                                        ++ ["r2 := r2 + " ++ show (_sizewhile stk)]
                                                                                        ++ ["goto " ++ pname ++ "WHILE_END_" ++ getWhileName sim]) sim label where
                                                                                            _sizewhile ((StkFrame ('.':'w':'h':'i':'l':'e':'_':_) _ size):_) = size
                                                                                            _sizewhile ((StkFrame _ _ size):stk) = _sizewhile stk + size
                                                                                            _sizewhile _ = error "fatal error: stack sim empty"
visitStat _ _ = error "fatal error: unable to match statement"

-- geneate asm for a function call
visitFuncCall :: AsmProg -> FuncCall -> AsmProg
visitFuncCall (AsmProg pname asm sim label) (FuncCall ident args) = case pushExprSeq (AsmProg pname asm (stkSimPushFrame sim (".call_" ++ ident)) label) args of
    (AsmProg pname asm1 sim1 label1) -> AsmProg pname (asm1 ++ ["goto F_" ++ ident ++ " linking r1"]) (stkSimPopFrame sim1) label1

-- generate asm for a sequence of expressions, results are pushed onto the stack in reverse order
pushExprSeq :: AsmProg -> [Expr] -> AsmProg
pushExprSeq prog x = _pes prog (reverse x) where
    _pes prog (x:xs) = case visitExpr prog x of
        prog1 -> _pes prog1 xs
    _pes prog [] = prog

-- "visit expression" wrapper
visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr (AsmProg pname asm sim label) exp = case _vexpr (AsmProg pname asm (stkSimPushFrame sim ".expr") label) exp of
    (AsmProg pname asm1 sim1@(StackSim ((StkFrame _ (_:fs) _):_)) label1) -> AsmProg pname asm1 (stkSimPush (stkSimPopFrame sim1) (".result", Int)) label1
    _ -> error "fatal error: not a valid expression"

-- generate asm for an expression
-- pushes an additional value onto the stack
_vexpr :: AsmProg -> Expr -> AsmProg
-- stack-push operations
_vexpr (AsmProg pname asm sim label) (IntE x) = AsmProg pname (asm ++ ["push " ++ show x ++" on stack r2"]) (stkSimPush sim (".int", Int)) label
_vexpr (AsmProg pname asm sim label) (CharE x) = AsmProg pname (asm ++ ["push " ++ show (ord x) ++ " on stack r2"]) (stkSimPush sim (".char", Char)) label
_vexpr (AsmProg pname asm sim label) (VarE var) = case  AsmProg pname (asm
                                                    ++ ["r4 := m[r0][" ++ "r2 + " ++ show (stkSimQuery sim var) ++ "]"])
                                                    (stkSimPush sim (".var", stkSimQueryType sim var)) label of
                                                        prog -> enforceTypeSize prog
_vexpr (AsmProg pname asm sim label) In = AsmProg pname (asm
                                                ++ ["r4 := input()"]
                                                ++ ["push r4 on stack r2"]) (stkSimPush sim (".in", Char)) label
_vexpr prog (FunE call) = case visitFuncCall prog call of
    (AsmProg pname asm sim label) -> AsmProg pname (asm ++ ["push r1 on stack r2"]) (stkSimPush sim (".func", Int)) label
_vexpr (AsmProg pname asm sim label) (Str str) = AsmProg pname ([pname ++ "STR_" ++ show label ++ ":"]
                                                                ++ makeStrLit str
                                                                ++ asm
                                                                ++ ["push " ++ pname ++ "STR_" ++ show label ++ " on stack r2"])
                                                                (stkSimPush sim (".str", Ptr Char)) (label + 1)
-- unary operations --
_vexpr (AsmProg pname asm sim label) (RefE var) = AsmProg pname (asm ++ ["r4 := r2 + " ++ show (stkSimQuery sim var)]
                                                                        ++ ["push r4 on stack r2"]) (stkSimPush sim (".ref", Ptr (stkSimQueryType sim var))) label
_vexpr prog (Neg x) = case _vexpr prog x of
    (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["r4 := -r4"]) sim label of
                                                prog -> enforceTypeSize prog
_vexpr prog (Lno x) = case _vexpr prog x of
    (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["r4 := ~r4"]) sim label of
                                                prog -> enforceTypeSize prog
_vexpr prog (No x) = case _vexpr prog x of
    (AsmProg pname asm sim label) -> AsmProg pname (asm ++ ["pop r4 off stack r2"]
                                            ++ ["if (r4 == 0) goto " ++ pname ++"L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++"L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"]) sim (label + 2)
_vexpr prog (Dref addr) = case _vexpr prog addr of
    (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                        ++ ["pop r4 off stack r2"]
                                        ++ ["r4 := m[r0][r4]"])
                                        (stkSimPush (stkSimPop sim) (".val", CType.dref (stkSimPeekType sim))) label of
                                            prog1 -> enforceTypeSize prog1

-- relational operations
_vexpr prog (Le x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <=s r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".le", Char)) (label + 2)
_vexpr prog (Lt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 <s r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".lt", Char)) (label + 2)
_vexpr prog (Ge x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >=s r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".ge", Char)) (label + 2)
_vexpr prog (Gt x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 >s r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".gt", Char)) (label + 2)
_vexpr prog (Eq x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 == r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".eq", Char)) (label + 2)
_vexpr prog (Ne x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 != r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".ne", Char)) (label + 2)
_vexpr prog (Or x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 | r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".or", Char)) (label + 2)
_vexpr prog (And x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["if (r3 & r4) goto " ++ pname ++ "L"  ++ show label]
                                            ++ ["push 0 on stack r2"]
                                            ++ ["goto " ++ pname ++ "L" ++ show (label + 1)]
                                            ++ [pname ++ "L" ++ show label ++ ":"]
                                            ++ ["push 1 on stack r2"]
                                            ++ [pname ++ "L" ++ show (label + 1) ++ ":"])
                                            (stkSimPush (stkSimPop (stkSimPop sim)) (".and", Char)) (label + 2)
-- binary operations
_vexpr prog (Sub x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 - r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Add x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 + r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Div x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname(asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 / r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Mul x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 * r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Bor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 | r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Xor x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 xor r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Mod x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 mod r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr prog (Band x y) = case _vexpr prog x of
    prog1 -> case _vexpr prog1 y of
        (AsmProg pname asm sim label) -> case AsmProg pname (asm
                                            ++ ["pop r4 off stack r2"]
                                            ++ ["pop r3 off stack r2"]
                                            ++ ["r4 := r3 & r4"]) (stkSimPop (stkSimPop sim)) label of
                                                (AsmProg pname asm1 sim1 label1) -> case evalTypes (stkSimPeekType sim) (stkSimPeekType (stkSimPop sim)) of
                                                    typ -> case stkSimPush sim1 (".sub", typ) of
                                                        sim2 -> enforceTypeSize (AsmProg pname asm1 sim2 label)
_vexpr _ _ = error "fatal error: not a valid expression"

-- data type sizes are enforced in a lazy manner
-- assumes r4 contains the value we are enforcing type size for
enforceTypeSize :: AsmProg -> AsmProg
enforceTypeSize prog@(AsmProg pname asm sim label) = case stkSimPeekType sim of
    Char -> AsmProg pname (asm ++ ["r4 := r4 mod 256"]
                        ++ ["push r4 on stack r2"]) sim label
    _ -> AsmProg pname (asm ++ ["push r4 on stack r2"]) sim label

-- given the types of both values in a binary expression returns the type of the result
evalTypes :: CType -> CType -> CType
evalTypes Char Char = Char
evalTypes p@(Ptr _) _ = p
evalTypes _ p@(Ptr _) = p
evalTypes (Arr _ x) _ = Ptr x
evalTypes _ (Arr _ x) = Ptr x
evalTypes _ _ = Int

-- get the name of the function who's scope we are in
getFuncName :: StackSim -> String
getFuncName (StackSim ((StkFrame ('.':'f':'u':'n':'c':'_':ident) _ _):stk)) = ident
getFuncName (StackSim (_:stk)) = getFuncName $ StackSim stk
getFuncName (StackSim _) = error "fatal error: can't find function name: stack empty"

-- get the name of the the while loop who's scope we are in
getWhileName :: StackSim -> String
getWhileName (StackSim ((StkFrame ('.':'w':'h':'i':'l':'e':'_':ident) _ _):stk)) = ident
getWhileName (StackSim (_:stk)) = getWhileName $ StackSim stk
getWhileName (StackSim _) = error "fatal error: can't find while loop name: stack empty"

-- add a string literal to the data section
makeStrLit :: String -> [String]
makeStrLit str = _pc str ++ [".data 0"] where
    _pc (x:xs) = (".data " ++ show (ord x)) : _pc xs
    _pc [] = []
