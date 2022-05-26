module AsmGen where
import ASTGen
    ( CType(..),
      Var(Var),
      Expr(Band, IntE, CharE, VarE, In, FunE, Neg, Lno, No, Dref, Le, Lt,
           Ge, Gt, Eq, Ne, Or, And, Sub, Add, Div, Mul, Bor, Xor, Mod),
      Stat(..),
      VarDecl(..),
      FuncCall(..),
      ExtDecl(..),
      Program(..) )
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
import GHC.Char ()
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

data AsmProg = AsmProg [String] StackSim Int deriving Show

-- generate asm for a program
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

-- generate asm for an external declaration
visitExtDecl :: AsmProg -> ExtDecl -> AsmProg
visitExtDecl prog@(AsmProg asm sim label) (FuncDecl _ ident params body) = case stkSimPush (_sspa (stkSimPushFrame sim (".func_" ++ ident)) params) (Var ".ret",U32) of
    result -> case visitStatList (AsmProg (asm ++ ["F_" ++ ident ++ ":"] ++ ["push r1 on stack r2"]) result label) body of
        AsmProg asm1 sim1@(StackSim stk@((StkFrame _ (_:fs) size):_)) label1 -> AsmProg (asm1
                                                                                        ++ ["F_END_" ++ ident ++ ":"]
                                                                                        ++ replicate (size - (length params + 1)) "pop stack r2"
                                                                                        ++ ["pop r4 off stack r2"]
                                                                                        ++ replicate (length params) "pop stack r2"
                                                                                        ++ ["goto r4"] ) (stkSimPopFrame sim1) label1

-- push sequence of arguments onto stack simulation
_sspa :: StackSim -> [VarDecl] -> StackSim
_sspa sim ((VarDecl t v):ps) = stkSimPush (_sspa sim ps) (v,t)
_sspa sim [] = sim

-- generate asm for a list of statements
visitStatList :: AsmProg -> [Stat] -> AsmProg
visitStatList prog [] = prog
visitStatList prog (x:xs) = case visitStat prog x of
    result -> visitStatList result xs

-- generate asm for a statement
visitStat :: AsmProg -> Stat -> AsmProg
visitStat prog (ReturnS exp) = case visitExpr prog exp of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["pop r1 off stack r2"]
                                                                                ++ ["goto F_END_" ++ getFuncName sim]) (stkSimPop sim) label
visitStat prog (VarDeclS (VarDecl typ var) init) = case visitExpr prog init of
    (AsmProg asm sim label) -> AsmProg asm (stkSimPush (stkSimPop sim) (var, typ)) label
visitStat prog (ArrDeclS (VarDecl (Arr size typ) var) []) = case pushExprSeq prog (replicate size (IntE 0)) of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var,Ptr typ)) label
visitStat prog (ArrDeclS (VarDecl typ var) init) = case pushExprSeq prog init of
    (AsmProg asm sim label) -> AsmProg (asm ++ ["r4 := r2"]
                                            ++ ["push r4 on stack r2"]) (stkSimPush sim (var,Ptr typ)) label
visitStat prog (IfS cond body) = case visitExpr prog cond of
    (AsmProg asm sim label) -> case visitStatList (AsmProg (asm
                                                                                             ++ ["pop r4 off stack r2"]
                                                                                             ++ ["if (r4 == 0) goto L" ++ show label])
                                                                                     (stkSimPushFrame (stkSimPop sim) ".if") (label + 1)) body of
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
visitStat prog (FuncCallS call) = visitFuncCall prog call
visitStat prog@(AsmProg asm sim label) (WhileS cond body) = case visitExpr (AsmProg (asm ++ ["WHILE_" ++ show label ++ ":"]) sim (label + 1)) cond of
    (AsmProg asm1 sim1@(StackSim ((StkFrame sfid _ _):_)) label1) -> case visitStatList (AsmProg (asm1
                                                                                                ++ ["pop r4 off stack r2"]
                                                                                                ++ ["if (r4 == 0) goto WHILE_END_" ++ show label])
                                                                                        (stkSimPushFrame (stkSimPop sim1) (".while_" ++ show label)) label1) body of
        AsmProg asm2 sim2@(StackSim ((StkFrame _ _ size):_)) label2 -> AsmProg (asm2
                                                                                    ++ replicate size "pop stack r2"
                                                                                    ++ ["goto WHILE_" ++ show label]
                                                                                    ++ ["WHILE_END_" ++ show label ++ ":"]) (stkSimPopFrame sim2) label2
visitStat (AsmProg asm sim@(StackSim stk@((StkFrame _ _ size):_)) label) Break = AsmProg (asm
                                                                                        ++ replicate (_sizewhile stk) "pop stack r2"
                                                                                        ++ ["goto WHILE_END_" ++ getWhileName sim]) sim label where
                                                                                            _sizewhile ((StkFrame ('.':'w':'h':'i':'l':'e':'_':_) _ size):_) = size
                                                                                            _sizewhile ((StkFrame _ _ size):stk) = _sizewhile stk + size

-- geneate asm for a function call
visitFuncCall :: AsmProg -> FuncCall -> AsmProg
visitFuncCall (AsmProg asm sim label) (FuncCall ident args) = case pushExprSeq (AsmProg asm (stkSimPushFrame sim (".call_" ++ ident)) label) args of
    (AsmProg asm1 sim1 label1) -> AsmProg (asm1 ++ ["goto F_" ++ ident ++ " linking r1"]) (stkSimPopFrame sim1) label1

-- generate asm for a sequence of expressions, results are pushed onto the stack in reverse order
pushExprSeq :: AsmProg -> [Expr] -> AsmProg
pushExprSeq prog x = _pes prog (reverse x) where
    _pes prog (x:xs) = case visitExpr prog x of
        prog1 -> _pes prog1 xs
    _pes prog [] = prog

-- visit expression wrapper --
visitExpr :: AsmProg -> Expr -> AsmProg
visitExpr (AsmProg asm sim label) exp = case _vexpr (AsmProg asm (stkSimPushFrame sim ".expr") label) exp of
    (AsmProg asm1 sim1@(StackSim ((StkFrame _ (_:fs) _):_)) label1) -> AsmProg asm1 (stkSimPush (stkSimPopFrame sim1) (Var ".result", U32)) label1

-- generate asm for an expression
-- pushes an additional value onto the stack
_vexpr :: AsmProg -> Expr -> AsmProg
-- stack-push operations
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

-- relational operations
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
-- binary operations
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

-- data type sizes are enforced in a lazy manner
-- assumes r4 contains the value we are enforcing type size for
enforceTypeSize :: AsmProg -> AsmProg
enforceTypeSize prog@(AsmProg asm sim label) = case stkSimPeekType sim of
    U8 -> AsmProg (asm ++ ["r4 := r4 mod 256"]
                        ++ ["push r4 on stack r2"]) sim label
    _ -> AsmProg (asm ++ ["push r4 on stack r2"]) sim label

-- given the types of both values in a binary expression returns the type of the result
evalTypes :: CType -> CType -> CType
evalTypes U8 U8 = U8
evalTypes p@(Ptr _) _ = p
evalTypes _ p@(Ptr _) = p
evalTypes (Arr _ x) _ = Ptr x
evalTypes _ (Arr _ x) = Ptr x
evalTypes _ _ = U32

-- given a type get the type of the result of dereferencing a value of that type
dref :: CType -> CType
dref (Ptr x) = x
dref (Arr _ x) = x
dref x = error ("type " ++ show x ++ " cannot be dereferenced")

-- get the name of the function who's scope we are in
getFuncName :: StackSim -> String
getFuncName (StackSim ((StkFrame ('.':'f':'u':'n':'c':'_':ident) _ _):stk)) = ident
getFuncName (StackSim (_:stk)) = getFuncName $ StackSim stk
getFuncName (StackSim _) = error "can't find function name: stack empty"

-- get the name of the the while loop who's scope we are in
getWhileName :: StackSim -> String
getWhileName (StackSim ((StkFrame ('.':'w':'h':'i':'l':'e':'_':ident) _ _):stk)) = ident
getWhileName (StackSim (_:stk)) = getWhileName $ StackSim stk
getWhileName (StackSim _) = error "can't find while loop name: stack empty"