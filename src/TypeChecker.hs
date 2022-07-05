module TypeChecker where

import SymbTable ( SymbTable (SymbTableNull, table), symbTablePushScope, symbTableEmpty, symbTableQuery, symbTableInsert, symbTablePopScope )
import AnnAST (AnnAST (AnnAST), AExtDecl (AFuncDefn, AFuncDecl), AExpr (AIntE, ACastE, ACharE, AAdd, ASub, AMul, ADiv, AMod, ABand, ABor, AXor, AGe, ALe, AEq, ANe, AGt, ALt, AOr, AAnd, AVarE, ARefE, ADref, AFunE, ANeg, ALno, ANo, AStr, AIn), AFuncCall (AFuncCall), AStat (AReturnS, AAssignS, AVarDeclS, AArrDeclS, AFuncCallS, AIfS, AWhileS, AOut, ABreak), AVarDecl (AVarDecl))
import Control.Monad.Reader (Reader, MonadReader (ask, local), runReader)
import Text.Parsec.Pos (initialPos, SourcePos)
import CType (CType (Int, Char, Ptr, Func, paramTypes, dref, Arr, Void))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), Except, runExceptT)
import ASTUtils (Identifier)

data TCError = TCError {errorPos :: SourcePos,
                        message :: Message}

data Message = Expect CType CType
            | InvalidBinOps CType CType String
            | InvalidRelOps CType CType String
            | InvalidSymbol String
            | CantDeref CType
            | NotCallable Identifier
            | BadExprNum
            | ReDef Identifier
            | BadDef Identifier

instance Show TCError where
    show err = show (errorPos err) ++ " : " ++ showMessage (message err)

showMessage :: Message -> String
showMessage msg = case msg of
    Expect t1 t2 -> "Expected value of type " ++ show t1 ++ " but got a value of type " ++ show t2 ++ " instead"
    InvalidBinOps t1 t2 op -> "Invalid operands to binop '" ++ op ++ "': " ++ show t1 ++ " and " ++ show t2
    InvalidRelOps t1 t2 op -> "Invalid operands to relop '" ++ op ++ "': " ++ show t1 ++ " and " ++ show t2
    InvalidSymbol sym -> "Implicit declaration of symbol '" ++ sym ++ "'"
    CantDeref typ -> "Can't dereference value of type " ++ show typ
    NotCallable n -> "Called object '" ++ n ++ "' is not a function or function pointer"
    BadExprNum -> "Incorrect number of elements in comma-seperated list of expressions"
    ReDef n -> "Redefinition of symbol '" ++ n ++ "'"
    BadDef n -> "Incorrect return type or parameters in definition of function '" ++ n ++ "'"

typeCheck :: AnnAST -> Maybe TCError
typeCheck (AnnAST body) = case runExceptT (checkExtDeclList body) of
    rdr -> case runReader rdr symbTableEmpty of
      Left te -> Just te
      Right ct -> Nothing

checkExtDeclList :: [AExtDecl] -> ExceptT TCError (Reader SymbTable) CType
checkExtDeclList (x:xs) = do
    checkExtDecl x
    checkExtDeclList xs
checkExtDeclList [] = return Void

checkExtDecl :: AExtDecl -> ExceptT TCError (Reader SymbTable) CType
checkExtDecl (AFuncDefn pos ret id params body) = do
    checkExtDecl (AFuncDecl pos ret id params)
    local symbTablePushScope ask
    checkStatList (toVarDeclList params)
    checkStatList body
    local symbTablePopScope ask
    return Void where
        toVarDeclList (decl@(AVarDecl pos _ _):xs) = AVarDeclS pos decl (AIntE pos 0) : toVarDeclList xs
        toVarDeclList [] = []
checkExtDecl (AFuncDecl pos ret id params) = do
    let ftype = Func (toTypeList params) ret
    st <- ask
    case symbTableQuery id st of
      Nothing -> local (symbTableInsert id ftype) ask
      Just ct -> if ftype == ct then ask else throwError $ TCError pos $ BadDef id
    return Void where
        toTypeList ((AVarDecl _ typ _):xs) = typ : toTypeList xs
        toTypeList [] = []

checkStatList :: [AStat] -> ExceptT TCError (Reader SymbTable) CType
checkStatList (x:xs) = do
    checkStat x
    checkStatList xs
checkStatList [] = do
    st <- ask
    error (show st)
    return Void

checkStat :: AStat -> ExceptT TCError (Reader SymbTable) CType
checkStat (AReturnS pos exp) = checkExpr exp
checkStat (AAssignS pos exp1 exp2) = do
    typ1 <- checkExpr exp1
    typ2 <- checkExpr exp2
    if typ1 == typ2 then return typ1 else throwError $ TCError pos $ Expect typ1 typ2
checkStat (AVarDeclS _ (AVarDecl pos typ id) (AIntE _ 0)) = do
    st <- ask
    case symbTableQuery id st of
      Nothing -> local (symbTableInsert id typ) ask
      _ -> throwError $ TCError pos $ ReDef id
    return Void
checkStat (AVarDeclS _ (AVarDecl pos typ id) exp) = do
    etyp <- checkExpr exp
    st <- ask
    case symbTableQuery id st of
      Nothing -> local (symbTableInsert id typ) ask
      _ -> throwError $ TCError pos $ ReDef id
    if etyp == typ then return Void else throwError $ TCError pos $ Expect typ etyp
checkStat (AArrDeclS _ (AVarDecl pos typ@(Arr size elemType) id) []) = do
    st <- ask
    case symbTableQuery id st of
      Nothing -> local (symbTableInsert id typ) ask
      _ -> throwError $ TCError pos $ ReDef id
    return Void
checkStat (AArrDeclS _ (AVarDecl pos typ@(Arr size elemType) id) arr) = do
    st <- ask
    elemTypes <- checkExprList pos arr (replicate size elemType)
    case symbTableQuery id st of
      Nothing -> local (symbTableInsert id typ) ask
      _ -> throwError $ TCError pos $ ReDef id
    return Void
checkStat (AFuncCallS pos call) = checkFuncCall call
checkStat (AIfS _ cond body) = do
    checkExpr cond
    st <- ask
    local symbTablePushScope ask
    checkStatList body
    st <- ask
    local symbTablePopScope ask
    return Void
checkStat (AWhileS _ cond body) = do
    checkExpr cond
    checkStatList body
checkStat (AOut pos exp) = do
    checkExpr exp
    return Void
checkStat (ABreak pos) = return Void
checkStat _ = error "fatal error: invalid statement"

checkExpr :: AExpr -> ExceptT TCError (Reader SymbTable) CType
checkExpr (AIntE _ _) = return Void
checkExpr (ACharE _ _) = return Char
checkExpr (ACastE pos typ exp) = do
    checkExpr exp
    return typ
-- binops
checkExpr (AAdd pos exp1 exp2) = checkBinExpr pos exp1 "+" exp2
checkExpr (ASub pos exp1 exp2) = checkBinExpr pos exp1 "-" exp2
checkExpr (AMul pos exp1 exp2) = checkBinExpr pos exp1 "*" exp2
checkExpr (ADiv pos exp1 exp2) = checkBinExpr pos exp1 "/" exp2
checkExpr (AMod pos exp1 exp2) = checkBinExpr pos exp1 "%" exp2
checkExpr (ABand pos exp1 exp2) = checkBinExpr pos exp1 "&" exp2
checkExpr (ABor pos exp1 exp2) = checkBinExpr pos exp1 "|" exp2
checkExpr (AXor pos exp1 exp2) = checkBinExpr pos exp1 "^" exp2
-- relops
checkExpr (AGe pos exp1 exp2) = checkRelExpr pos exp1 ">=" exp2
checkExpr (ALe pos exp1 exp2) = checkRelExpr pos exp1 "<=" exp2
checkExpr (AEq pos exp1 exp2) = checkRelExpr pos exp1 "==" exp2
checkExpr (ANe pos exp1 exp2) = checkRelExpr pos exp1 "!=" exp2
checkExpr (AGt pos exp1 exp2) = checkRelExpr pos exp1 ">" exp2
checkExpr (ALt pos exp1 exp2) = checkRelExpr pos exp1 "<" exp2
checkExpr (AOr pos exp1 exp2) = checkRelExpr pos exp1 "||" exp2
checkExpr (AAnd pos exp1 exp2) = checkRelExpr pos exp1 "&&" exp2
-- prefix expressions
checkExpr (ANeg pos exp) = checkPrefExpr pos "-" exp
checkExpr (ALno pos exp) = checkPrefExpr pos "~" exp
checkExpr (ANo pos exp) = checkPrefExpr pos "!" exp
checkExpr (ADref pos exp) = checkPrefExpr pos "*" exp

checkExpr (AVarE pos var) = do
    st <- ask
    case symbTableQuery var st of
      Nothing -> throwError $ TCError pos $ InvalidSymbol var
      Just typ -> return typ
checkExpr (ARefE pos var) = do
    st <- ask
    case symbTableQuery var st of
      Nothing -> throwError $ TCError pos $ InvalidSymbol var
      Just typ -> return $ Ptr typ
checkExpr (AFunE pos1 call) = checkFuncCall call
checkExpr (AStr pos str) = return $ Ptr Char
checkExpr (AIn pos) = return Char

checkFuncCall :: AFuncCall -> ExceptT TCError (Reader SymbTable) CType
checkFuncCall (AFuncCall pos fname args) = do
    st <- ask
    ftype <- case symbTableQuery fname st of
      Nothing -> throwError $ TCError pos $ InvalidSymbol fname
      Just ct@(Func _ _) -> return ct
      Just typ -> throwError $ TCError pos $ NotCallable fname
    params <- checkExprList pos args $ paramTypes ftype
    return Void

checkExprList :: SourcePos -> [AExpr] -> [CType] -> ExceptT TCError (Reader SymbTable) [CType]
checkExprList pos (x:xs) (y:ys) = do
    rest <- checkExprList pos xs ys
    typ <- checkExpr x
    if typ == y then return $ typ : rest else throwError $ TCError pos $ Expect y typ
checkExprList pos [] [] = return []
checkExprList pos _ _ = throwError $ TCError pos BadExprNum

checkBinExpr :: SourcePos -> AExpr -> String -> AExpr -> ExceptT TCError (Reader SymbTable) CType
checkBinExpr pos exp1 op exp2 = do
    t1 <- checkExpr exp1
    t2 <- checkExpr exp2
    cbe pos t1 op t2 where
        cbe :: SourcePos -> CType -> String -> CType -> ExceptT TCError (Reader SymbTable) CType
        cbe _ p@(Ptr _) _ Int = return p
        cbe _ Int _ p@(Ptr _) = return p
        cbe _ p@(Ptr _) _ Char = return p
        cbe _ Char _ p@(Ptr _) = return p
        cbe _ Int _ _ = return Int
        cbe _ _ _ Int = return Int
        cbe _ Char _ _ = return Char
        cbe _ _ _ Char = return Char
        cbe pos a op b = if a == b then return a else throwError $ TCError pos (InvalidBinOps a b op)

checkRelExpr :: SourcePos -> AExpr -> String -> AExpr -> ExceptT TCError (Reader SymbTable) CType
checkRelExpr pos exp1 op exp2 = do
    t1 <- checkExpr exp1
    t2 <- checkExpr exp2
    if t1 == t2 then return Int else throwError $ TCError pos (InvalidRelOps t1 t2 op)

checkPrefExpr :: SourcePos -> String -> AExpr -> ExceptT TCError (Reader SymbTable) CType
checkPrefExpr pos op exp = do
    typ <- checkExpr exp
    cpe pos op typ where
        cpe :: SourcePos -> String -> CType -> ExceptT TCError (Reader SymbTable) CType
        cpe _ "!" typ = return Int
        cpe _ "~" typ = return typ
        cpe _ "-" typ = return typ
        cpe _ "*" typ@(Ptr _) = return $ dref typ
        cpe pos "*" typ = throwError $ TCError pos $ CantDeref typ
        cpe _ _ _ = error "fatal error: prefix expression check failed"