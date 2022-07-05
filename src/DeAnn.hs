module DeAnn where
import AnnAST
    ( AExpr(..),
      AStat(..),
      AVarDecl(..),
      AFuncCall(..),
      AExtDecl(..),
      AnnAST (AnnAST) )
import AST
    ( AST (AST),
      Expr(..),
      ExtDecl(..),
      FuncCall(..),
      Stat(..),
      VarDecl(..),
      Expr(VarE) )

deAnnotate :: AnnAST -> AST
deAnnotate (AnnAST aeds) = AST (deAExtDecls aeds)

deAExtDecls :: [AExtDecl] -> [ExtDecl]
deAExtDecls = foldr (\x -> (++) [deAExtDecl x]) []

deAExtDecl :: AExtDecl -> ExtDecl
deAExtDecl x = case x of
  AFuncDefn sp ct id avds asts -> FuncDefn ct id (deAVarDecls avds) (deAStats asts)
  AFuncDecl sp ct id avds -> FuncDecl ct id (deAVarDecls avds) where

deAVarDecls :: [AVarDecl] -> [VarDecl]
deAVarDecls = foldr (\x -> (++) [deAVarDecl x]) []

deAStat :: AStat -> Stat
deAStat x = case x of
  AReturnS sp ae -> ReturnS (deAExpr ae)
  AAssignS sp ae ae' -> AssignS (deAExpr ae) (deAExpr ae')
  AVarDeclS sp avd ae -> VarDeclS (deAVarDecl avd) (deAExpr ae)
  AArrDeclS sp avd aes -> ArrDeclS (deAVarDecl avd) (deAExprs aes)
  AFuncCallS sp afc -> FuncCallS (deAFuncCall afc)
  AIfS sp ae asts -> IfS (deAExpr ae) (deAStats asts)
  AWhileS sp ae asts -> WhileS (deAExpr ae) (deAStats asts)
  AOut sp ae -> Out (deAExpr ae)
  ABreak sp -> Break

deAStats :: [AStat] -> [Stat]
deAStats = foldr (\x -> (++) [deAStat x]) []

deAExpr :: AExpr -> Expr
deAExpr x = case x of
  AIntE sp n -> IntE n
  ACharE sp c -> CharE c
  AVarE sp s -> VarE s
  ARefE sp s -> RefE s
  ANeg sp ae -> Neg (deAExpr ae)
  ALno sp ae -> Lno (deAExpr ae)
  ANo sp ae -> No (deAExpr ae)
  ADref sp ae -> Dref (deAExpr ae)
  AFunE sp afc -> FunE (deAFuncCall afc)
  AGe sp ae ae' -> Ge (deAExpr ae) (deAExpr ae')
  ALe sp ae ae' -> Le (deAExpr ae) (deAExpr ae')
  AEq sp ae ae' -> Eq (deAExpr ae) (deAExpr ae')
  ANe sp ae ae' -> Ne (deAExpr ae) (deAExpr ae')
  AGt sp ae ae' -> Gt (deAExpr ae) (deAExpr ae')
  ALt sp ae ae' -> Lt (deAExpr ae) (deAExpr ae')
  AOr sp ae ae' -> Or (deAExpr ae) (deAExpr ae')
  AAnd sp ae ae' -> And (deAExpr ae) (deAExpr ae')
  AAdd sp ae ae' -> Add (deAExpr ae) (deAExpr ae')
  ASub sp ae ae' -> Sub (deAExpr ae) (deAExpr ae')
  AMul sp ae ae' -> Mul (deAExpr ae) (deAExpr ae')
  ADiv sp ae ae' -> Div (deAExpr ae) (deAExpr ae')
  ABand sp ae ae' -> Band (deAExpr ae) (deAExpr ae')
  ABor sp ae ae' -> Bor (deAExpr ae) (deAExpr ae')
  AXor sp ae ae' -> Xor (deAExpr ae) (deAExpr ae')
  AMod sp ae ae' -> Mod (deAExpr ae) (deAExpr ae')
  AStr sp s -> Str s
  AIn sp -> In
  ACastE sp ct ae -> CastE ct (deAExpr ae)

deAFuncCall :: AFuncCall -> FuncCall
deAFuncCall (AFuncCall _ id aexprs) = FuncCall id (deAExprs aexprs)

deAExprs :: [AExpr] -> [Expr]
deAExprs = foldr (\x -> (++) [deAExpr x]) []

deAVarDecl :: AVarDecl -> VarDecl
deAVarDecl (AVarDecl _ ct id) = VarDecl ct id