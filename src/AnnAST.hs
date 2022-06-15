module AnnAST where
import CType ( CType )
import Var ( Var )
import Text.Parsec ( SourcePos )

newtype AnnAst = AAst [AExtDecl] deriving Show
data AExtDecl = AFuncDefn SourcePos CType String [AVarDecl] [AStat] 
            | AFuncDecl SourcePos CType String [AVarDecl] 
            deriving Show
data AFuncCall = AFuncCall SourcePos String [AExpr] 
                deriving Show
data AVarDecl = AVarDecl SourcePos CType Var 
                deriving Show
data AStat = AReturnS SourcePos AExpr
        | AAssignS SourcePos AExpr AExpr 
        | AVarDeclS SourcePos AVarDecl AExpr 
        | AArrDeclS SourcePos AVarDecl [AExpr] 
        | AFuncCallS SourcePos AFuncCall 
        | AIfS SourcePos AExpr [AStat] 
        | AWhileS SourcePos AExpr [AStat] 
        | AOut SourcePos AExpr 
        | ABreak SourcePos
        deriving Show
data AExpr = AIntE SourcePos Int
        | ACharE SourcePos Char
        | AVarE SourcePos Var
        | ARefE SourcePos Var
        | ANeg SourcePos AExpr
        | ALno SourcePos AExpr 
        | ANo SourcePos AExpr 
        | ADref SourcePos AExpr 
        | AFunE SourcePos AFuncCall 
        | AGe SourcePos AExpr AExpr 
        | ALe SourcePos AExpr AExpr 
        | AEq SourcePos AExpr AExpr 
        | ANe SourcePos AExpr AExpr 
        | AGt SourcePos AExpr AExpr 
        | ALt SourcePos AExpr AExpr 
        | AOr SourcePos AExpr AExpr 
        | AAnd SourcePos AExpr AExpr 
        | AAdd SourcePos AExpr AExpr 
        | ASub SourcePos AExpr AExpr 
        | AMul SourcePos AExpr AExpr 
        | ADiv SourcePos AExpr AExpr 
        | ABand SourcePos AExpr AExpr 
        | ABor SourcePos AExpr AExpr 
        | AXor SourcePos AExpr AExpr 
        | AMod SourcePos AExpr AExpr 
        | AStr SourcePos String 
        | AIn SourcePos
        deriving Show