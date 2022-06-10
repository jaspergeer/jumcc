module SymbTable where

import ASTGen ( CType )

data SymbTable = SymbTable [Entry] SymbTable | SymbTableEmpty
data Entry = Var String CType | Func String [CType] CType
newtype SymbTableError = SymbTableError String

symbTableAdd :: SymbTable -> Entry -> SymbTable
symbTableAdd (SymbTable symbs parent) x = SymbTable (x:symbs) parent
symbTableAdd SymbTableEmpty x = error "Can't add symbol to empty table"

symbTablePushScope :: SymbTable -> SymbTable
symbTablePushScope = SymbTable []

symbTablePopScope :: SymbTable -> SymbTable
symbTablePopScope (SymbTable _ x) = x
symbTablePopScope SymbTableEmpty = error "Parent scope does not exist"

symbTableQueryType :: SymbTable -> String -> CType
symbTableQueryType (SymbTable [] parent) x = symbTableQueryType parent x
symbTableQueryType (SymbTable ((Var id typ):ys) parent) x = 
    if x == id then typ else symbTableQueryType (SymbTable ys parent) x
symbTableQueryType (SymbTable ((Func id _ typ):ys) parent) x = 
    if x == id then typ else symbTableQueryType (SymbTable ys parent) x
symbTableQueryType SymbTableEmpty x = error ("Symbol " ++ x ++ " does not exist")

symbTableQueryParams :: SymbTable -> String -> [CType]
symbTableQueryParams (SymbTable [] parent) x = symbTableQueryParams parent x
symbTableQueryParams (SymbTable ((Var id typ):ys) parent) x = 
    if x == id then error ("Identifier " ++ x  ++ " refers to a function, not a variable") else symbTableQueryParams (SymbTable ys parent) x
symbTableQueryParams (SymbTable ((Func id params _):ys) parent) x = 
    if x == id then params else symbTableQueryParams (SymbTable ys parent) x
symbTableQueryParams SymbTableEmpty x = error ("Symbol " ++ x ++ " does not exist")