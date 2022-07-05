module SymbTable where

import CType ( CType )
import qualified Data.Map as Map
import ASTUtils (Identifier)

data SymbTable = SymbTable {table :: Map.Map Identifier CType,
                            parent :: SymbTable}
                | SymbTableNull deriving Show

symbTablePushScope :: SymbTable -> SymbTable
symbTablePushScope = SymbTable Map.empty

symbTablePopScope :: SymbTable -> SymbTable
symbTablePopScope (SymbTable _ x) = x
symbTablePopScope SymbTableNull = error "fatal error: parent scope does not exist"

symbTableEmpty :: SymbTable
symbTableEmpty = SymbTable Map.empty SymbTableNull

symbTableQuery :: Identifier -> SymbTable -> Maybe CType
symbTableQuery id (SymbTable t p) = case Map.lookup id t of
  Nothing -> symbTableQuery id p
  Just ct -> Just ct
symbTableQuery _ SymbTableNull = Nothing

symbTableInsert :: Identifier -> CType -> SymbTable -> SymbTable
symbTableInsert id typ (SymbTable t p) = SymbTable (Map.insert id typ t) p
symbTableInsert _ _ _ = error "fatal error: can't insert into empty symbol table"