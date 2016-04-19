module Uasm.SymbolTable where

import qualified Data.Map as DM
import           Prelude hiding (lookup)
import           Uasm.Types

new = SymTab DM.empty NullTable

lookup :: SymbolKey -> SymbolTable -> Maybe Value
lookup k NullTable = Nothing
lookup k (SymTab cur next) =
  case DM.lookup k cur of
    (Just v) -> Just v
    Nothing -> lookup k next

insert k t NullTable = error "SymbolTable.insert deadcode"
insert k t (SymTab m rest) = SymTab (DM.insert k t m) rest
-- lookup k (SymTab cur next) =
--   case DM.lookup k cur of
--     (Just v) -> Just v
--     Nothing -> lookup k next

nest symtab = SymTab DM.empty symtab

bind symtab [] [] = symtab
bind symtab (i:idents) (x:exprs) =
  bind (insert (KeyIdent i) (ValExpr x) symtab) idents exprs
  
