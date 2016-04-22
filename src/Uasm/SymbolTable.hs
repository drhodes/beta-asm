{-# LANGUAGE FlexibleContexts #-}
module Uasm.SymbolTable where

import qualified Data.Map as DM
import           Prelude hiding (lookup)
import           Uasm.Types
import Control.Monad.State
import           Control.Monad.Trans.Except

new = SymTab DM.empty DM.empty NullTable

lookup :: SymbolKey -> SymbolTable -> Maybe Expr
lookup (KeyIdent CurInstruction) _ = Just (ExprTerm $ TermIdent CurInstruction)
lookup k NullTable = Nothing
lookup k (SymTab cur macs next) =
  case DM.lookup k cur of
    (Just v) -> Just v
    Nothing -> lookup k next

lookupMacro :: SymbolKey -> SymbolTable -> Maybe Macro
lookupMacro (KeyIdent CurInstruction) _ = Nothing
lookupMacro k NullTable = Nothing
lookupMacro k (SymTab cur macs next) =
  case DM.lookup k macs of
    (Just v) -> Just v
    Nothing -> lookupMacro k next




insert k t NullTable = error "SymbolTable.insert deadcode"
insert k t (SymTab cur macs rest) = SymTab (DM.insert k t cur) macs rest

-- insert k t NullTable = error "SymbolTable.insert deadcode"
-- insert k t (SymTab m rest) = SymTab (DM.insert k t m) rest


nest symtab = SymTab DM.empty DM.empty symtab

build xs = SymTab (DM.fromList xs) DM.empty NullTable

-- bind symtab [] [] = symtab
-- bind symtab (i:idents) (x:exprs) =
--   bind (insert (KeyIdent i) (ValExpr x) symtab) idents exprs

mInsert :: MonadState SymbolTable m => SymbolKey -> Expr -> m ()
mInsert k t = modify (insert k t)

mLookup :: MonadState SymbolTable m => SymbolKey -> m (Maybe Expr)
mLookup k = do st <- get
               return $ lookup k st

               
mLookupMacro :: MonadState SymbolTable m => SymbolKey -> m (Maybe Macro)
mLookupMacro k =
  do st <- get
     return $ lookupMacro k st
               
mInsertMacro mac@(Macro ident args _) =
  do st <- get
     case st of
       NullTable -> throwE "Can't somethign soemthing null symbol table"
       (SymTab exprs macs next) ->
         do let macs' = DM.insert (KeyMacro ident (length args)) mac macs
            put $ SymTab exprs macs' next
                      
