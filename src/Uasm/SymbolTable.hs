{-# LANGUAGE FlexibleContexts #-}
module Uasm.SymbolTable where


import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map as DM
import           Prelude hiding (lookup)
import           Text.PrettyPrint.GenericPretty
import           Uasm.Types

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

nest symtab = SymTab DM.empty DM.empty symtab

build xs = SymTab (DM.fromList xs) DM.empty NullTable

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
       NullTable -> throwError "Can't somethign soemthing null symbol table"
       (SymTab exprs macs next) ->
         do let macs' = DM.insert (KeyMacro ident (length args)) mac macs
            put $ SymTab exprs macs' next
                      
