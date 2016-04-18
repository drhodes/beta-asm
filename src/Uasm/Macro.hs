{-# LANGUAGE FlexibleInstances #-}
module Uasm.Macro where

import qualified Uasm.SymbolTable as SymTab
import Uasm.Types

class Expand a where
  expand :: a -> SymbolTable -> (Either String [Stmt], SymbolTable)

accum (Right xs, _) (Right ys, st) = (Right $ xs ++ ys, st)
accum x@(Left msg, _) _ = x
accum _ x@(Left msg, _) = x

instance (Expand e) => Expand [e] where
  expand [] symtab = (Right [], symtab)
  expand (x:[]) symtab = expand x symtab
  expand (x:xs) symtab = case expand x symtab of
    x1@(Right _, nextSymTab) -> accum x1 (expand xs nextSymTab)
    (Left msg, _) -> (Left msg, symtab)

instance Expand TopLevel where
  expand (TopStmt stmt) symtab = expand stmt symtab
  expand (TopMacro _) symtab = (Right [], symtab)

instance Expand Stmt where
  expand (StmtCall call) symtab = expand call symtab
  expand (StmtAssn assn) symtab = expand assn symtab
  expand s@(StmtExpr expr) symtab = (Right [s], symtab)
  expand _ symtab = error "Need to implement Expand Stmt"

instance Expand Assn where
  expand (Assn ident expr) symtab = 
    let k = KeyIdent ident
    in (Right [], SymTab.insert k (ValExpr expr) symtab)

instance Expand Call where
  expand (Call ident exprs) symtab =
    let k = KeyMacro ident (length exprs)
    in case SymTab.lookup k symtab of
      Just (ValMacro mac) -> expandMacro mac exprs symtab
      Nothing -> (Left $ "Macro definition not found: " ++ show ident, symtab)

-- expandMacro :: Value -> [Expr] -> SymbolTable -> Either String [Stmt]
expandMacro (Macro name args stmts) exprs symtab = 
  if length args == length exprs
  then let innerTable = SymTab.bind (SymTab.nest symtab) args exprs
       in expand stmts symtab
  else (Left "Impossible error in Marco.expandMacro", symtab)       

-- first thing after parsing, expand the macros into stmts.
