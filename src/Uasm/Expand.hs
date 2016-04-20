{-# LANGUAGE FlexibleInstances #-}
module Uasm.Expand where

import           Uasm.Eval
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types

class Expand a where
  expand :: a -> SymbolTable -> (Either String [Value], SymbolTable)

accum (Right xs, _) (Right ys, st) = (Right $ xs ++ ys, st)
accum x@(Left msg, _) _ = x
accum _ x@(Left msg, _) = x

instance (Expand e) => Expand [e] where
  expand [] st = (Right [], st)
  expand (x:[]) st = expand x st
  expand (x:xs) st = case expand x st of
    x1@(Right _, nextSt) -> accum x1 (expand xs nextSt)
    (Left msg, _) -> (Left msg, st)

instance Expand TopLevel where
  expand (TopStmt stmt) st = expand stmt st
  expand (TopMacro mac@(Macro name args  _)) st =
    (Right [], SymTab.insert (KeyMacro name (length args)) (ValMacro mac) st)

instance Expand Stmt where
  expand (StmtCall call) st = expand call st
  expand (StmtAssn assn) st = expand assn st
  expand s@(StmtExpr expr) st = (Right [eval expr st], st)
  expand s@(StmtProc proc) st = (Right [ValProc proc], st)

instance Expand Assn where
  expand (Assn ident expr) st = 
    let k = KeyIdent ident
    in (Right [], SymTab.insert k (eval expr st) st)

instance Expand Call where
  expand (Call ident exprs) st =
    let k = KeyMacro ident (length exprs)
    in case SymTab.lookup k st of
      Just (ValMacro mac) -> expandMacro mac exprs st
      Nothing -> (Left $ "Macro definition not found: " ++ show ident, st)

-- expandMacro :: Value -> [Expr] -> SymbolTable -> Either String [Stmt]
expandMacro (Macro name args stmts) exprs st = 
  if length args == length exprs
  then let innerTable = SymTab.bind (SymTab.nest st) args exprs
       in expand stmts innerTable
  else (Left "Impossible error in Marco.expandMacro", st)

-- first thing after parsing, expand the macros into stmts.
