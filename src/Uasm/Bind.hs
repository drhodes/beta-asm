{-# LANGUAGE FlexibleInstances #-}
module Uasm.Bind where

import           Control.Monad
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types

-- 
class Bind a where
  bind :: SymbolTable -> a -> Either String a

instance Bind a => Bind [a] where
  bind st xs = sequence $ map (bind st) xs

instance Bind TopLevel where
  bind st (TopStmt stmt) = liftM TopStmt $ bind st stmt
  bind st (TopMacro stmt) = Left "Can't bind an unexpanded macro"

instance Bind Stmt where
  bind st s@(StmtProc _) = Right s
  bind st (StmtCall call) = liftM StmtCall $ bind st call
  bind st (StmtAssn assn) = liftM StmtAssn $ bind st assn 
  bind st (StmtExpr expr) = liftM StmtExpr $ bind st expr

instance Bind Expr where
  bind st (ExprNeg expr) = liftM ExprNeg $ bind st expr
  bind st (ExprTerm term) = liftM ExprTerm $ bind st term
  bind st (ExprTermExpr term exprs) =
    liftM2 ExprTermExpr (bind st term) (bind st exprs)
  bind st (ExprBinTail binop term) = liftM (ExprBinTail binop) (bind st term)
  

instance Bind Term where
  bind st t@(TermIdent ident) =
    case SymTab.lookup (KeyIdent ident) st of
      (Just expr) -> liftM TermExpr (bind st expr)
      Nothing -> Right t
    
  bind st t@(TermLitNum litnum) = Right t
  bind st (TermNeg term) = liftM TermNeg (bind st term)
  bind st (TermExpr expr) = liftM TermExpr (bind st expr)

instance Bind Assn where
  bind st (Assn ident expr) = liftM (Assn ident) (bind st expr)

instance Bind Call where
  bind st (Call ident exprs) = liftM (Call ident) (bind st exprs)

