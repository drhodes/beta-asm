{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Uasm.Expand where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Functor.Identity
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types
import Control.Monad.Except

type ExpandErr b = forall m. ( MonadState SymbolTable m,
                               MonadError String m ) => m b

-- runStateExceptT :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)

runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: Monad m => s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

expand expandFunc node = runIdentity . runExceptStateT (SymTab.new) $ expandFunc node

uniRunExpand :: [TopLevel] -> Either [Char] [TopLevel]
uniRunExpand nodes = fst <$> expand expandTopLevels nodes


flattenTops :: [TopLevel] -> [Stmt]
flattenTops [] = []
flattenTops ((TopStmt stmt):rest) = flattenStmt stmt ++ (flattenTops rest)
flattenTops ((TopMacro _):rest) = flattenTops rest

flattenStmt (StmtMany stmts) = concat $ map flattenStmt stmts
flattenStmt (StmtExpr (ExprTerm (TermExpr x))) = flattenStmt (StmtExpr x)
flattenStmt x = [x]

  
expandTopLevels :: [TopLevel] -> ExpandErr [TopLevel]
expandTopLevels xs = mapM expandTopLevel xs

expandTopLevel :: TopLevel -> ExpandErr TopLevel
expandTopLevel (TopStmt stmt) = TopStmt <$> expandStmt stmt
expandTopLevel (TopMacro mac) = SymTab.mInsertMacro mac >> return (TopMacro mac)

expandStmt :: Stmt -> ExpandErr Stmt
expandStmt (StmtAssn assn) = StmtAssn <$> expandAssn assn -- this inserts an ident.
expandStmt (StmtExpr expr) = StmtExpr <$> expandExpr expr 
expandStmt (StmtProc proc) = return $ StmtProc proc
expandStmt (StmtCall call) = expandCall call
expandStmt (StmtMany stmts) = StmtMany <$> mapM expandStmt stmts

-- expandCall takes a macro call and expands it into statements with
-- as many identifiers bound as possible.  Some of the identifiers may
-- be labels, and if so then they must be left alone until the label
-- pass.  Why? No expressions have been evaluated yet, and some
-- expressions involve the (.) current instruction, therefore label
-- addresses can't be known, yet.

expandCall :: Call -> ExpandErr Stmt
expandCall (Call ident exprs) =
  do let k = KeyMacro ident (length exprs)
     mac <- SymTab.mLookupMacro k
     case mac of
       Just mac' -> bindMacro exprs mac'
       Nothing -> throwError $ show ("Macro definition not found: " ++ show ident)

bindMacro :: [Expr] -> Macro -> ExpandErr Stmt
bindMacro exprs (Macro _ args stmts) =
  -- flatten macro into many statements
  do when (length args /= length exprs) $
       throwError "(Improbable error) in Expand.bindMacro, argument length mismatch"
     curScope <- get     
     -- bind exprs to argument idents and construct new symbol table
     exprs' <- mapM expandExpr exprs
     let (SymTab ids macs _) = SymTab.build (zip (map KeyIdent args) exprs')
     
     -- push the new symbol table to give priority to argument
     -- identifiers.
     put (SymTab ids macs curScope)

     -- st <- get
     -- throwError $ show st
     
     -- throwError $ show (SymTab ids macs curScope)
     -- expand statments defined by the macro
     stmts <- StmtMany <$> mapM expandStmt stmts
     -- done expanding, restore current scope
     put curScope
     return stmts

expandAssn assn@(Assn ident expr) =
  do let k = KeyIdent ident
     if ident == CurInstruction          
       then do Assn ident <$> expandExpr expr
       else do SymTab.mInsert k expr
               return assn

expandTerm :: Term -> ExpandErr Term
expandTerm term@(TermIdent ident) =
  do let k = KeyIdent ident     
     if ident == CurInstruction          
       then return term
       else do expr <- SymTab.mLookup k
               case expr of
                 Just expr -> return $ TermExpr expr
                 Nothing -> return term --throwError $ "Couldn't find term: " ++ (show ident)

expandTerm (TermExpr expr) = TermExpr <$> expandExpr expr
expandTerm (TermNeg term) = TermNeg <$> expandTerm term
expandTerm term = return term

expandExpr :: Expr -> ExpandErr Expr
expandExpr (ExprNeg expr) = ExprNeg <$> expandExpr expr
expandExpr (ExprTerm term) = ExprTerm <$> expandTerm term
expandExpr (ExprTermExpr term []) = ExprTerm <$> expandTerm term
expandExpr (ExprTermExpr term exprs) =
  liftM2 ExprTermExpr (expandTerm term) (mapM expandExpr exprs)
expandExpr (ExprBinTail binop term) = ExprBinTail binop <$> expandTerm term
