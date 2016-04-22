{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Uasm.Expand where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map as DM
import           Data.Functor.Identity
import Control.Monad.Identity
import           Uasm.Bind
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Functor.Identity


type ExpandErr a b = forall m. ( MonadState SymbolTable m,
                                 MonadError String m ) => a -> m b

runStateExceptT :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: Monad m => s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

foo = runIdentity . runExceptStateT (SymTab.build [(KeyIdent (Ident "a")
                                                   , ExprTerm (TermLitNum (LitNum 3)))]) $
  expandTerm (TermIdent (Ident "a"))

  
expandTopLevels xs = mapM expandTopLevel xs


-- expandTopLevel :: ExpandErr TopLevel TopLevel
expandtoplevel (TopStmt stmt) = TopStmt <$> expandStmt stmt
expandTopLevel (TopMacro mac) = SymTab.mInsertMacro mac >> return (TopMacro mac)

-- expandStmt :: ExpandErr Stmt Stmt
expandStmt (StmtAssn assn) = StmtAssn <$> expandAssn assn -- this inserts an ident.
expandStmt (StmtExpr expr) = StmtExpr <$> expandExpr expr 
expandStmt (StmtProc proc) = return $ StmtProc proc
expandStmt (StmtCall call) = expandCall call

-- expandCall takes a macro call and expands it into statements with
-- as many identifiers bound as possible.  Some of the identifiers may
-- be labels, and if so then they must be left alone until the label pass.
-- expandCall :: ExpandErr Call Stmt
expandCall (Call ident exprs) =
  do let k = KeyMacro ident (length exprs)
     mac <- SymTab.mLookupMacro k
     case mac of
       Just mac' -> bindMacro exprs mac'
       Nothing -> throwE $ show ("Macro definition not found: " ++ show ident)

-- bindMacro :: [Expr] -> ExpandErr Macro Stmt
bindMacro exprs (Macro name args stmts) =
  -- flatten macro into many statements
  do when (length args /= length exprs) $
       throwE "(Improbable error) in Expand.bindMacro, argument length mismatch"
     curScope <- get
     -- bind exprs to argument idents and construct new symbol table
     let (SymTab ids macs _) = SymTab.build (zip (map KeyIdent args) exprs)
     -- push the new symbol table to give priority to argument
     -- identifiers.
     put (SymTab ids macs curScope)
     -- expand statments defined by the macro
     stmts <- StmtMany <$> mapM expandStmt stmts
     -- done expanding, restore current scope
     put curScope
     return $ stmts

expandAssn assn@(Assn ident expr) =
  do let k = KeyIdent ident
     if ident == CurInstruction          
       then return assn
       else do SymTab.mInsert k expr
               return assn

expandTerm :: ExpandErr Term Term
expandTerm term@(TermIdent ident) =
  do let k = KeyIdent ident
     if ident == CurInstruction          
       then return term
       else do expr <- SymTab.mLookup k
               case expr of                 
                 Just expr -> return $ TermExpr expr
                 Nothing -> do st <- get
                               throwError (show (expr))
                 

expandTerm (TermExpr expr) = TermExpr <$> expandExpr expr
expandTerm (TermNeg term) = TermNeg <$> expandTerm term
expandTerm term = return term

-- expandExpr :: (MonadState SymbolTable m, MonadError String m) => Expr -> m Expr
expandExpr :: ExpandErr Expr Expr
expandExpr (ExprNeg expr) = ExprNeg <$> expandExpr expr
expandExpr (ExprTerm term) = ExprTerm <$> expandTerm term
expandExpr (ExprTermExpr term exprs) =
  liftM2 ExprTermExpr (expandTerm term) (mapM expandExpr exprs)
expandExpr (ExprBinTail binop term) = ExprBinTail binop <$> expandTerm term


-- temp = do s <- evalStateT (expandTerm (TermLitNum (LitNum 3))) SymTab.new
--           return s
