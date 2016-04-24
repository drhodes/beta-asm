{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Uasm.LabelPass where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map as DM
import           Data.Functor.Identity
import Control.Monad.Identity
import           Uasm.Bind
import           Uasm.Pretty
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Text.PrettyPrint.Leijen as PP

{-
Macros have been expanded.  Label addresses are unknown because
expressions havn't been evaluated. Current address identifiers (.)
can be determined, but only if they don't share an expression with a
label that has yet to be encountered. 

When a label declaration is encountered, then it's value is known,
however, if a label string is encountered, such as in the call of BEQ,
then the label value may not yet be known, therefore, evaluation of
these labels is not yet possible in all cases.  Evaluation is only
possible for an expression if it does not contain undeclared labels,
at least for this pass.  The next pass, the eval pass will enjoy the
symbol table from this pass, so all label positions will be known at
that point.
---------------------------------

The value table contains 

-}

-- move this into Uasm.PlaceState or something.

type ValueTable = DM.Map Ident Value

data PlaceState = PlaceState { psCurAddr :: Integer
                             , psValueTable :: ValueTable
                             } deriving (Show, Eq)

psIncCurAddr :: MonadState PlaceState m => m ()
psIncCurAddr =
  do addr <- psGetCurAddr
     psSetCurAddr (addr + 4)
     return ()

psSetCurAddr :: MonadState PlaceState m => Integer -> m ()
psSetCurAddr n =
  do ps@(PlaceState _ _) <- get
     put ps{psCurAddr = n}
     return ()

psGetCurAddr :: MonadState PlaceState m => m Integer
psGetCurAddr =
  do (PlaceState addr _) <- get
     return addr

psGetValueTable :: MonadState PlaceState m => m ValueTable
psGetValueTable =
  do (PlaceState _ vt) <- get
     return vt
       
psInsertValue key val =
  do ps@(PlaceState _ table) <- get
     let table' = DM.insert key val table
     psIncCurAddr
     addr <- psGetCurAddr
     put ps{psValueTable = table'}
     return ()

psLookupVal name =
  do vt <- psGetValueTable
     return $ DM.lookup name vt

newPlaceState = PlaceState 0 DM.empty
------------------------------------------------------------------

type LabelPass b = forall m. ( MonadState PlaceState m,
                               MonadError String m ) => m b

runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

runLabelPass func node = runIdentity . runExceptStateT (newPlaceState) $ func node


--------------------------------------------
labelPassStmts stmts = mapM labelPassStmt stmts

labelPassStmt :: Stmt -> LabelPass Value
labelPassStmt (StmtProc p) = labelPassProc p
labelPassStmt (StmtCall call) = throwError $
  "labelPassStmt shouldn't handle any macro calls, \ 
  \they should have been flattened alreadu in Uasm.Expand"

labelPassStmt (StmtAssn assn) = labelPassAssn assn
labelPassStmt (StmtExpr expr) =
  do vt <- psGetValueTable
     if hasUnknown expr vt
       then do x <- eval expr
               psIncCurAddr
               return x
       else labelPassExpr expr
            
labelPassStmt (StmtMany _) = throwError $
  "label pass shouldn't handle StmtMany. \
  \StmtMany should have already been \
  \flattened out in Uasm.Expand.flattenStmt"

labelPassProc :: Proc -> LabelPass Value

labelPassProc (Label name) = 
  do addr <- psGetCurAddr
     psInsertValue name (ValNum addr)
     return $ ValProc (Label name)
     
labelPassAssn assn@(Assn CurInstruction expr) =
  -- try to eval expr, this might not work because expr might contain
  -- a label that has yet to be encountered, and if that's the case,
  -- then this should fail.
  -- NB. This only holds for assignments where (.) is the LHS.
  do valTab <- psGetValueTable
     if hasUnknown expr valTab
       then throwError "Unknown symbol"
       else do val <- eval expr
               case val of 
                 (ValNum addr) -> psSetCurAddr addr
                 x -> throwError $ "Can't assign program counter to: " ++ (show x)
               return $ ValNop

labelPassAssn assn@(Assn ident@(Ident name) expr) =               
  -- else the LHS is not (.), therefore it's okay for label
  -- identifiers found in expressions to delay evaluation, this is
  -- necessary to have BR(label) work.  Any such labels will be
  -- determined in the next pass.
  do v <- eval expr
     psInsertValue ident v
     return $ ValAssn assn

labelPassExpr :: Expr -> LabelPass Value       
labelPassExpr expr = eval expr

class HasUnknown a where
  hasUnknown :: a -> ValueTable -> Bool

instance HasUnknown a => HasUnknown [a] where
  hasUnknown xs vt = any (==True) [hasUnknown x vt | x <- xs] 

instance HasUnknown Expr where  
  hasUnknown (ExprNeg expr) vt = hasUnknown expr vt
  hasUnknown (ExprTerm term) vt = hasUnknown term vt
  hasUnknown (ExprTermExpr term exprs) vt = hasUnknown term vt || hasUnknown exprs vt
  hasUnknown (ExprBinTail binop term) vt = hasUnknown term vt

instance HasUnknown Term where  
  hasUnknown (TermIdent ident) vt = not $ DM.member ident vt
  hasUnknown (TermNeg term) vt = hasUnknown term vt
  hasUnknown (TermExpr expr) vt = hasUnknown expr vt
  hasUnknown (TermLitNum _) vt = False

------------------------------------------------------------------
-- eval 

class Eval a where
  eval :: a -> LabelPass Value

instance Eval Expr where
  eval (ExprNeg expr) = eval expr
  eval (ExprTerm term) = eval term
  eval (ExprTermExpr term []) = eval term
  eval (ExprTermExpr term exprs) = throwError ("Crap! " ++ (show exprs))
  eval (ExprBinTail binop term) = throwError ("Binop Crap! " ++ (show (binop, term)))

instance Eval Term where
  eval (TermIdent CurInstruction) = ValNum <$> psGetCurAddr
  eval (TermIdent ident) =
    do found <- psLookupVal ident
       case found of
         Just val -> return val
         Nothing -> throwError $ "Couldn't find symbol: " ++ (show ident)
                                
  eval (TermNeg term) = NegVal <$> eval term
  eval (TermExpr expr) = eval expr
  eval (TermLitNum (LitNum n)) = return $ ValNum n
