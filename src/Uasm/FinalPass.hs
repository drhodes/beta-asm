{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Uasm.FinalPass where

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
import qualified Data.Bits as DB
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Functor.Identity
import qualified Text.PrettyPrint.Leijen as PP

{-

This is the final pass
---------------------------------

The value table contains 
-}

-- move this into Uasm.PlaceState or something.

------------------------------------------------------------------

type FinalPass b = forall m. ( MonadState PlaceState m,
                               MonadError String m ) => m b

runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

runFinalPass func node = runIdentity . runExceptStateT (newPlaceState) $ func node

--------------------------------------------
finalPassStmts stmts = mapM finalPassStmt stmts

finalPassStmt :: Stmt -> FinalPass Value
finalPassStmt (StmtProc p) = finalPassProc p
finalPassStmt (StmtCall call) = throwError $
  "finalPassStmt shouldn't handle any macro calls, \ 
  \they should have been flattened alreadu in Uasm.Expand"

finalPassStmt (StmtAssn assn) = finalPassAssn assn
finalPassStmt (StmtExpr expr) =
  do x <- eval expr
     psIncCurAddr
     return x
            
finalPassStmt (StmtMany _) = throwError $
  "label pass shouldn't handle StmtMany. \
  \StmtMany should have already been \
  \flattened out in Uasm.Expand.flattenStmt"

finalPassProc :: Proc -> FinalPass Value

finalPassProc lbl@(Label name) = 
  do addr <- psGetCurAddr
     psInsertValue name (ValNum addr)     
     return $ ValProc lbl

finalPassProc (DotAlign expr) =
  do addr <- psGetCurAddr
     val <- eval expr
     case val of
       ValNum n -> if addr `mod` n == 0
                   then return ValNop
                   else let padLen = fromIntegral $ n - ((addr + n) `mod` n)
                        in return $ ValSeq (take padLen $ repeat (ValNum 0))
       _ -> throwError $ "Couldn't evaluate expression in a .align: " ++ (show expr)
     
finalPassAssn assn@(Assn CurInstruction expr) =
  -- try to eval expr, this might not work because expr might contain
  -- a label that has yet to be encountered, and if that's the case,
  -- then this should fail.
  -- NB. This only holds for assignments where (.) is the LHS.
  do valTab <- psGetValueTable
     curAddr <- psGetCurAddr
     if hasUnknown expr valTab
       then throwError $ "Unknown symbol: " ++ (show valTab)
       else do val <- eval expr
               case val of 
                 (ValNum addr) ->
                   if addr > curAddr
                   then do psSetCurAddr addr
                           return $ let numZeros = (fromIntegral $ addr - curAddr)
                                    in ValSeq $ take numZeros (repeat (ValNum 0))
                   else throwError $ "Can't move current instruction backwards"

                 x -> throwError $ "Can't assign program counter to: " ++ (show x)

finalPassAssn assn@(Assn ident@(Ident name) expr) =               
  -- else the LHS is not (.), therefore it's okay for label
  -- identifiers found in expressions to delay evaluation, this is
  -- necessary to have BR(label) work.  Any such labels will be
  -- determined in the next pass.
  do v <- eval expr
     psInsertValue ident v
     valTab <- psGetValueTable     
     return $ if hasUnknown expr valTab
              then ValAssn assn
              else ValNop

finalPassExpr :: Expr -> FinalPass Value       
finalPassExpr expr = eval expr

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
  hasUnknown (TermIdent CurInstruction) _ = False
  hasUnknown (TermIdent ident) vt = not $ DM.member ident vt
  hasUnknown (TermNeg term) vt = hasUnknown term vt
  hasUnknown (TermExpr expr) vt = hasUnknown expr vt
  hasUnknown (TermLitNum _) _ = False

------------------------------------------------------------------
-- eval 

opApply :: Binop -> Integer -> Integer -> FinalPass Value
opApply op x y =
  let f = case op of
        Addition -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Modulo -> mod
        RightShift -> let f' x' y' = DB.shiftR (fromIntegral x') (fromIntegral y')
                      in f'
        LeftShift -> let f' x' y' = DB.shiftL (fromIntegral x') (fromIntegral y')
                     in f'
        z -> error $ "Need to implement: " ++ (show z) ++ " for opApply in FinalPass"
  in return $ ValNum (f x y)

opVal bop (ValNum x) (ValNum y) = opApply bop x y
opVal bop x y = return $ Delayed bop x y


class Eval a where
  eval :: a -> FinalPass Value

negative (ValNum n) = ValNum (-n)
negative x = NegVal x

instance Eval Expr where
  eval (ExprNeg expr) = negative <$> eval expr
  eval (ExprTerm term) = eval term
  eval (ExprTermExpr term []) = eval term
  eval (ExprTermExpr term exprs) = 
    case exprs of      
      [ExprBinTail binop rest] -> do v1 <- (eval term)
                                     v2 <- (eval rest)
                                     opVal binop v1 v2
      ((ExprBinTail binop rest):xs) ->
        do v1 <- (eval term)
           v2 <- (eval rest)
           val <- opVal binop v1 v2
           case val of
             (ValNum n) -> let term' = TermLitNum (LitNum n)
                           in eval (ExprTermExpr term' xs)
             _ -> throwError $ "Weird Val in eval: " ++ (show val)
      casex -> throwError $ "Unhandled Eval expr: " ++ (show casex)

  eval (ExprBinTail binop term) = throwError ("Binop Crap! " ++ (show (binop, term)))

instance Eval Term where
  eval (TermIdent CurInstruction) = ValNum <$> psGetCurAddr
  eval (TermIdent ident) =
    do found <- psLookupVal ident
       case found of
         Just val -> return val
         Nothing -> return $ ValIdent ident
                                
  eval (TermNeg term) = NegVal <$> eval term
  eval (TermExpr expr) = eval expr
  eval (TermLitNum (LitNum n)) = return $ ValNum n
