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
import           Uasm.Pretty
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types
import qualified Data.Bits as DB
import Control.Monad.Except
import qualified Text.PrettyPrint.Leijen as PP
import           Text.Parsec

{-
This is the final pass

The remaining artifact at this point is a list of values, some of
which have delayed evaluations containing unknown identifiers.  Those
identifiers should be undetermined labels from LabelPass.  If the
unknown identifiers aren't labels, then they are either forward
symbols, which are not supported, or typos.  In either of the latter
cases an error needs to be raised to alert the user.

This pass will replace the forward labels with concrete byte addresses
then eval the delayed expressions.
-}

type LabelMap = DM.Map Ident Addr

type FinalPass b = forall m. ( MonadState LabelMap m,
                               MonadError String m ) => m b
                   
runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

runFinalPass :: [(Value, SourcePos)] -> Either [Char] [(Value, SourcePos)]
runFinalPass vals = do
  -- flatten sequences and remove the nops and
  let flatVals = filter (not . isNop) $ flattenVals vals :: [(Value, SourcePos)]
  (vs, _) <- runIdentity . runExceptStateT DM.empty $
    finalPass $ ((map fst flatVals) :: [Value])
  return $ zip (filter (/=ValNop) vs) (map snd flatVals)

lookupAddr :: Ident -> FinalPass (Maybe Addr)
lookupAddr name = DM.lookup name <$> get 

isLabel (ValProc (Label _)) = True
isLabel _ = False

flattenVals :: [(Value, SourcePos)] -> [(Value, SourcePos)]
flattenVals [] = []
flattenVals ((ValSeq xs, pos) : rest) = flattenVals $ [(x, pos) | x <- xs] ++ rest
flattenVals (x:xs) = x : flattenVals xs

identOfLabel (ValProc (Label name)) = name

isNop (ValNop, _) = True
isNop (_, _) = False

finalPass :: [Value] -> FinalPass [Value]
finalPass flatVals = 
  do 
    let -- associate values with their byte addresses
        placedBytes = zip flatVals (map Addr [0..])
        -- pickout the labels with their byte addreses
        placedLabels = filter (isLabel . fst) placedBytes
        -- create a lookup table for labels and their byte addresses
        labelMap = DM.fromList [(identOfLabel lbl, idx) | (lbl, idx) <-  placedLabels]
    
    put labelMap
    
    -- replace the unknown identifiers with values.
    knowns <- mapM replace flatVals
    
    -- evaluate the remaining values.
    mapM eval knowns

--------------------------------------------

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
        z -> error $ "Need to implement: " ++ show z ++ " for opApply in FinalPass"
  in return $ ValNum (f x y)

opVal bop (ValNum x) (ValNum y) = opApply bop x y
opVal bop x y = return $ Delayed bop x y

class Eval a where
  eval :: a -> FinalPass Value

negative (ValNum n) = ValNum (-n)
negative x = NegVal x

instance Eval Value where
  eval (ValExpr expr) = eval expr
  eval (ValTerm term) = eval term
  -- eval (ValAssn assn) = eval assn
  eval (NegVal value) = liftM negative $ eval value
  eval (Delayed binop value1 value2) = do v1 <- eval value1
                                          v2 <- eval value2
                                          opVal binop v1 v2
  eval val@(ValNum _) = return val
  -- ValProc, this needs to be handled ... 
  eval (ValProc _) = return ValNop
  eval x = return x

instance Eval Expr where
  eval (ExprNeg expr) = negative <$> eval expr
  eval (ExprTerm term) = eval term
  eval (ExprTermExpr term []) = eval term
  eval (ExprTermExpr term exprs) = 
    case exprs of      
      [ExprBinTail binop rest] -> do v1 <- eval term
                                     v2 <- eval rest
                                     opVal binop v1 v2
      (ExprBinTail binop rest : xs) ->
        do v1 <- eval term
           v2 <- eval rest
           val <- opVal binop v1 v2
           case val of
             (ValNum n) -> let term' = TermLitNum (LitNum n)
                           in eval (ExprTermExpr term' xs)
             _ -> throwError $ "FinalPass.eval: unexpected val: " ++ show val
      casex -> throwError $ "Unhandled Eval expr: " ++ show casex

  eval (ExprBinTail binop term) = throwError ("Binop Crap! " ++ show (binop, term))

instance Eval Term where
  eval (TermNeg term) = negative <$> eval term
  eval (TermExpr expr) = eval expr
  eval (TermLitNum (LitNum n)) = return $ ValNum n

--------------------------------------------
class Replace a where
  replace :: a -> FinalPass a

instance Replace Value where
  replace val@(ValIdent ident) = ValNum <$> lookupLabel ident
  replace (ValExpr expr) = ValExpr <$> replace expr
  replace (ValTerm term) = ValTerm <$> replace term
  replace (ValAssn assn) = ValAssn <$> replace assn
  replace (NegVal value) = NegVal <$> replace value
  replace (Delayed binop value1 value2) =
    liftM2 (Delayed binop) (replace value1) (replace value2)
  replace val@(ValNum _) = return val
  -- ValProc, this needs to be handled for dealing with simulator flags.
  replace (ValProc _) = return ValNop
  replace x = return x
  -- replace x = throwError $ "Unhandled: " ++ (show x)
instance Replace Assn where
  replace (Assn ident expr) = Assn ident <$> replace expr

instance Replace Expr where
  replace (ExprNeg expr) = ExprNeg <$> replace expr
  replace (ExprTerm term) = ExprTerm <$> replace term
  replace (ExprTermExpr term exprs) =
    liftM2 ExprTermExpr (replace term) (mapM replace exprs)
  replace (ExprBinTail binop term) = ExprBinTail binop <$> replace term

instance Replace Term where
  replace (TermIdent CurInstruction) = throwError "Should not have reached this code"
  replace (TermIdent ident) = 
    do addr <- lookupAddr ident
       case addr of
         Just (Addr n) -> return $ TermLitNum (LitNum n)
         Nothing -> throwError $ "Couldn't find symbol: " ++ show ident
  replace (TermNeg term) = TermNeg <$> replace term
  replace (TermExpr expr) = TermExpr <$> replace expr
  replace x = return x

lookupLabel ident = 
    do addr <- lookupAddr ident
       case addr of
         Just (Addr n) -> return n
         Nothing -> throwError ("Couldn't find label: " ++ show ident)
