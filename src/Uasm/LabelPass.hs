{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Uasm.LabelPass where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Bits as DB
import qualified Data.Map as DM
import qualified Text.PrettyPrint.Leijen as PP
import Uasm.Pretty
import           Uasm.Types
import qualified Data.Char as DC
import           Text.Parsec

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

type ValueTable = DM.Map Ident Value

data PlaceState = PlaceState { psCurAddr :: Integer
                             , psValueTable :: ValueTable
                             } deriving (Show, Eq)

displayTableKeys :: DM.Map Ident a -> String
displayTableKeys table = show $ [x | (Ident x) <- DM.keys table]

psIncCurAddr :: MonadState PlaceState m => m ()
psIncCurAddr =
  do addr <- psGetCurAddr
     psSetCurAddr (addr + 1)
     return ()

psIncCurAddrN n =
  do addr <- psGetCurAddr
     psSetCurAddr (addr + (fromIntegral n))
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

runLabelPass :: (t -> StateT PlaceState (ExceptT e Identity) a)
             -> t
             -> Either e (a, PlaceState)
runLabelPass func node = runIdentity . runExceptStateT (newPlaceState) $ func node

-- uniLabelPass :: [(Pos, Stmt)] -> Either String [(Pos, Value)]

uniLabelPass :: [(Stmt, SourcePos)] -> Either String [(Value, SourcePos)]
uniLabelPass nodes = do (x, _) <- runLabelPass labelPassStmts nodes
                        return x

withPos f p = do x <- f
                 return (x, p)

--------------------------------------------
labelPassStmts :: [(Stmt, SourcePos)] -> LabelPass [(Value, SourcePos)]
labelPassStmts stmts = mapM labelPassStmt stmts

labelPassStmt :: (Stmt, SourcePos) -> LabelPass (Value, SourcePos)
labelPassStmt (StmtProc p, pos) = withPos (labelPassProc p) pos
labelPassStmt (StmtCall _, pos) = throwError $
  "labelPassStmt shouldn't handle any macro calls, \ 
  \they should have been flattened alreadu in Uasm.Expand"

labelPassStmt (StmtAssn assn, pos) = withPos (labelPassAssn assn) pos
labelPassStmt (StmtExpr expr, pos) =
  do x <- eval expr
     psIncCurAddr
     return (x, pos)
            
labelPassStmt (StmtMany _ , pos) = throwError $
  "label pass shouldn't handle StmtMany. \
  \StmtMany should have already been \
  \flattened out in Uasm.Expand.flattenStmt"

labelPassProc :: Proc -> LabelPass Value

labelPassProc lbl@(Label name) = 
  do addr <- psGetCurAddr
     psInsertValue name (ValNum addr)     
     return $ ValProc lbl

labelPassProc (DotAscii txt) =
  do psIncCurAddrN (length txt)
     return $ ValSeq $ map (ValNum . fromIntegral . DC.ord) txt


-- Like .ascii except an additional 0 byte is added to the end of the
-- string in memory and the next byte assembled will be word-aligned.
labelPassProc (DotText txt) = 
  do (ValSeq ys) <- labelPassProc (DotAscii txt)
     (ValSeq xs) <- align 4
     let nullByte = ValNum 0
     return $ ValSeq (ys ++ [nullByte] ++ xs)


labelPassProc (DotAlign expr) =
  do addr <- psGetCurAddr
     val <- eval expr
     case val of
       ValNum n -> if addr `mod` n == 0
                   then return ValNop
                   else do let padLen = fromIntegral $ n - ((addr + n) `mod` n)
                           replicateM_ padLen psIncCurAddr
                           return $ ValSeq (take padLen $ repeat (ValNum 0))
       _ -> throwError $ "Couldn't evaluate expression in a .align: " ++ (show expr)

align n =
  do addr <- psGetCurAddr
     if addr `mod` n == 0
       then return $ ValSeq []
       else do let padLen = fromIntegral $ n - ((addr + n) `mod` n)
               replicateM_ padLen psIncCurAddr
               return $ ValSeq (take padLen $ repeat (ValNum 0))     
     
labelPassAssn (Assn CurInstruction expr) =
  -- try to eval expr, this might not work because expr might contain
  -- a label that has yet to be encountered, and if that's the case,
  -- then this should fail.
  -- NB. This only holds for assignments where (.) is the LHS.
  do valTab <- psGetValueTable
     curAddr <- psGetCurAddr
     if hasUnknown expr valTab
       then throwError $ "Unknown symbol found in: "
            ++ (show $ PP.pretty expr)
            ++ "\n"
            ++ (displayTableKeys valTab)
       else do val <- eval expr
               case val of 
                 (ValNum addr) ->
                   if addr > curAddr
                   then do psSetCurAddr addr
                           return $ let numZeros = (fromIntegral $ addr - curAddr)
                                    in ValSeq $ take numZeros (repeat (ValNum 0))
                   else throwError $ "Can't move current instruction backwards"

                 x -> throwError $ "Can't assign program counter to: " ++ (show x)

labelPassAssn assn@(Assn ident@(Ident _) expr) =               
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
  hasUnknown (ExprBinTail _ term) vt = hasUnknown term vt

instance HasUnknown Term where  
  hasUnknown (TermIdent CurInstruction) _ = False
  hasUnknown (TermIdent ident) vt = not $ DM.member ident vt
  hasUnknown (TermNeg term) vt = hasUnknown term vt
  hasUnknown (TermExpr expr) vt = hasUnknown expr vt
  hasUnknown (TermLitNum _) _ = False

------------------------------------------------------------------
-- eval 

opApply :: Binop -> Integer -> Integer -> LabelPass Value
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
        z -> error $ "Need to implement: " ++ (show z) ++ " for opApply in LabelPass"
  in return $ ValNum (f x y)

opVal bop (ValNum x) (ValNum y) = opApply bop x y
opVal bop x y = return $ Delayed bop x y


class Eval a where
  eval :: a -> LabelPass Value

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
             x -> return x
             --throwError $ "LabelPass.eval: unexpected Val in eval: " ++ (show val)
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
