module Uasm.Eval where

import Uasm.Parser
import Uasm.Types

negateVal (ValNum n) = ValNum (-n)
netateVal (ValIdent n) = NegVal (ValIdent n)

opApply op x y =
  let f = case op of
        Addition -> (+)
        Subtract -> (-)
        Multiply -> (*)
        x -> error $ "Need to implement: " ++ (show x) ++ " for opApply in Eval"
  in f x y

opVal bop (ValNum x) (ValNum y) = ValNum (opApply bop x y)
opVal bop x y = Delayed bop x y

instance Eval Expr where  
  eval (ExprNeg expr) = negateVal (eval expr)
  eval (ExprTerm term) = eval term
  eval (ExprTermExpr term exprs) =
    if null exprs
    then eval term
    else case exprs of
      [ExprBinTail binop rest] -> opVal binop (eval term) (eval rest)
      _ -> error "Unhandled Eval expr"
      -- let msg = "(term, exprs): " ++ show (term, exprs)
      --    in error $ "eval (ExprTermExpr term exprs), " ++ msg
  eval (ExprBinTail binop term) = error "eval (ExprBinTail binop term)"

instance Eval Term where
  eval (TermIdent ident) = ValIdent ident
  eval (TermLitNum (LitNum n)) = ValNum n
  eval (TermNeg term) = negateVal (eval term)
  eval (TermExpr expr) = eval expr

