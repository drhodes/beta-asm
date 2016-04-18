module Uasm.Eval where

import Uasm.Parser

data Value = ValNum Integer
           | ValIdent Ident
           | NegVal Value
           | Delayed Binop 
             deriving (Show, Eq)

class Eval a where
  eval :: a -> Value

negateVal (ValNum n) = ValNum (-n)
netateVal (ValIdent n) = NegVal (ValIdent n)

valAdd (ValNum x) (ValNum y) = ValNum (x+y)

instance Eval Expr where  
  eval (ExprNeg expr) = negateVal (eval expr)
  eval (ExprTerm term) = eval term
  eval (ExprTermExpr term exprs) =
    if null exprs
    then eval term
    else case exprs of
      [ExprBinTail binop rest] ->
        case binop of
          Addition -> valAdd (eval term) (eval rest)
      _ -> error "Unhandled Eval expr"
      -- let msg = "(term, exprs): " ++ show (term, exprs)
      --    in error $ "eval (ExprTermExpr term exprs), " ++ msg
  eval (ExprBinTail binop term) = error "eval (ExprBinTail binop term)"

instance Eval Term where
  eval (TermIdent ident) = ValIdent ident
  eval (TermLitNum (LitNum n)) = ValNum n
  eval (TermNeg term) = negateVal (eval term)
  eval (TermExpr expr) = eval expr





