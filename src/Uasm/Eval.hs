module Uasm.Eval where

import qualified Uasm.SymbolTable as SymTab

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
  eval (ExprNeg expr) st = negateVal (eval expr st)
  eval (ExprTerm term) st = eval term st
  eval (ExprTermExpr term exprs) st =
    if null exprs
    then eval term st
    else case exprs of
      [ExprBinTail binop rest] -> opVal binop (eval term st) (eval rest st)
      _ -> error "Unhandled Eval expr"
  eval (ExprBinTail binop term) st = error "eval (ExprBinTail binop term)"

instance Eval Term where
  eval (TermIdent ident) st =
    case SymTab.lookup (KeyIdent ident) st of
      Just val -> val
      Nothing -> error $ "Can't find identifier: " ++ show ident ++ (show st)
  eval (TermLitNum (LitNum n)) st = ValNum n
  eval (TermNeg term) st = negateVal (eval term st)
  eval (TermExpr expr) st = eval expr st

