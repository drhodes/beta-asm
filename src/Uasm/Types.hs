module Uasm.Types where

import qualified Data.Map as DM

data SymbolTable = SymTab { symTabCurScope :: DM.Map SymbolKey Value
                          , symTabNextScope :: SymbolTable }
                 | NullTable
                 deriving (Show, Eq)

-- instance Show SymbolTable where
--   show NullTable = "<Null SymbolTable>"
--   show (SymTab cur next) = 

                          
data SymbolKey = KeyMacro Ident Int
               | KeyIdent Ident
                 deriving (Show, Eq, Ord)

data Value = ValNum Integer
           | ValMacro Macro
           | ValIdent Ident
           | ValExpr Expr
           | ValProc Proc
           | ValDotAssn Value
           | NegVal Value
           | Delayed Binop Value Value
             deriving (Show, Eq)

class Eval a where
  eval :: a -> SymbolTable -> Either String Value

data TopLevel = TopStmt Stmt
              | TopMacro Macro
                deriving (Show, Eq)

data Stmt = StmtProc Proc
          | StmtCall Call
          | StmtAssn Assn
          | StmtExpr Expr
            deriving (Show, Eq)

data Macro = Macro Ident [Ident] [Stmt] deriving (Show, Eq)

data Assn = Assn Ident Expr deriving (Show, Eq)

data Call = Call Ident [Expr] deriving (Show, Eq)

data Proc = DotInclude String
          | DotAlign Expr
          | DotAscii String
          | DotText String
          | DotBreakPoint
          | DotProtect
          | DotUnprotect
          | DotOptions
          | Label Ident
            deriving (Show, Eq)

data Binop = BitwiseComplement
           | BitwiseAnd
           | BitwiseOr
           | Addition
           | Subtract
           | Multiply
           | Division
           | Modulo
           | RightShift
           | LeftShift
             deriving (Show, Eq)

data Expr = ExprNeg Expr
          | ExprTerm Term
          | ExprTermExpr Term [Expr]
          | ExprBinTail Binop Term
            deriving (Show, Eq)
                     
data Term = TermIdent Ident
          | TermLitNum LitNum
          | TermNeg Term
          | TermExpr Expr
            deriving (Show, Eq)

data Ident = Ident String
           | CurInstruction
             deriving (Show, Eq, Ord)

data LitNum = LitNum Integer deriving (Show, Eq)

