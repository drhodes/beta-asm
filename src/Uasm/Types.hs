{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Uasm.Types where

import qualified Data.Map as DM
import qualified Data.Word as DW
import           Text.Parsec

data SymbolTable = SymTab { symTabCurScope :: DM.Map SymbolKey Expr
                          , symTabCurMacroScope :: DM.Map SymbolKey Macro
                          , symTabNextScope :: SymbolTable }
                 | NullTable
                 deriving (Show, Eq)
                          
data SymbolKey = KeyMacro Ident Int
               | KeyIdent Ident
                 deriving (Show, Eq, Ord)

type Address = Integer
                          
data Value = ValNum Integer
           | ValMacro Macro
           | ValIdent Ident
           | ValExpr Expr
           | ValTerm Term
           | ValProc Proc
           | ValDotAssn Value
           | ValAssn Assn
           | NegVal Value
           | ValNop
           | ValSeq [Value]
           | Delayed Binop Value Value
             deriving (Show, Eq, Ord)

data TopLevel = TopStmt Stmt SourcePos
              | TopMacro Macro SourcePos
                deriving (Show, Eq, Ord)

data Stmt = StmtProc Proc
          | StmtCall Call
          | StmtAssn Assn
          | StmtExpr Expr
          | StmtMany [Stmt]
          deriving (Show, Eq, Ord)

data Macro = Macro Ident [Ident] [Stmt] deriving (Show, Eq, Ord)

data Addr = Addr Integer deriving (Show, Eq, Ord)

data Assn = Assn Ident Expr deriving (Show, Eq, Ord)

data Call = Call Ident [Expr] deriving (Show, Eq, Ord)

data PlacedByte = PlacedByte Addr DW.Word8

data Proc = DotInclude String
          | DotAlign Expr
          | DotAscii String
          | DotText String
          | DotBreakPoint
          | DotProtect
          | DotUnprotect
          | DotOptions
          | Label Ident
            deriving (Show, Eq, Ord)

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
             deriving (Show, Eq, Ord)

data Expr = ExprNeg Expr
          | ExprTerm Term
          | ExprTermExpr Term [Expr]
          | ExprBinTail Binop Term
            deriving (Show, Eq, Ord)
                     
data Term = TermIdent Ident
          | TermLitNum LitNum
          | TermNeg Term
          | TermExpr Expr
            deriving (Show, Eq, Ord)

data Ident = Ident String
           | CurInstruction
           | PlacedInstruction Integer
             deriving (Show, Eq, Ord)

data LitNum = LitNum Integer deriving (Show, Eq, Ord)


