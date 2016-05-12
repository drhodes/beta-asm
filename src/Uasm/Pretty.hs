{-# LANGUAGE FlexibleInstances #-}
module Uasm.Pretty where

import Text.PrettyPrint.Leijen
import Uasm.Parser
import Uasm.Types
import qualified Data.Map as DM

instance Pretty Ident where
  pretty (Ident s) = text s
  pretty (CurInstruction) = text "."
          
instance Pretty Binop where
  pretty x = text $ case x of
    Multiply -> "*" 
    Subtract -> "-" 
    Addition -> "+" 
    Division -> "/" 
    RightShift -> ">>" 
    LeftShift -> "<<" 
    Modulo -> "%" 
    BitwiseComplement -> "~" 
    BitwiseAnd -> "&" 
    BitwiseOr -> "|" 

instance Pretty SymbolTable where
  pretty (SymTab exprs macs next) =
    text "(SymTab"
    <> (pretty exprs)
    <> (pretty macs)
    <> (pretty next)
    <> text ")"
  pretty NullTable = text "{}"

instance (Show a, Show b) => Pretty (DM.Map a b) where
  pretty m = text (show m)
                          
instance Pretty SymbolKey where
  pretty (KeyMacro name numArgs) = pretty name <> pretty numArgs
  pretty (KeyIdent name) = pretty name
  
instance Pretty TopLevel where
  pretty (TopStmt stmt _) = pretty stmt <> linebreak
  pretty (TopMacro mac _) = pretty mac <> linebreak

instance Pretty Stmt where
  pretty (StmtProc p) = pretty p
  pretty (StmtCall call) = pretty call
  pretty (StmtAssn assn) = pretty assn
  pretty (StmtExpr expr) = pretty expr
  pretty (StmtMany stmts) = vcat (map pretty stmts)

instance Pretty Macro where
  pretty (Macro name args stmts) =
    text ".macro" <+> pretty name
    <> tupled (map pretty args) <+> text "{"
    <$$> (vcat $ map pretty stmts)
    <$$> text "}"

instance Pretty Assn where
  pretty (Assn name expr) = pretty name <+> char '=' <+> pretty expr

instance Pretty Call where
  pretty (Call name args) = pretty name <> tupled (map pretty args)

instance Pretty Proc where
  pretty (DotInclude filename) = text ".include" <+> dquotes (text filename)
  pretty (DotAlign expr) = text ".align" <+> pretty expr
  pretty (DotAscii str) = text ".ascii" <+> dquotes (text str)
  pretty (DotText str) = text ".text" <+> dquotes (text str)
  pretty DotBreakPoint = text ".breakpoint"
  pretty DotProtect = text ".protect"
  pretty DotUnprotect = text ".unprotect"
  pretty (Label ident) = pretty ident <> char ':'

instance Pretty Expr where
  pretty (ExprNeg expr) = text "-" <> pretty expr
  pretty (ExprTerm term) = pretty term
  pretty (ExprTermExpr term []) = pretty term
  pretty (ExprTermExpr term exprs) = text "(" <> pretty term <+> hcat (map pretty exprs)
  pretty (ExprBinTail binop term) = pretty binop <+> pretty term <> text ")"

instance Pretty Term where    
  pretty (TermIdent name) = pretty name
  pretty (TermLitNum litnum) = pretty litnum
  pretty (TermNeg term) = char '-' <> pretty term
  pretty (TermExpr expr) = pretty expr

instance Pretty LitNum where
  pretty (LitNum n) = pretty n
