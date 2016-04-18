module Uasm.Macro where

import Uasm.Parser
import Uasm.SymbolTable


expandTopLevels (TopStmt s) symtab = [s]
expandTopLevels (TopMacro m) symtab = expandMacro m


expandMacro :: Macro -> SymbolTable -> [Stmt]
expandMacro (Macro name args stmts) symtab = undefined
  

-- First thing after parsing, expand the macros into stmts.
