{-# LANGUAGE FlexibleInstances #-}
module Uasm.Bind where

import           Control.Monad
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types

-- 
class Bind a where
  bind :: a -> SymbolTable -> Either String (a, SymbolTable)
