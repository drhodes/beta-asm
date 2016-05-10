{-# LANGUAGE FlexibleContexts #-}
module Beta.Err where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

infixr 0 ?
(?) action msg = action `catchError` (\str -> throwError (msg ++ "\n" ++ str))
