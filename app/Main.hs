{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Text.Parsec as TP
import           Uasm.Expand
import           Uasm.FinalPass
import           Uasm.LabelPass
import qualified Uasm.Parser as P
import           Uasm.Types
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as W
import           Data.Bits
import qualified Beta.Mach as BM
import           Web.Scotty.Trans
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Beta.Types
import qualified Control.Concurrent.MVar as M
import qualified Text.JSON as JSON
import qualified Text.JSON.Generic as G
import           Data.Word
import qualified Beta.Util as BU
import System.Environment
import Numeric

main :: IO ()
main = do
  args <- getArgs
  print args
  src <- readFile (head args)
  bytes <- BU.assembleString src

  case bytes of
    Left msg -> error msg
    Right ws -> do
      -- mapM_ (\(n, _) -> putStrLn (showHex n ""))  ws
      let m = BM.fromWordPos ws
      case BM.doMach m BM.runAndGetR0 of
        Left msg -> error msg
        Right (r0, m') -> do
          print r0
