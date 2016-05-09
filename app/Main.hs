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
import           Beta.Util

withVar var f = do
  machine <- liftIO $ M.takeMVar var
  case BM.doMach machine f of
    Right (rsp, next) -> do
      liftIO $ M.putMVar var next
      W.text $ T.pack (JSON.encode ("OK"::String, rsp))
    Left msg -> do
      liftIO $ M.putMVar var machine
      W.text $ T.pack (JSON.encode ("ERR"::String, msg))

say s = liftIO $ putStrLn s

main = do
  print "ok"

  putStrLn "BETA server starting..."

  m <- M.newMVar BM.new
  
  
  W.scotty 3000 $ do {
    --------------------------------------------
    ; W.get "/reset" $ do {
        ; say "beta:  Resetting the CPU"
        ; W.text "Resetting the CPU"
        ; withVar m BM.reset
        }

    ; W.get "/step" $ do {
        ; say "beta:  Stepping the CPU"
        ; W.text $ T.pack (JSON.encode ( "OK"::String
                                       , "Stepping the CPU"::String))
        ; withVar m BM.step
        }

    ; W.get "/state" $ do {
        ; let msg = "beta:  Retrieving CPU state"
        ; say msg
        ; withVar m BM.jsonState
        }

    ; W.get "/assemble/:filename" $ do {
        ; fn <- W.param "filename"
        ; say $ "beta:  Assembling " ++ fn
        ; x <- liftIO $ assemble fn
        ; W.text $ T.pack (show x)
        }

    ; W.post "/load" $ do {
        ; x <- param "program"
        ; W.text $ x
        }

    ; W.get "/load-sample" $ do {
        ; withVar m BM.loadSample1
        }
    }

