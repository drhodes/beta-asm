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


storeMach var m = liftIO $ M.putMVar var m
takeMach var = liftIO $ M.takeMVar var 


ok x = W.text $ T.pack $ G.encodeJSON ("OK"::String, x)
err msg = W.text $ T.pack $ JSON.encode ("ERR"::String, msg)

withVar var f = do
  machine <- takeMach var
  case BM.doMach machine f of
    Right (rsp, next) -> do
      ok rsp
      storeMach var next
    Left msg -> do      
      err msg
      storeMach var machine
      
say s = liftIO $ putStrLn s

main = do
  print "ok"

  putStrLn "BETA server starting..."

  -- mutable state for machine.
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
        ; withVar m BM.step
        ; ok ("Stepping the CPU" :: String)
        }

    ; W.get "/state" $ do {
        ; let msg = "beta:  Retrieving CPU state"
        ; v <- takeMach m
        ; ok v
        ; storeMach m v
        }

    ; W.get "/assemble/:filename" $ do {
        ; fn <- W.param "filename"
        ; say $ "beta:  Assembling " ++ fn
        ; x <- liftIO $ BU.assemble fn
        ; W.text $ T.pack (show x)
        }

    ; W.post "/load-buffer" (postLoadBuffer m)
      
    ; W.get "/load-sample" $ do {
        ; withVar m BM.loadSample1
        }
    }

postLoadBuffer :: M.MVar Mach -> ActionT T.Text IO ()
postLoadBuffer m = do
  -- lost post data
  name <- W.param "buffer-name"
  say $ "beta: Loading buffer: " ++ name
  
  src <- W.param "buffer-src"
  say $ "beta: Loading src   : " ++ (take 40 src)

  -- assemble the buffer.
  words <- liftIO $ BU.assembleString src
  
  case words of
    Left msg -> err msg
    Right ws -> do
      takeMach m
      storeMach m (BM.fromWords ws)
      ok ("ASDFASDF has spoken" :: String)
