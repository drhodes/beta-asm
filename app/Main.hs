{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Monad.IO.Class
import qualified Text.Parsec as TP
import           Uasm.Expand
import           Uasm.FinalPass
import           Uasm.LabelPass
import qualified Uasm.Parser as P
import           Uasm.Types
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as WS
import           Data.Bits

main = do
  print "ok"
  putStrLn "BETA server starting..."
  WS.scotty 3000 $ do
    -- reset the CPU
    WS.get "/reset" $ do
      liftIO $ putStrLn "Resetting the CPU"
      WS.text "Resetting the CPU"

    WS.get "/assemble/:filename" $ do
      fn <- WS.param "filename"
      x <- liftIO $ assemble fn
      WS.text $ T.pack (show x)

padBytes bs = if length bs `mod` 4 == 0
              then bs
              else padBytes $ bs ++ [0]
                   
groupBytes [] = []
groupBytes bs = (littlize $ take 4 bs) : groupBytes (drop 4 bs)

littlize [d, c, b, a] =
  let a' = shiftL a 24
      b' = shiftL b 16
      c' = shiftL c 8
      d' = shiftL d 0
  in d' .|. c' .|. b' .|. a'

toBinary vals =
  let bytes = padBytes [n | (ValNum n) <- vals]
  in groupBytes bytes

assemble fname =
  do beta <- readFile "test/uasm/beta.uasm"
     prog <- readFile $ "test/uasm/" ++ fname
     let src = P.eraseComments $ beta ++ "\n" ++ prog
     let result = doFinalPass src
     return result

doFinalPass src = 
  do labelPassResult <- doLabelPass src
     case labelPassResult of
       (Right (vals, _)) -> runFinalPass vals
       (Left msg) -> error msg

doLabelPass :: Monad m => String -> m (Either String ([Value], PlaceState))
doLabelPass prog = do  
  case TP.parse (TP.many P.topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, _)) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels) 
         (Left msg) -> error msg
    (Left msg) -> error (show msg)

