{-# LANGUAGE FlexibleContexts #-}
module Beta.Util where

import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Text.Parsec as TP
import           Uasm.Expand
import           Uasm.FinalPass
import           Uasm.LabelPass
import qualified Uasm.Parser as P
import           Uasm.Types
import qualified Data.Text.Lazy as T
import           Data.Bits
import qualified Beta.Mach as BM
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
import           Beta.Err


padBytes bs = if length bs `mod` 4 == 0
              then bs
              else padBytes $ bs ++ [0]
                   
groupBytes [] = []
groupBytes bs = toLittleEndian (take 4 bs) : groupBytes (drop 4 bs)

toLittleEndian [d, c, b, a] =
  shiftL a 24 .|. shiftL b 16 .|. shiftL c 08 .|. shiftL d 00 
     
toLittleEndian xs =
  error $ "toLittleEndian called with wrong args: " ++ show xs

toBinary vals =
  let bytes = padBytes [fromIntegral n :: Word32 | (ValNum n) <- vals]
  in groupBytes bytes

assemble fname =
  do beta <- readFile "test/uasm/beta.uasm"
     prog <- readFile $ "test/uasm/" ++ fname
     let src = P.eraseComments $ beta ++ "\n" ++ prog
     let result = doFinalPass src
     return result

assembleString :: String -> IO (Either String [Value])
assembleString s = do
  beta <- readFile "test/uasm/beta.uasm"
  let src = P.eraseComments $ beta ++ "\n" ++ s
  return $ doFinalPass src

doFinalPass src = 
  do labelPassResult <- doLabelPass src
     case labelPassResult of
       (Right (vals, _)) -> runFinalPass vals
       (Left msg) -> error msg

doLabelPass :: Monad m => String -> m (Either String ([Value], PlaceState))
doLabelPass prog = 
  case TP.parse (TP.many P.topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, _)) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels) 
         (Left msg) -> error msg
    (Left msg) -> error (show msg)
