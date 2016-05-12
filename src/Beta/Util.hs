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
import qualified Text.Parsec.Pos as Pos

zeroPos = Pos.newPos "" 0 0 



padBytes bs = if length bs `mod` 4 == 0
              then bs
              else padBytes $ bs ++ [(0, zeroPos)]
                   
groupBytes [] = []
groupBytes bs = toLittleEndian (take 4 bs) : groupBytes (drop 4 bs)

toLittleEndian [(d, _), (c, _), (b, _), (a, p1)] =
  (shiftL a 24 .|. shiftL b 16 .|. shiftL c 08 .|. shiftL d 00, p1)
     
toLittleEndian xs =
  error $ "toLittleEndian called with wrong args: " ++ show xs

toBinary :: [(Value, TP.SourcePos)] -> [(Word32, TP.SourcePos)]
toBinary vals =
  let bytes = padBytes [(fromIntegral n :: Word32, p) | (ValNum n, p) <- vals]
  in groupBytes bytes

assemble :: [Char] -> IO (Either [Char] [(Value, TP.SourcePos)])
assemble fname =
  do beta <- readFile "test/uasm/beta.uasm"
     prog <- readFile $ "test/uasm/" ++ fname
     let src = P.eraseComments $ beta ++ "\n" ++ prog
     let result = doFinalPass src
     return result

assembleString s = do
  beta <- readFile "test/uasm/beta.uasm"
  let src = P.eraseComments $ beta ++ "\n" ++ s
  return $ liftM toBinary $ doFinalPass src

doFinalPass :: String -> Either [Char] [(Value, TP.SourcePos)]
doFinalPass src = 
  do labelPassResult <- doLabelPass src
     case labelPassResult of
       (Right (vals, _)) -> runFinalPass vals
       (Left msg) -> error msg

-- doLabelPass :: Monad m => String -> m (Either String ([Value], PlaceState))
doLabelPass prog = 
  case TP.parse (TP.many P.topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, _)) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels)
         (Left msg) -> error msg
    (Left msg) -> error (show msg)
