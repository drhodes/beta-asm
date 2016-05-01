module Main where
import qualified Text.Parsec as TP
import           Uasm.Expand
import           Uasm.FinalPass
import           Uasm.LabelPass
import qualified Uasm.Parser as P
import           Uasm.Types

main :: IO ()
main = print "hi"

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

