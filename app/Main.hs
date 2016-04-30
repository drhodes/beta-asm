module Main where

import Uasm.Expand
import Uasm.FinalPass
import Uasm.LabelPass
import Uasm.Parser

main :: IO ()
main = print "hi"


assemble prog = do
  


testFileWithBeta fname expect =
  do beta <- readFile "test/uasm/beta.uasm"
     prog <- readFile $ "test/uasm/" ++ fname
     testFinalPass (eraseComments (beta ++ "\n" ++ prog)) expect
     
testFinalPass :: String -> [Value] -> IO ()
testFinalPass prog expect = 
  do labelPassResult <- doLabelPass prog
     case labelPassResult of
       (Right (vals, placeState)) ->
         case runFinalPass vals of
           Right result ->
             unless (expect == result) $
               do let trunc x = putStrLn $ take 2000 $ show x
                  putStrLn "\nFail"
                  trunc  prog
                  putStrLn "\nExpected"
                  trunc expect
                  putStrLn "\nGot"
                  trunc result
                  putStrLn "\nLabelPass"
                  trunc vals
                  error "test fails"
           Left msg -> error msg
       (Left msg) -> error msg

doLabelPass :: Monad m => String -> m (Either String ([Value], PlaceState))
doLabelPass prog = do  
  case TP.parse (TP.many topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, symtab)) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels) 
         (Left msg) -> error msg
    (Left msg) -> error (show msg)

