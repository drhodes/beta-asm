import           Uasm.Parser
import           Uasm.Eval

import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

testEval :: [Char] -> Int -> String -> Value -> IO ()
testEval setName tid src val =
  case TP.parse expr2 "" src of
    Left msg -> error (setName ++ "\n" ++ (show msg))
    Right v -> if val == (eval v)
               then print ("Good", src, eval v)
               else error ("error in " ++ (show (setName, tid, src)))

testExprBin = do    
  let f = testEval "testExprBin" 
  f 1 "1" (ValNum 1)
  f 2 "1+2" (ValNum 3)

