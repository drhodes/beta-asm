module TestFixDot where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import           Uasm.LabelPass
import qualified Uasm.SymbolTable as SymTab
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

assigns = testGroup "Dot Assigs"
  [ testCase "assn1" $ fixDotTest ". = . + 4 \n .+." [ValNum 8]
  -- , testCase "assn2" $ fixDotTest ". = . + 4 \n myLabel: \n .+myLabel" [ValNum 8]
  ] 

fixDotTest :: String -> [Value] -> IO ()
fixDotTest str expected = do
  let ast = TP.parse ((TP.many topLevel) <* TP.eof) "" str
  case ast of
    (Right val) ->
      case expand val (SymTab.new) of
        (Right v, _) ->
          let r = fixDots v 0 SymTab.new
          in if expected == r
             then return ()
             else error $ show ("Expected", expected, "GOT", fixDots v 0 SymTab.new)
        (Left msg, _) -> error $ "Can't expand: " ++ (show msg) ++ "\n\n" ++ (show str)
    (Left msg) -> error $ (show msg) ++ "\n\n" ++ (show str)


  
