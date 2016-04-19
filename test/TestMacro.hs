module TestMacro where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Macro
import qualified Uasm.SymbolTable as SymTab
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"

testAll = testGroup "Macro Tests"
  [ testCase "testSomething1" $ testSomething1
  , testCase "testSomething2" $ testSomething2
  , testCase "testSomething4" $ testSomething4
  ] 

testSomething1 :: IO ()
testSomething1 = do
  let prog = "a=1+1 .macro Add(x){x} Add(asdf) asdf:"
      expect = [ ValExpr (ExprTermExpr (TermIdent (Ident "asdf")) [])
               , ValProc (Label (Ident "asdf"))]
  testMacro prog expect

testSomething2 :: IO ()
testSomething2 = do
  testMacro "a=1+1 b=a+1 c=b+1 c+1" [ValNum 5]
  
testSomething4 :: IO ()
testSomething4 = do
  let prog = concat [ ".macro Add(x, y) {x + y}"
                    , "Add(1,2)"
                    ]
  testMacro prog [ValNum 3]


testMacro :: String -> [Value] -> IO ()
testMacro prog val = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right ast) ->
      let (result, _) = expand ast SymTab.new
      in if result == Right val
         then return () --print ("Pass", prog, val)
         else error $ show ("Fail", prog, result, val)
    (Left msg) -> error (show msg)
  
