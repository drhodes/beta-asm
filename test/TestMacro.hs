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
  , testCase "testDot1" $ testExpand "." [ValIdent CurInstruction]
    
  , testCase "testDot2" $ testExpand ". ." [ ValIdent CurInstruction
                                          , ValIdent CurInstruction]
  -- , testCase "testSomething4" $ testSomething4
  ] 


testLabels = testGroup "Label Tests"
  [ testCase "label1" $ testExpand "0x1 \n myLabel: \n 0b10"
    [ ValNum 1 
    , ValProc $ Label $ Ident "myLabel"
    , ValNum 2 
    ]

    ------------------------------------------------------------------
  , testCase "label2" $ testExpand "myLabel1: 10 \n myLabel2: 20"
    [ ValProc $ Label $ Ident "myLabel1"
    , ValNum 10
    , ValProc $ Label $ Ident "myLabel2"
    , ValNum 20
    ]

    ------------------------------------------------------------------
  , testCase "label3" $ testExpand "a=1 a+a myLabel2: a*a"
    [ ValNum 2
    , ValProc $ Label $ Ident "myLabel2"
    , ValNum 1
    ]

      
  ] 


testSomething1 :: IO ()
testSomething1 = do
  let prog = "a=1+1 .macro Add(x){x} Add(asdf) asdf:"
      expect = [ ValExpr (ExprTermExpr (TermIdent (Ident "asdf")) [])
               , ValProc (Label (Ident "asdf"))]
  testExpand prog expect

testSomething2 :: IO ()
testSomething2 = do
  testExpand "a=1+1 b=a+1 c=b+1 c+1" [ValNum 5]
  
testSomething4 :: IO ()
testSomething4 = do
  let prog = concat [ ".macro Add(x, y) {x + y}"
                    , "Add(1,2)"
                    ]
  testExpand prog [ValNum 3]


testExpand :: String -> [Value] -> IO ()
testExpand prog val = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right ast) ->
      let (result, _) = expand ast SymTab.new
      in if result == Right val
         then return () 
         else error $ show ("Fail", prog, result, val)
    (Left msg) -> error (show msg)

