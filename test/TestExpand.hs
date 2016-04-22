module TestExpand where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import qualified Uasm.SymbolTable as SymTab
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"

testAll = testGroup "TestExpand.hs"
  [ testCase "test1" $ testExpand "."
    [TopStmt (StmtExpr (ExprTermExpr (TermIdent CurInstruction) []))]
    
  , testCase "test2" $ testExpand "a=1 \n b=a \n .macro M1(c) {b + c} \n M1(5)"
    [ TopStmt (StmtAssn (Assn (Ident "a") (ExprTermExpr (TermLitNum (LitNum 1)) [])))
    , TopStmt (StmtAssn (Assn (Ident "b") (ExprTermExpr (TermIdent (Ident "a")) [])))
    , TopMacro (Macro (Ident "M1")
                [Ident "c"]
                [StmtExpr (ExprTermExpr (TermIdent (Ident "b")) [ExprBinTail Addition (TermIdent (Ident "c"))])])
    , TopStmt (StmtMany
               [StmtExpr (ExprTermExpr (TermExpr (ExprTermExpr (TermIdent (Ident "a")) []))
                          [ExprBinTail Addition (TermExpr (ExprTermExpr (TermLitNum (LitNum 5)) []))])])]
    

    
    -- testCase "testSomething1" $ testSomething1
    -- , testCase "testSomething2" $ testSomething2
    -- , testCase "testDot1" $ testExpand "." [ValIdent CurInstruction]
    
    -- , testCase "testDot2" $ testExpand ". ." [ ValIdent CurInstruction
    --                                         , ValIdent CurInstruction]
    -- , testCase "testSomething4" $ testSomething4
  ] 

testLabels = testGroup "Label Tests"
  [
  --   testCase "label1" $ testExpand "0x1 \n myLabel: \n 0b10"
  --   [ ValNum 1 
  --   , ValProc $ Label $ Ident "myLabel"
  --   , ValNum 2 
  --   ]

  --   ------------------------------------------------------------------
  -- , testCase "label2" $ testExpand "myLabel1: 10 \n myLabel2: 20"
  --   [ ValProc $ Label $ Ident "myLabel1"
  --   , ValNum 10
  --   , ValProc $ Label $ Ident "myLabel2"
  --   , ValNum 20
  --   ]

  --   ------------------------------------------------------------------
  -- -- , testCase "label3" $ testExpand "a=1 a+a myLabel2: a*a"
  -- --   [ ValNum 2
  -- --   , ValProc $ Label $ Ident "myLabel2"
  -- --   , ValNum 1
  -- --   ]



  -- , testCase "label4" $ testExpand ". = . + 4 \n myLabel: \n .+myLabel" []

    
  ] 
{-
testSomething1 :: IO ()
testSomething1 = do
  let prog = "a=1+1 .macro Add(x){x} Add(a)"
      expect = []
  testExpand prog expect


testSomething2 :: IO ()
testSomething2 = do

  testExpand "a=1+1 b=a+1 c=b+1 c+1" []
  
testSomething4 :: IO ()
testSomething4 = do
  let prog = concat [ ".macro Add(x, y) {x + y}"
                    , "Add(1,2)"
                    ]
  testExpand prog []
-}

testExpand :: String -> [TopLevel] -> IO ()
testExpand prog expect = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right tops) ->
      do case expand expandTopLevels tops of
           (Right (topLevels, symtab)) -> 
             if topLevels == expect
             then return () 
             else error $ show ("Fail", prog, "Expected", expect, "Got", topLevels)
           (Left msg) ->
             error msg
    (Left msg) -> error (show msg)
