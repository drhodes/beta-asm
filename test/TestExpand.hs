module TestExpand where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import qualified Uasm.Pretty as PP
import qualified Uasm.SymbolTable as SymTab
import Text.PrettyPrint.Leijen
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

unitTests = testGroup "Unit tests"

testAll = testGroup "TestExpand.hs"
  [ testCase "test1" $ testExpand "."
    [StmtExpr (ExprTerm (TermIdent CurInstruction))]
    
  , testCase "test2" $ testExpand "a=1 \n b=a \n .macro M1(c) {b + c} \n M1(5)"
    [ StmtAssn (Assn (Ident "a") (ExprTermExpr (TermLitNum (LitNum 1)) []))
    , StmtAssn (Assn (Ident "b") (ExprTermExpr (TermIdent (Ident "a")) []))
    , StmtExpr (ExprTermExpr (TermExpr (ExprTermExpr (TermIdent (Ident "a")) []))
                [ExprBinTail Addition (TermExpr (ExprTerm (TermLitNum (LitNum 5))))])]
    
  -- , testCase "test3" $ testExpand "a=1 b=2 .macro M1(a){a} M1(2)"
  --   [ (StmtAssn (Assn (Ident "a") (ExprTermExpr (TermLitNum (LitNum 1)) [])))
  --   , (StmtAssn (Assn (Ident "b") (ExprTermExpr (TermLitNum (LitNum 2)) [])))
  --   , (StmtExpr (ExprTermExpr (TermExpr (ExprTermExpr (TermLitNum (LitNum 2)) [])) []))]

  , testCase "test4" $ testExpand ".macro M1(a){a} .macroM2(a){M1(a) M1(a)} M2(1)"
    [ StmtExpr (ExprTerm (TermLitNum (LitNum 1)))
    , StmtExpr (ExprTerm (TermLitNum (LitNum 1)))]
    -- testcase "testSomething1" $ testSomething1
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

testExpand prog expect = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right tops) ->
      do case expand expandTopLevels tops of
           (Right (topLevels, symtab)) ->
             let result = flattenTops topLevels
             in if result == expect
                then do return () 
                else do putStrLn "\nFail"
                        putStrLn prog
                        putStrLn "\nExpected"
                        print expect
                        putStrLn "\nGot"
                        print result
                        putStrLn "\nTopLevels parsed"
                        print tops
                        error "test fails"
           (Left msg) ->
             error msg
    (Left msg) -> error (show msg)
