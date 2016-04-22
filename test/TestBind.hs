module TestBind where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import qualified Uasm.SymbolTable as SymTab
import Uasm.Bind 
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

-- tests = testGroup "TestBind.hs"
--   [
--     testCase "bind_test_1" $ testSomething1
--     -- , testCase "testSomething2" $ testSomething2
--     -- , testCase "testDot1" $ testExpand "." [ValIdent CurInstruction]
    
--     -- , testCase "testDot2" $ testExpand ". ." [ ValIdent CurInstruction
--     --                                         , ValIdent CurInstruction]
--     -- , testCase "testSomething4" $ testSomething4
--   ] 

tests = testGroup "TestBind.hs" 
  [ testCase "simple assign" $ testBind "a=b"
    (SymTab.build [
        (KeyIdent $ Ident "b", ExprTerm $ TermLitNum $ LitNum 1)
        ])
    [TopStmt (StmtAssn
              (Assn (Ident "a")
               (ExprTermExpr
                (TermExpr
                 (ExprTerm
                  (TermLitNum
                   (LitNum 1)))) [])))]

    ------------------------------------------------------------------
    -- testCase "simple assign 2" $
    -- testBind "a=b"
  ]

testBind prog symtab val = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right ast) -> 
      case bind symtab ast of
        (Right x) -> if x == val
                     then return () -- yay
                     else fail (show x)
        (Left msg) -> error $ msg ++ "\n" ++ (show ast)
    (Left msg) -> error (show msg)

