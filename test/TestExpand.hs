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
  [ --------------------------------------------
    testCase "test1" $ testExpand "."
    [StmtExpr (ExprTerm (TermIdent CurInstruction))]
    
    --------------------------------------------
  , testCase "test2" $ testExpand "a=1 \n b=a \n .macro M1(c) {b + c} \n M1(5)"
    [ StmtAssn (Assn (Ident "a") (ExprTermExpr (TermLitNum (LitNum 1)) []))
    , StmtAssn (Assn (Ident "b") (ExprTermExpr (TermIdent (Ident "a")) []))
    , StmtExpr (ExprTermExpr (TermExpr (ExprTermExpr (TermIdent (Ident "a")) []))
                [ExprBinTail Addition (TermExpr (ExprTerm (TermLitNum (LitNum 5))))])]
    
    --------------------------------------------
  , testCase "test3" $ testExpand "a=1 b=2 .macro M1(a){a} M1(2)"
    [ StmtAssn (Assn (Ident "a") (ExprTermExpr (TermLitNum (LitNum 1)) []))
    , StmtAssn (Assn (Ident "b") (ExprTermExpr (TermLitNum (LitNum 2)) []))
    , StmtExpr (ExprTerm (TermLitNum (LitNum 2)))]
    
    --------------------------------------------
  , testCase "test4" $ testExpand ".macro M1(a){a} .macroM2(a){M1(a) M1(a)} M2(1)"
    [ StmtExpr (ExprTerm (TermLitNum (LitNum 1)))
    , StmtExpr (ExprTerm (TermLitNum (LitNum 1)))]

    --------------------------------------------
  , testCase "test5" $ testExpand "label1: 1 2 label2:"
    [ StmtProc (Label (Ident "label1"))
    , StmtExpr (ExprTerm (TermLitNum (LitNum 1)))
    , StmtExpr (ExprTerm (TermLitNum (LitNum 2)))
    , StmtProc (Label (Ident "label2"))]
  ]

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
