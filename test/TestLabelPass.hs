module TestLabelPass where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import           Uasm.LabelPass
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

testAll = testGroup "TestLabelPass.hs"
  [ --------------------------------------------
    testCase "test1" $ testLabelPass "."
    [ValNum 0]

    --------------------------------------------
  , testCase "test2" $ testLabelPass ". ."
    [ValNum 0, ValNum 4]

    --------------------------------------------
  , testCase "test3" $ testLabelPass ".\n."
    [ValNum 0, ValNum 4]

    --------------------------------------------
  , testCase "test4" $ testLabelPass ".."
    [ValNum 0, ValNum 4]

    --------------------------------------------
  , testCase "test5" $ testLabelPass ".=4"    
    [ ValNop ]
    
    --------------------------------------------
  , testCase "test5" $ testLabelPass ".=4 ."    
    [ ValNop
    , ValNum 4
    ]
    
  ]

testLabelPass prog expect = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right tops) ->
      do case expand expandTopLevels tops of
           (Right (topLevels, symtab)) ->
             case runLabelPass labelPassStmts [stmt | (TopStmt stmt) <- topLevels] of
               Right (result, valueTable) ->
                 if result == expect
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
               Left msg -> error msg
           (Left msg) ->
             error msg
    (Left msg) -> error (show msg)
