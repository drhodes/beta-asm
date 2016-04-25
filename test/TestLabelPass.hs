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
    [ValNum 0, ValNum 1]
    --------------------------------------------
  , testCase "test3" $ testLabelPass ".\n."
    [ValNum 0, ValNum 1]
    

    --------------------------------------------
  , testCase "test4" $ testLabelPass ".."
    [ValNum 0, ValNum 1]

    --------------------------------------------
  , testCase "test5" $ testLabelPass ".=4"    
    [ ValSeq [ValNum 0, ValNum 0, ValNum 0, ValNum 0]
    ]
    
    --------------------------------------------
  , testCase "test6" $ testLabelPass ".=4 ."    
    [ ValSeq [ValNum 0, ValNum 0, ValNum 0, ValNum 0]
    , ValNum 4
    ]
    
    --------------------------------------------
  , testCase "test7" $ testLabelPass "a=3 .=a ."    
    [ ValNop
    , ValSeq [ValNum 0, ValNum 0, ValNum 0]
    , ValNum 3
    ]

    --------------------------------------------
  , testCase "test8" $ testLabelPass ". myLabel: \n  myLabel"    
    [ ValNum 0
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]
    
    --------------------------------------------
  , testCase "test9" $ testLabelPass ". myLabel: \n  myLabel"    
    [ ValNum 0
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]

    --------------------------------------------
    -- planned error
    -- , testCase "test10" $ testLabelPass ". + myLabel \n myLabel:"

  , testIt "test11"
    ". myLabel: \n  myLabel"
    [ ValNum 0
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]

    --------------------------------------------
  , testIt "test12" 
    (concat [ " .              "
            , " myLabel:       "
            , " . = . + myLabel"
            , " .              "
           ])
    [ ValNum 0
    , ValProc (Label (Ident "myLabel"))
    , ValSeq [ValNum 0]
    , ValNum 2
    ]

    --------------------------------------------
  , testIt "test13" 
    (concat [ " .              "
            , " x = myLabel    "
            , " myLabel:       "
            ])
     [ ValNum 0
       -- delay evaluation on this pass because myLabel isn't known.
     , ValAssn (Assn (Ident "x") (ExprTermExpr (TermIdent (Ident "myLabel")) []))
     , ValProc (Label (Ident "myLabel"))
     ] 

    --------------------------------------------
  , testIt "test14" 
    (concat [ " .              "
            , " x = myLabel    "
            , " myLabel:       "
            , " myLabel        "
            ])
    [ ValNum 0
    , ValAssn (Assn (Ident "x") (ExprTermExpr (TermIdent (Ident "myLabel")) []))
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]
  ]

testIt caseNum prog expect = testCase caseNum $ testLabelPass prog expect

testLabelPass prog expect = do  
  case TP.parse (TP.many topLevel) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
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
         (Left msg) -> error msg
    (Left msg) -> error (show msg)
