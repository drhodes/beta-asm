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
    ". \n myLabel: \n  myLabel"
    [ ValNum 0
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]

    --------------------------------------------
  , testIt "test12" 
    (unlines [ " .              "
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
    (unlines [ " .              "
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
    (unlines [ " .              "
            , " x = myLabel    "
            , " myLabel:       "
            , " myLabel        "
            ])
    [ ValNum 0
    , ValAssn (Assn (Ident "x") (ExprTermExpr (TermIdent (Ident "myLabel")) []))
    , ValProc (Label (Ident "myLabel"))
    , ValNum 1
    ]
    
    --------------------------------------------
  , testIt "test15" 
    (unlines [ " .           "
            , " x = 1+2+3   "
            , " . = x       "
            , " 1           "
            ])
    [ ValNum 0
    , ValNop
    , ValSeq [ValNum 0,ValNum 0,ValNum 0,ValNum 0,ValNum 0]
    , ValNum 1
    ]

    --------------------------------------------
  , testIt "test16" 
    (unlines [ " .               "
             , " x = 1+1+1+1+1+1 "
             , " . = x           "
             , " 1               "
             ])
    [ ValNum 0
    , ValNop
    , ValSeq [ValNum 0,ValNum 0,ValNum 0,ValNum 0,ValNum 0]
    , ValNum 1
    ]

    --------------------------------------------
  , testIt "test17" 
    (unlines [ " .               "
             , " x = 1+1+1+1+1+1 "
             , " . = x           "
             , " 1               "
             ])
    [ ValNum 0
    , ValNop
    , ValSeq [ValNum 0,ValNum 0,ValNum 0,ValNum 0,ValNum 0]
    , ValNum 1
    ]

    --------------------------------------------
  , testIt "test18" 
    (unlines [ "."
             , ".macro Add(x, y) { x + y }"
             , "Add(1,-1)"
             , "."
             ])
    [ ValNum 0
    , ValNum 0
    , ValNum 2
    ]

    -- recursive call to JMP infinite loop
    --   --------------------------------------------
    -- , testIt "test19" 
    --   (unlines [ "."
    --            , ".macro JMP(lbl) { JMP(lbl) }"
    --            , "JMP(myLabel)" 
    --            , "myLabel:"
    --            , "."
    --            ])
    --   [ ValNum 0
    --   ]
    --------------------------------------------

    --------------------------------------------
  , testIt "test20"
      (unlines [ "."
               , ".macro WORD(x) x%0x100 (x>>8)%0x100"
               , "WORD(0xFFFF)"
               ])
      [ ValNum 0 , ValNum 255 , ValNum 255 ]

  , testIt "test21"
      (unlines [ "."
               , ".macro WORD(x) x%0x100 (x>>8)%0x100"
               , "WORD(0xFFFFFFFF)"
               ])
      [ ValNum 0 , ValNum 255 , ValNum 255 ]

  , testIt "test22"
      (unlines [ "."
               , ".macro WORD(x) x%0x100 (x>>8)%0x100"
               , ".macro LONG(x) WORD(x) WORD(x >> 16)"
               , "LONG(0xDEADBEEF)"
               ])
      [ ValNum 0 
      , ValNum 0xEF, ValNum 0xBE, ValNum 0xAD, ValNum 0xDE
      ]
    
  ]


testProc = testGroup "TestLabelPass.hs Procs"
  [
    --------------------------------------------
    testCase "testProc1" $ testLabelPass
    (unlines [ "1 2"
             , ".align 4"
             , "3"
             ])
    [ ValNum 1
    , ValNum 2
    , ValSeq [ValNum 0, ValNum 0]
    , ValNum 3
    ]
    
    --------------------------------------------
  , testCase "testProc2" $ testLabelPass
    (unlines [ ".ascii \"ABCD\""
             ])
    [ ValSeq [ ValNum 0x41
             , ValNum 0x42
             , ValNum 0x43
             , ValNum 0x44
             ]
    ]


    
    
  ] -- eof of testProc


--------------------------------------------
    
testFile fname expect = do
  prog <- readFile $ "test/uasm/" ++ fname
  defaultMain $ testIt fname (eraseComments prog) expect

testIt caseNum prog expect = testCase caseNum $ testLabelPass prog expect

testLabelPass :: String -> [Value] -> IO ()
testLabelPass prog expect = do  
  case TP.parse (TP.many topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, symtab)) ->
           case runLabelPass labelPassStmts (flattenTops topLevels) of
             Right (result, valueTable) ->
               if result == expect
               then do return () 
               else do let trunc x = putStrLn $ take 2000 $ show x
                       putStrLn "\nFail"
                       trunc  prog
                       putStrLn "\nExpected"
                       trunc expect
                       putStrLn "\nGot"
                       trunc result
                       putStrLn "\nTopLevels parsed"
                       trunc tops
                       error "test fails"
             Left msg -> error msg
         (Left msg) -> error msg
    (Left msg) -> error (show msg)
