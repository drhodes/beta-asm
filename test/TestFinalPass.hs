module TestFinalPass where

import           Uasm.Parser
import           Uasm.Types
import           Uasm.Eval
import           Uasm.Expand
import           Uasm.LabelPass
import           Uasm.FinalPass
import qualified Uasm.Pretty as PP
import qualified Uasm.SymbolTable as SymTab
import           Text.PrettyPrint.Leijen
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.HUnit

import           GHC.Stack

unitTests = testGroup "Unit tests"
testAll = testGroup "TestLabelPass.hs"
  [ --------------------------------------------
    testIt "test1"
    (unlines [ "."
             , "LABEL"
             , "LABEL:"
             , "."
             ])
    [ ValNum 0
    , ValNum 2
    , ValNum 2
    ]
    
    -- --------------------------------------------
    -- , testIt "test2"
    -- (unlines [ "."
    --          , "r31 = 31"
    --          , ".macro WORD(x) x%0x100 (x>>8)%0x100"
    --          , ".macro LONG(x) WORD(x) WORD(x >> 16)"
    --          , ".macro betaopc(OP,RA,CC,RC) {"
    --          , "   .align 4"
    --          , "   LONG((OP<<26)+((RC%0x20)<<21)+((RA%0x20)<<16)+(CC%0x10000)) }"
    --          , ".macro BETABR(OP,RA,RC,LABEL)   betaopc(OP,RA,((LABEL-.)>>2)-1, RC)"
    --          , ".macro BEQ(RA, LABEL, RC)       BETABR(0x1C,RA,RC,LABEL)"
    --          , ".macro BR(LABEL,RC)             BEQ(r31, LABEL, RC)"
    --          , ".macro BR(LABEL) BR(LABEL, r31)"
    --          , "BR(LABEL)"
    --          , "LABEL:"
    --          , "."
    --          ])
    -- [ ValNum 0
    -- , ValNum 0
    -- , ValNum 0
    -- , ValNum 0
    -- , ValNum 0
    -- , ValNum 0
    -- , ValNum 0xff
    -- , ValNum 0x73
    -- , ValNum 0x8
    -- ]

    --------------------------------------------
    , testIt "test3"
    (unlines [ "LABEL:"
             , "."
             , "r31 = 31"
             , ".macro WORD(x) x%0x100 (x>>8)%0x100"
             , ".macro LONG(x) WORD(x) WORD(x >> 16)"
             , ".macro betaopc(OP,RA,CC,RC) {"
             , "   .align 4"
             , "   LONG((OP<<26)+((RC%0x20)<<21)+((RA%0x20)<<16)+(CC%0x10000)) }"
             , ".macro BETABR(OP,RA,RC,LABEL)   betaopc(OP,RA,((LABEL-.)>>2)-1, RC)"
             , ".macro BEQ(RA, LABEL, RC)       BETABR(0x1C,RA,RC,LABEL)"
             , ".macro BR(LABEL,RC)             BEQ(r31, LABEL, RC)"
             , ".macro BR(LABEL) BR(LABEL, r31)"
             , "BR(LABEL)"
             ])
    [ ValNum 0
    , ValNum 0
    , ValNum 0
    , ValNum 0
    , ValNum 0xfe
    , ValNum 0xff 
    , ValNum 0xff
    , ValNum 0x73
    ]

    
  ]

--------------------------------------------
testFile fname expect = do
  prog <- readFile $ "test/uasm/" ++ fname
  defaultMain $ testIt fname (eraseComments prog) expect

-- testIt caseNum prog expect = testCase caseNum $ testLabelPass prog expect

testIt caseNum prog expect =
  testCase caseNum $ testFinalPass prog expect
     
testFinalPass prog expect = 
  do labelPassResult <- doLabelPass prog
     case labelPassResult of
       (Right (vals, placeState)) ->
         case runFinalPass vals of
           Right result ->
             if expect == result
             then return ()
             else do let trunc x = putStrLn $ take 2000 $ show x
                     putStrLn "\nFail"
                     trunc  prog
                     putStrLn "\nExpected"
                     trunc expect
                     putStrLn "\nGot"
                     trunc result
                     putStrLn "\nLabelPass"
                     trunc vals
                     error "test fails"
           Left msg -> error msg
       (Left msg) -> error msg

doLabelPass :: Monad m => String -> m (Either [Char] ([Value], PlaceState))
doLabelPass prog = do  
  case TP.parse (TP.many topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case expand expandTopLevels tops of
         (Right (topLevels, symtab)) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels) 
         (Left msg) -> error msg
    (Left msg) -> error (show msg)

-- LABEL:
-- .
-- r31 = 31
-- .macro WORD(x) x%0x100 (x>>8)%0x100
-- .macro LONG(x) WORD(x) WORD(x >> 16)
-- .macro betaopc(OP,RA,CC,RC) {
-- .align 4
-- LONG((OP<<26)+((RC%0x20)<<21)+((RA%0x20)<<16)+(CC%0x10000)) }
-- .macro BETABR(OP,RA,RC,LABEL)   betaopc(OP,RA,((LABEL-.)>>2)-1, RC)
-- .macro BEQ(RA, LABEL, RC)       BETABR(0x1C,RA,RC,LABEL)
-- .macro BR(LABEL,RC)             BEQ(r31, LABEL, RC)
-- .macro BR(LABEL) BR(LABEL, r31)
-- BR(LABEL)
-- .
