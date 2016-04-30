module TestFinalPass where

import           Uasm.Expand
import           Uasm.FinalPass
import           Uasm.LabelPass
import           Uasm.Parser
import           Uasm.Types
  
import qualified Text.Parsec as TP
import           Control.Monad
import qualified Data.Bits as DB
import           Test.Tasty
import           Test.Tasty.HUnit

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
    
    --------------------------------------------
    , testIt "test2"
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

    
    --------------------------------------------
    , testIt "test3"
    (unlines [ "1"
             , ".align 4"
             , "2"
             , ".align 4"
             , "3"
             ])
    [ ValNum 1, ValNum 0, ValNum 0, ValNum 0
    , ValNum 2, ValNum 0, ValNum 0, ValNum 0
    , ValNum 3
    ]

    --------------------------------------------
    , testIt "test4"
      (unlines [ "LABEL:"
               , "1"
               , "r31 = 31"
               , ".macro WORD(x) x%0x100 (x>>8)%0x100"
               , ".macro LONG(x) WORD(x) WORD(x >> 16)"
               , ".macro betaopc(OP,RA,CC,RC) {"
               , ".align 4"
               , "LONG((OP<<26)+((RC%0x20)<<21)+((RA%0x20)<<16)+(CC%0x10000)) }"
               , ".macro BETABR(OP,RA,RC,LABEL)   betaopc(OP,RA,((LABEL-.)>>2)-1, RC)"
               , ".macro BEQ(RA, LABEL, RC)       BETABR(0x1C,RA,RC,LABEL)"
               , ".macro BR(LABEL,RC)             BEQ(r31, LABEL, RC)"
               , ".macro BR(LABEL) BR(LABEL, r31)"
               , "BR(LABEL)"
               , "."
               ])
      [ ValNum 1
      , ValNum 0
      , ValNum 0
      , ValNum 0
      , ValNum 0xfe
      , ValNum 0xff 
      , ValNum 0xff
      , ValNum 0x73
      , ValNum 0x08
      ]

    --------------------------------------------
    , testIt "test5"
      (unlines [ "."
               , "myLabel:"
               , "."
               , "myLabel"
               , "."
               ])
      [ ValNum 0, ValNum 1, ValNum 1, ValNum 3 ]


    --------------------------------------------
    , testIt "test5"
      (unlines [ ".macro M1(n) { . = n + 4 }"
               , "M1(0)"
               ])
      [ ValNum 0, ValNum 0, ValNum 0, ValNum 0 ]


      -- these tests cat beta.uasm with the given .uasm file as the given source.
    , testCaseBeta "reserve1.uasm" [ValNum 1]
    , testCaseBeta "reserve2.uasm" [ValNum 0, ValNum 0,ValNum 0,ValNum 0]
      
    , testCaseBeta "snippet1.uasm" $ hexify [ 0xc3bd0004
                                            , 0x679dfffc
                                            , 0xc3bd0004
                                            , 0x677dfffc
                                            , 0x837df800
                                            , 0x83bbf800
                                            , 0x637dfffc
                                            , 0xc3bdfffc
                                            , 0x639dfffc
                                            , 0xc3bdfffc
                                            , 0x6ffc0000
                                            ]
      
    ]
  
--------------------------------------------

hexify xs =
  let f n = map ValNum ([ DB.shiftR (0x000000FF DB..&. n) 0
                        , DB.shiftR (0x0000FF00 DB..&. n) 8
                        , DB.shiftR (0x00FF0000 DB..&. n) 16
                        , DB.shiftR (0xFF000000 DB..&. n) 24
                        ])
  in concat $ map f xs

testFile fname expect =
  do prog <- readFile $ "test/uasm/" ++ fname
     defaultMain $ testIt fname (eraseComments prog) expect

testCaseBeta fname expect = testCase fname $ testFileWithBeta fname expect

testFileWithBeta fname expect =
  do beta <- readFile "test/uasm/beta.uasm"
     prog <- readFile $ "test/uasm/" ++ fname
     testFinalPass (eraseComments (beta ++ "\n" ++ prog)) expect

testIt caseNum prog expect =
  testCase caseNum $ testFinalPass prog expect
     
testFinalPass :: String -> [Value] -> IO ()
testFinalPass prog expect = 
  do labelPassResult <- doLabelPass prog
     case labelPassResult of
       (Right (vals, _)) ->
         case runFinalPass vals of
           Right result ->
             unless (expect == result) $
               do let trunc x = putStrLn $ take 2000 $ show x
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

doLabelPass :: Monad m => String -> m (Either String ([Value], PlaceState))
doLabelPass prog = do  
  case TP.parse (TP.many topLevel <* TP.eof) "" prog of
    (Right tops) ->
       case uniRunExpand expandTopLevels tops of
         (Right topLevels) ->
           return $ runLabelPass labelPassStmts (flattenTops topLevels) 
         (Left msg) -> error msg
    (Left msg) -> error (show msg)

