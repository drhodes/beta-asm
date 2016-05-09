module TestBeta.TestMach where

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
import qualified TestFinalPass as TFP
import qualified Beta.Mach as Mach
import qualified Beta.Util as BU
import qualified Data.Map as DM
import           Beta.Types

testAll = testGroup "TestMach.hs"
  [ testMach1
  , testMach2
  , testMach3
  , testMach4
  , testMach5
  , testMach6
  , testMach7
  ]

  
--   testIt "ADDC(SP, 8000, SP)" (OPC (mkOpcode 0x30) sp sp 8000) 0x3db1f40
--   , testIt "ADDC(SP, 8000, SP)" (OPC (mkOpcode 0x30) sp sp 8000) 0x3db1f40
--   ]


-- testIt 
testMachReg0 numSteps str expect = do
  result <- BU.assembleString str
  case result of
    Left msg -> error msg
    Right words -> do
      let mach = Mach.fromWords (BU.toBinary words)
      case Mach.doMach mach (Mach.stepN numSteps) of
        Left msg -> error msg
        Right (_, m) -> 
          case DM.lookup R0 (cpuRegFile m) of
            Nothing -> error "Reg0 not found in testcase, should not happen"
            Just v -> when (v /= expect) $
              error $ "expected value not found: " ++ (show v)


testCaseReg0 numSteps src expect = do
  testCase src $ testMachReg0 numSteps src expect

testMach1 = testCaseReg0 1 "ADDC(0, 3, 0)" 3
testMach2 = testCaseReg0 2 "ADDC(0, 2, 0) ADDC(0, 2, 0)" 4
testMach3 = testCaseReg0 2 "ADDC(0, 2, 0) ADD(0, 0, 0)" 4
testMach4 = testCaseReg0
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "ADD(0, 0, 0)"
           , "ADD(0, 0, 0)"
           ])
  8

testMach5 = testCaseReg0
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "myLabel:"
           , "ADD(0, 0, 0)"
           , "ADD(0, 0, 0)"
           ])
  8

testMach6 = testCaseReg0
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "JMP(myLabel)"
           , "ADD(0, 0, 0)" 
           , "myLabel:"
           , "ADD(0, 0, 0)"
           ])
  4

testMach7 = testCaseReg0
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "JMP(myLabel, r0)"
           , "ADD(0, 0, 0)" 
           , "myLabel:"
           , "ADD(0, 0, 0)"
           ])
  4

           
