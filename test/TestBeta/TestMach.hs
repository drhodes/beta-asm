module TestBeta.TestMach where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
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
  , testMach8
  , testMach9
  ]
  
testMachReg0 numSteps str expect = do
  result <- BU.assembleString str
  case result of
    Left msg -> error msg
    Right words -> do
      let mach = Mach.fromWordPos words
      case Mach.doMach mach (Mach.stepN numSteps) of
        Left msg -> error $ "Stepping fails: " ++ msg
        Right (_, m) -> 
          case DM.lookup R0 (cpuRegFile m) of
            Nothing -> error "Reg0 not found in testcase, should not happen"
            Just v -> when (v /= expect) $
              error $ unlines [ "     GOT: " ++ show v
                              , "EXPECTED: " ++ show expect]

testCaseReg0 name numSteps src expect = do
  testCase name $ testMachReg0 numSteps src expect

testMach1 = testCaseReg0 "mach1" 1 "ADDC(0, 3, 0)" 3
testMach2 = testCaseReg0 "mach2" 2 "ADDC(0, 2, 0) ADDC(0, 2, 0)" 4
testMach3 = testCaseReg0 "mach3" 2 "ADDC(0, 2, 0) ADD(0, 0, 0)" 4
testMach4 = testCaseReg0 "mach4"
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "ADD(0, 0, 0)"
           , "ADD(0, 0, 0)"
           ])
  8

testMach5 = testCaseReg0 "mach5"
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "myLabel:"
           , "ADD(0, 0, 0)"
           , "ADD(0, 0, 0)"
           ])
  8
  
testMach6 = testCaseReg0 "mach6"
  4
  (unlines [ "ADDC(0, 60, 0)"
           , "JMP(myLabel)"
           , "ADD(0, 30, 0)" 
           , "myLabel:"
           , "ADDC(0, 0, 0)"
           ])
  120

testMach7 = testCaseReg0 "mach7"
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "JMP(r0, myLabel)"
           , "ADD(r0, r0, r0)" 
           , "myLabel:"
           , "ADD(r0, R0, R0)"
           ])
  4

testMach8 = testCaseReg0 "mach8"
  3
  (unlines [ "ADDC(0, 2, 0)"
           , "BR(myLabel)"
           , "ADD(0, 0, 0)" 
           , "myLabel:"
           , "ADD(0, 0, 0)"
           ])
  4

testMach9 = testCaseReg0 "mach9"
  3
  (unlines [ "SUB(r0, r0, r0)" -- set r0 to 0
           , "BEQ(r0, myLabel, r31)" -- branch to myLabel
           , "ADDC(r0, 2, r0)" 
           , "myLabel:"
           , "ADDC(r0, 3, r0)"
           ])
  3


testMach10 = testCaseReg0 "mach10"
  3
  (unlines [ "SUB(r0, r0, r0)" -- set r0 to 0
           , "BEQ(r0, myLabel, r31)" -- branch to myLabel
           , "ADDC(r0, 2, r0)" 
           , "myLabel:"
           , "ADDC(r0, 3, r0)"
           ])
  3
