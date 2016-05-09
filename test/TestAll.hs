import           Uasm.Parser
import           Uasm.Types
import qualified Text.Parsec as TP
import qualified TestExpand as TE
import qualified TestLabelPass as TLP
import qualified TestFinalPass as TFP
import qualified TestParse as TestParse

import qualified TestBeta.TestMach as TBM

import           Test.Tasty
import           Test.Tasty.HUnit

main = do
  defaultMain allTests

allTests :: TestTree
allTests = testGroup "Tests"
  [
    --------------------------------------------
    -- assembler tests
    TestParse.parseTests  
  , TE.testAll
  , TLP.testAll
  , TLP.testProc
  , TFP.testAll

    --------------------------------------------
    -- simulation tests    
  , TBM.testAll
    
  ]

