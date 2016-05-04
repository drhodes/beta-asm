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
