module Uasm.FileSet where

import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.String

import           Uasm.Types

-- parse all the files in the fileset, then merge the toplevels.
-- Include order matters.





-- parse :: FileSet -> IO (Either String [TopLevel])
-- parse 
