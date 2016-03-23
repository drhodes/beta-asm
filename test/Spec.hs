import Lib

import qualified Text.Parsec as TP
import Text.Parsec.Error
import Text.Parsec.String
import Control.Monad

main :: IO ()
main = do putStrLn "--------------------------------------------"
          putStrLn "Testing parser..."
          go1 multilineComment "multi-line-comment.uasm"
          go1 lineComment  "line-comment.uasm"
          go1 keywordMacro "keyword-macro.uasm" 
          go1 ident "ident1.uasm" 
          go1 argList "arglist.uasm" 
          go1 label "label.uasm" 
          go1 include "include.uasm" 
          go1 hexNum "hexnum.uasm"
          go1 binNum "binnum.uasm"
          go1 decNum "decnum.uasm"
          
          go1 numExpr "hexnum.uasm"
          go1 numExpr "binnum.uasm"
          go1 numExpr "decnum.uasm"
          
          go1 litChar "charlit.uasm" 
          go1 charExpr "charlit.uasm"

          go1 expr "hexnum.uasm"
          go1 expr "binnum.uasm"
          go1 expr "decnum.uasm"
          go1 expr "charlit.uasm"

          go1 expr "binexp.uasm"
          go1 expr "parenbinexp.uasm"

          go1 macroLine "macro1.uasm"
          
          go1 assn "assignment1.uasm"
          
          go1 callStmt "call.uasm"
          go1 callExpr "call.uasm"
          go1 callExpr "arglist1.uasm" 

          go1 macroBlock "macroblock.uasm"
          go1 macro "macroblock.uasm"
          go1 macro "macro1.uasm"

          go1 topLevel "top-level.uasm"
          
go1 x y = processResults $ setupTest x y
goN x y = setupTests x y
             
processResults (file, result) = do
  r <- result
  case r of
    Left err -> putStrLn $ "Fail: " ++ file ++ "\n" ++ (show err)
    Right msg -> putStrLn $ "Pass: " ++ file  -- ++ ": " ++ (show msg)
  
setupTest parser file =
  (file, parseFromFile parser ("./test/uasm/" ++ file))

setupTests parser file = do
  let path = "./test/uasm/" ++ file
  xs <- liftM lines $ readFile path
  mapM_ (\x -> (TP.parseTest parser x)) xs
