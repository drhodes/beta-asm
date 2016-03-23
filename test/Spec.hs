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
          
          --go1 callExpr "intercomment.uasm"
          

go1 x y = setupTest x y >>= processResults
             
processResults :: (Show a, Show a1) => ([Char], Either a a1) -> IO ()
processResults (file, result) = do
  case result of
    Left err -> putStrLn $ "Fail: " ++ file ++ "\n" ++ (show err)
    Right msg -> putStrLn $ "Pass: " ++ file ++ ": " ++ (show msg)
  
setupTest :: TP.Parsec [Char] () a -> [Char] -> IO ([Char], Either ParseError a)
setupTest parser file = do
  txt <- readFile $ "./test/uasm/" ++ file
  let t = eraseComments txt
  let r = TP.parse parser file t
  return (file, r)

eraseLineComment [] _ = []
eraseLineComment src@(c:str) inComment =
  if take 2 src == "//"
  then "  " ++ (eraseLineComment (drop 2 src) True)
  else if c == '\n'
       then c:(eraseLineComment str False)
       else if inComment
            then ' ':(eraseLineComment str True)
            else c:(eraseLineComment str False)

eraseBlockComment [] _ = []
eraseBlockComment src@(c:str) inComment =
  if take 2 src == "/*" && not inComment
  then "  " ++ (eraseBlockComment (drop 2 src) True)       
  else if take 2 src == "*/" && inComment
       then "  " ++ (eraseBlockComment (drop 2 src) False)
       else if inComment               
            then ' ':(eraseBlockComment str inComment)
            else c:(eraseBlockComment str inComment)


eraseComments src =
  (eraseLineComment
   (eraseBlockComment src False) False)
