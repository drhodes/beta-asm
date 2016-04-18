import           Uasm.Parser
import           Uasm.Eval
import           Uasm.Macro
import qualified Uasm.SymbolTable as SymTab
  
import qualified Text.Parsec as TP
import           Text.Parsec.Error
import           Text.Parsec.String
import           Control.Monad

testSomething = do
  case TP.parse (TP.many topLevel) "" "a=10 b=20 a+b a b" of
    (Right ast) -> expand ast SymTab.new
    (Left msg) -> error (show msg)

