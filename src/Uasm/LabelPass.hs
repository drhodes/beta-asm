module Uasm.LabelPass where

import qualified Uasm.SymbolTable as SymTab

import           Control.Monad
import           Data.Word
import           Uasm.Eval
import           Uasm.Parser
import           Uasm.Types

{- Macros have been expanded. Label addresses are still unknown. Current
address identifiers (.) can now be evaluated, and must be
-}
{-
type CurAddr = Integer

class FixDot a where
  fixdot :: a -> CurAddr -> SymbolTable -> Either String (Maybe a, CurAddr, SymbolTable)

fixDots :: [Value] -> CurAddr -> SymbolTable -> [Value]
fixDots [] _ _ = []
fixDots (x:xs) addr st =
  let tmp = fixdot x addr st
  in case tmp of
    Right (y, nextAddr, st2) ->
      case y of
        (Just s) -> s:(fixDots xs nextAddr st2)
        Nothing -> (fixDots xs nextAddr st2)
    Left msg -> error msg

instance FixDot Value where
  fixdot (ValNum n) addr st = Right (Just $ ValNum n, addr + 1, st)
  
  fixdot (ValDotAssn val) addr st =
    do (r, next, st2) <- fixdot val addr st
       case r of
         (Just (ValNum n)) -> Right (Nothing, next + n, st2)
         _ -> Left "Impossible fixdot didn't get a num when evaling the dot assignment"
         
  fixdot (ValIdent CurInstruction) addr st = Right (Just $ ValNum addr, addr, st)
  
  fixdot (Delayed op val1 val2) addr st =
    do (Just r1, _, st1) <- fixdot val1 addr st
       (Just r2, _, st2) <- fixdot val2 addr st1
       temp <- eval (Delayed op r1 r2) st2
       return (Just temp, addr, st2)
       
  fixdot (ValProc p) addr st = 
    do (r, addr2, st2) <- fixdot p addr st
       return (liftM ValProc r, addr2, st2)
       
  fixdot (ValExpr e) addr st =
    do v <- eval e st
       return (Just v, addr + 1, st)
    
  fixdot x _ _ = error $ "Need to implemenet Fixdot Value for: " ++ (show x)

instance FixDot Proc where
  fixdot (Label ident) addr st = 
    Right (Nothing, addr, SymTab.insert (KeyIdent ident) (ValNum addr) st)
  -- fixdot (DotInclude string) addr st = error "fixdot (DotInclude string) addr st"
  -- fixdot (DotAlign expr) addr st = error "fixdot (DotAlign expr) addr st"
  -- fixdot (DotAscii string) addr st = error "fixdot (DotAscii string) addr st"
  -- fixdot (DotText string) addr st = error "fixdot (DotText string) addr st"
  -- fixdot (DotBreakPoint) addr st = error "fixdot (DotBreakPoint) addr st"
  -- fixdot (DotProtect) addr st = error "fixdot (DotProtect) addr st"
  -- fixdot (DotUnprotect) addr st = error "fixdot (DotUnprotect) addr st"
  -- fixdot (DotOptions) addr st = error "fixdot (DotOptions) addr st"


-- Pass2 will determine label addresses.
-- labelPass :: [Value] -> CurAddr -> [Value]

-- labelPass (v:vals) addr =
--   case v of
--     ValMacro macro = labalPass
--     ValIdent ident
--     ValExpr expr
--     ValProc proc
--     NegVal val
--     Delayed binop val1 val2
--     ValNum n -> v
    
-}
