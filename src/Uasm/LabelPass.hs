module Uasm.LabelPass where

import qualified Uasm.SymbolTable as SymTab

import           Data.Word
import           Uasm.Eval
import           Uasm.Parser
import           Uasm.Types

{- Macros have been expanded. Label addresses are still unknown. Current
address identifiers (.) can now be evaluated, and must be
-}
type CurAddr = Integer

class FixDot a where
  fixdot :: a -> CurAddr -> SymbolTable -> (Maybe a, CurAddr, SymbolTable)

fixDots :: [Value] -> CurAddr -> SymbolTable -> [Value]
fixDots [] _ _ = []
fixDots (x:xs) addr st =
  let (y, nextAddr, st2) = fixdot x addr st
  in case y of
    (Just s) -> s:(fixDots xs nextAddr st2)
    Nothing -> (fixDots xs nextAddr st2)

instance FixDot Value where
  fixdot (ValNum n) addr st = (Just $ ValNum n, addr, st)
  fixdot (ValDotAssn val) addr st =
    let (r, next, st2) = fixdot val addr st
    in case r of
      (Just (ValNum n)) -> (Nothing, next + n, st2)
      _ -> error "Impossible fixdot didn't get a num when evaling the dot assignment"
  fixdot (ValIdent CurInstruction) addr st = (Just $ ValNum addr, addr, st)
  fixdot (Delayed op val1 val2) addr st =
    let (Just r1, _, st1) = fixdot val1 addr st
        (Just r2, _, st2) = fixdot val2 addr st1
    in (Just $ eval (Delayed op r1 r2) st2, addr, st2)
       
  fixdot (ValProc p) addr st =
    let (r, addr2, st2) = fixdot p addr st
    in case r of
      (Just p2) -> (Just $ ValProc p2, addr2, st2)
      Nothing -> (Nothing, addr2, st2)
       
  fixdot x _ _ = error (show x)


instance FixDot Proc where
  fixdot (Label ident) addr st =
    (Nothing, addr, SymTab.insert (KeyIdent ident) (ValNum addr) st)
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
    
