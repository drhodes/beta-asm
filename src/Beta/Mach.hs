{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BinaryLiterals #-}

module Beta.Mach where

import Numeric
import           Beta.Decoder
import           Beta.Types
import           Beta.Err
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits as DB
import qualified Data.Map as DM
import           Data.Word
import           Data.Functor.Identity
import qualified Text.JSON.Generic as G
import Prelude hiding (and, or, xor)
import qualified Text.Parsec as TP
import qualified Debug.Trace as DT

runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

new = Mach mkRegFile 0 mkRam []


fromWordPos ws = fromWords [WordLoc w (Just pos) | (w, pos) <- ws]

fromWords :: [WordLocated] -> Mach
fromWords words = Mach mkRegFile 0 (DM.fromList $ zip [0, 4 ..] words) []

doMach :: s -> StateT s (ExceptT e Identity) a -> Either e (a, s)
doMach m f = runIdentity . runExceptStateT m $ f

getReg :: Reg -> Mac Word32
getReg rx = do
  regFile <- liftM cpuRegFile get
  if rx == R31
    then return 0
    else case DM.lookup rx regFile of
      Just w -> return w
      Nothing -> throwError $ "Failed to find register in regfile: " ++ (show rx)

runAndGetR0 :: Mac Word32
runAndGetR0 = do
  go
  r <- getReg R0
  return r

say s = do
  m <- get
  let l = cpuLog m
  put m{cpuLog = l ++ [s]}

setMem :: Word32 -> Word32 -> Mac ()
setMem addr w = do
  say $ concat ["setting mem@: ", show addr, " to: ", asHex w]
  Mach rf pc ram log <- get
  put (Mach rf pc (DM.insert addr (WordLoc w Nothing) ram) log)
        
setReg :: Reg -> Word32 -> Mac ()
setReg rx w = do
  say $ concat ["setting reg@: ", show rx, " to: ", asHex w]
  Mach rf pc ram log <- get
  put (Mach (DM.insert rx w rf) pc ram log)

incPC :: Mac ()
incPC = do
  Mach rf pc ram log <- get
  say $ concat ["incrementing pc to: ", asHex $ pc + 4]
  put (Mach rf (pc + 4) ram log)

getPC :: Mac Word32
getPC = cpuPC <$> get
  
setPC pc = do
  Mach rf _ ram log <- get  
  say $ concat ["setting pc to: ", asHex $ pc]
  put (Mach rf pc ram log)

getMem :: Word32 -> Mac WordLocated
getMem addr = do
  Mach _ _ ram _ <- get
  case DM.lookup addr ram of
    Just n -> do
      say $ concat ["getting mem@: ", asHex addr, ", it is: ", show n]
      return n
    Nothing -> throwError $ "Invalid Address: " ++ (show addr)

reset :: Mac ()
reset = put new

-- store rc lit ra = do
--   incPC
--   regRA <- getReg ra
--   let ea = regRA + sxt lit
--   regRC <- getReg rc
--   setMem ea regRC

ld ra lit rc = do
  -- Reg[Rc] ← Mem[Reg[Ra] + SEXT(literal)]
  say $ show ("Load", ra, lit, rc)
  regRA <- getReg ra
  (WordLoc w _) <- (getMem $ regRA + sxt lit)
    ? "Can't LD: " ++ (show (ra, lit, rc))
  setReg rc w

opInst oper rc ra rb = do
  incPC
  rx <- getReg ra
  ry <- getReg rb
  setReg rc (oper (fromIntegral rx) (fromIntegral ry))

beq ra lit rc = do
  -- Reg[Rc] ← PC + 4; if Reg[Ra] = 0 then PC ← PC + 4 + 4*SEXT(literal)
  incPC
  pc <- getPC
  setReg rc pc
  regRA <- getReg ra
  let ea = pc + 4 * (sxt lit)
  when (regRA == 0) $ setPC ea
  
bne ra lit rc = do
  incPC
  pc <- getPC
  setReg rc (pc + 4)
  regRA <- getReg ra
  when (regRA /= 0) $ setPC (pc + 4 + 4 * sxt(lit))

st rc lit ra = do
  -- Mem[Reg[Ra] + SEXT(literal)] ← Reg[Rc]
  incPC
  regRa <- getReg ra
  regRc <- getReg rc
  setMem (regRa + sxt lit) regRc

cmpeq rc ra rb = do
  incPC
  regRa <- getReg ra
  regRb <- getReg rb
  if regRa == regRb
    then setReg rc 1
    else setReg rc 0 

cmpeqc ra lit rc = do
  incPC
  regRa <- getReg ra
  if regRa == sxt lit
    then setReg rc 1
    else setReg rc 0

cmple rc ra rb = do
  incPC
  regRa <- getReg ra
  regRb <- getReg rb
  if regRa <= regRb
    then setReg rc 1
    else setReg rc 0

cmplec ra lit rc = do
  incPC
  regRa <- getReg ra
  if regRa <= sxt lit
    then setReg rc 1
    else setReg rc 0

cmpltc ra lit rc = do
  incPC
  regRa <- getReg ra
  if regRa < sxt lit
    then setReg rc 1
    else setReg rc 0

cmplt rc ra rb = do
  incPC
  regRA <- getReg ra
  regRB <- getReg rb
  if regRA < regRB
    then setReg rc 1
    else setReg rc 0

simpleOp f rc ra rb = do
  incPC
  regRA <- getReg ra
  regRB <- getReg rb
  setReg rc (f (fromIntegral regRA) (fromIntegral regRB))

jmp ra rc = do
  incPC
  regRA <- getReg ra
  let ea = regRA .&. 0xFFFFFFFC
  pc <- getPC
  setReg rc pc
  setPC ea

loadWords :: MonadState Mach m => [WordLocated] -> m ()
loadWords words = do
  Mach rf pc _ log <- get
  put $ Mach rf pc (DM.fromList $ zip [0, 4 ..] words) log

fetch :: Mac Instruction
fetch = do
  pc <- getPC 
  (WordLoc word _) <- getMem (pc .&. 0x7FFFFFFF)
    ? "Mach.fetch can't mem at program counter: " ++ show pc
    
  decode word
    ? "Mach.fetch can't decode word: " ++ show word
    
divide c a b = opInst div c a b >> (say $ show ("divide", c, a, b))
mul c a b = opInst (*) c a b >> (say $ show ("mul", c, a, b))
or c a b = opInst (.|.) c a b >> (say $ show ("or", c, a, b))
shl c a b = opInst (shiftL) c a b >> (say $ show ("shl", c, a, b))
add c a b  = opInst (+) c a b >> (say $ show ("add", c, a, b))
sub c a b = opInst (-) c a b >> (say $ show ("sub", c, a, b))
and c a b = opInst (.&.) c a b >> (say $ show ("and", c, a, b))
shr c a b = opInst (shiftR) c a b >> (say $ show ("shr", c, a, b))
xor' c a b = opInst (DB.xor) c a b >> (say $ show ("xor'", c, a, b))
xnor c a b = opInst (\x y -> DB.complement (DB.xor x y)) c a b >> (say $ show ("xnor", c, a, b))

sra rc ra rb = do
  incPC
  x <- getReg ra
  shiftAmt <- liftM fromIntegral (getReg rb)
  let base = shiftR x shiftAmt

  if testBit x 31
     -- x[31] is 1 so fill left with ones, consider sra by 4, then need
     -- 1111000 .. 0000
    then let mask = shiftL 0xFFFFFFFF (31 - shiftAmt)         
         in setReg rc (fromIntegral (base .|. mask))
            -- x[31] is 0 so fill left with zeros, consider sra by 4,
            -- then need 00001111 ..1111
    else let mask = shiftR 0xFFFFFFFF shiftAmt
         in setReg rc (fromIntegral (base .&. mask))
  
runOP :: Instruction -> Mac ()
runOP inst@(OP (Opcode n) rc ra rb) = do
  let msg = "runOP finds an invalid instruction: " ++ (show inst)
      f = case n of
            0x20 -> add 
            0x28 -> and 
            0x24 -> cmpeq
            0x26 -> cmple
            0x25 -> cmplt
            0x23 -> divide
            0x22 -> mul
            0x29 -> or
            0x2C -> shl
            0x2D -> shr
            0x2E -> sra
            0x21 -> sub
            0x2B -> xnor
            0x2A -> xor'
            _ -> \_ _ _ -> throwError msg
  f rc ra rb

simpleOPC :: (Word32 -> Word32 -> Word32)
          -> Reg
          -> Reg
          -> Word16
          -> Mac ()
simpleOPC f rc ra lit = do
  incPC
  regRA <- getReg ra
  setReg rc (f regRA (sxt lit))

divc c l a = simpleOPC div c a l >> (say $ show ("divc", c, l, a))
mulc c l a = simpleOPC (*) c a l >> (say $ show ("mulc", c, l, a))
orc c l a = simpleOPC (.|.) c a l >> (say $ show ("orc", c, l, a))
shlc rc lit ra = do
  say $ show ("shlc", rc, lit, ra)
  incPC
  regRA <- getReg ra
  setReg rc (shiftL regRA (fromIntegral (sxt lit)))
shrc rc lit ra = do
  say $ show ("shrc", rc, lit, ra)
  incPC
  regRA <- getReg ra
  setReg rc (shiftR regRA (fromIntegral (sxt lit)))
  
addc c l a  = simpleOPC (+) c a l >> (say $ show ("addc", c, l, a))
subc c l a = simpleOPC (-) c a l >> (say $ show ("subc", c, l, a))
andc c l a = simpleOPC (.&.) c a l >> (say $ show ("andc", c, l, a))
xorc c l a = simpleOPC DB.xor c a l >> (say $ show ("xorc", c, l, a))
xnorc c l a = simpleOPC (\x y -> DB.complement (DB.xor x y)) c a l >>
  (say $ show ("xnorc", c, l, a))

ldr :: Reg -> Word16 -> Mac ()
ldr rc lit = do
  say $ show ("ldr", rc, lit)
  pc <- getPC
  let literal = (lit - (fromIntegral pc)) `div` 4 - 1
  incPC
  let ea = pc + (4 * (sxt literal))
  (WordLoc val _) <- getMem ea
  setReg rc val

runOPC :: Instruction -> Mac ()
runOPC inst@(OPC (Opcode n) rc ra lit) = 
  let msg = "runOPC finds an invalid instruction: " ++ (show inst)
  in case n of
    0x30 -> addc rc lit ra 
    0x38 -> andc rc lit ra
    0x1C -> beq ra lit rc
    0x1D -> bne ra lit rc
    0x34 -> cmpeqc ra lit rc
    0x36 -> cmplec ra lit rc
    0x35 -> cmpltc ra lit rc
    0x33 -> divc rc lit ra
    0x1B -> jmp ra rc
    0x18 -> ld ra lit rc
    0x1F -> ldr rc lit
    0x32 -> mulc rc lit ra
    0x39 -> orc rc lit ra
    0x3C -> shlc rc lit ra
    0x3D -> shrc rc lit ra 
    -- 0x3E -> srac
    0x19 -> st rc lit ra
    0x31 -> subc rc lit ra
    0x3B -> xnorc rc lit ra
    0x3A -> xorc rc lit ra
    _ -> throwError msg
    
step :: Mac ()
step = do 
  inst <- fetch ? "can't step"
  case inst of
      OPC _ _ _ _ -> runOPC inst
      OP _ _ _ _ -> runOP inst

stepN n = do
  if n == 0
    then return ()
    else do step ? ("step failed with this many to go: " ++ (show n))
            stepN (n - 1)

asHex w = "0x" ++ showHex w ""

go :: Mac ()
go = do
  pc1 <- getPC ? "Can't Mach.go"
  (WordLoc w _) <- getMem pc1
  if w == 0 -- if pc contains halt
    then do log <- cpuLog <$> get
            if length log == 0
              then return ()
              else DT.trace (concat (map (++"\n") log)) (return ())
    else do step 
            go

jsonState :: Mac G.JSValue
jsonState = liftM G.toJSON get

currentPos :: Mac (Maybe TP.SourcePos)
currentPos = do
  pc <- getPC
  (WordLoc _ pos) <- getMem pc
  return pos
