{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BinaryLiterals #-}

module Beta.Mach where

import           Beta.Decoder
import           Beta.Types
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits as DB
import qualified Data.Map as DM
import           Data.Word
import Prelude hiding (and, or, xor)

runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

getReg :: Reg -> Mac Word32
getReg rx = do
  regFile <- liftM cpuRegFile get
  if rx == R31
    then return 0
    else case DM.lookup rx regFile of
      Just w -> return w
      Nothing -> throwError $ "Failed to find register in regfile: " ++ (show rx)

setMem :: Word32 -> Word32 -> Mac ()
setMem addr w = do
  Mach rf pc ram <- get
  put (Mach rf pc (DM.insert addr w ram))
        
setReg :: Reg -> Word32 -> Mac ()
setReg rx w = do
  Mach rf pc ram <- get
  put (Mach (DM.insert rx w rf) pc ram)

incPC :: Mac ()
incPC = do
  Mach rf pc ram <- get
  put (Mach rf (pc + 4) ram)

getPC :: Mac Word32
getPC = do
  Mach _ pc _ <- get
  -- most uses of PC ignore supervisor bit.
  return (pc .|. 0x80000000)

getSuperPC :: Mac Word32
getSuperPC = do
  Mach _ pc _ <- get  
  return pc
  
setPC pc = do
  Mach rf _ ram <- get
  put (Mach rf pc ram)

getMem :: Word32 -> Mac Word32
getMem addr = do
  Mach rf pc ram <- get
  case DM.lookup addr ram of
    Just n -> return n
    Nothing -> throwError $ "Invalid Address: " ++ (show addr)

setSupervisor b = do
  Mach rf pc ram <- get
  let mask = 0x80000000
      pc' = pc .|. mask
  put $ Mach rf pc' ram

store rc lit ra = do
  incPC
  regRA <- getReg ra
  let ea = regRA + sxt lit
  regRC <- getReg rc
  setMem ea regRC

load ra lit rc = do
  incPC
  regRA <- getReg ra
  let ea = regRA + sxt lit
  val <- getMem ea
  setReg rc val


opInst oper rc ra rb = do
  incPC
  rx <- getReg ra
  ry <- getReg rb
  setReg rc (oper (fromIntegral rx) (fromIntegral ry))



beq ra label rc = do
  pc <- getPC
  let literal = (label - (fromIntegral pc)) - 1
  incPC
  let ea = pc + 4 * (sxt (fromIntegral literal))
  temp <- getReg ra
  setReg rc pc
  when (temp == 0) $ setPC ea

bne ra label rc = do
  pc <- getPC
  let literal = (label - (fromIntegral pc)) - 1
  incPC
  let ea = pc + 4 * (sxt (fromIntegral literal))
  temp <- getReg ra
  setReg rc pc
  when (temp /= 0) $ setPC ea

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
  if ra <= rb
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

loadWords words = do
  Mach rf pc _ <- get
  put $ Mach rf pc (DM.fromList $ zip [0..] words) 

fetch :: Mac Instruction
fetch = do
  pc <- getPC
  word <- getMem pc
  decode word

divide c a b = opInst div c a b  
mul c a b = opInst (*) c a b  
or c a b = opInst (.|.) c a b  
shl c a b = opInst (shiftL) c a b
add c a b  = opInst (+) c a b
sub c a b = opInst (-) c a b 
and c a b = opInst (.&.) c a b
shr c a b = opInst (shiftR) c a b
xor' c a b = opInst (DB.xor) c a b
xnor c a b = opInst (\x y -> DB.complement (DB.xor x y)) c a b

sra rc ra rb = do
  incPC
  x <- getReg ra
  shiftAmt <- liftM fromIntegral (getReg rb)
  
  let base = shiftR x shiftAmt
      mask = if testBit x 31
             then shiftL 0xFFFFFFFF (31 - shiftAmt)
             else 0x00000000
                  
  setReg rc (fromIntegral (base .|. mask))
  
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

runOPC _ = undefined

run :: Mac ()
run = do
  -- fetch an instruction
  inst <- fetch
  case inst of
    OPC _ _ _ _ -> runOPC inst
    -- OP _ _ _ _ -> runOP inst
  
  return ()


