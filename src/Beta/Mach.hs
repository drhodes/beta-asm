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
import           Data.Functor.Identity
import qualified Text.JSON.Generic as G
import Numeric (showIntAtBase)
import Prelude hiding (and, or, xor)

{-

-}


runStateExceptT :: s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

new = Mach mkRegFile 0 mkRam
fromWords words = Mach mkRegFile 0 (DM.fromList $ zip [0, 4 ..] words) 


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
getPC = cpuPC <$> get
  
setPC pc = do
  Mach rf _ ram <- get
  put (Mach rf pc ram)

getMem :: Word32 -> Mac Word32
getMem addr = do
  Mach rf pc ram <- get
  case DM.lookup addr ram of
    Just n -> return n
    Nothing -> throwError $ "Invalid Address: " ++ (show addr)

reset :: Mac ()
reset = put new

store rc lit ra = do
  incPC
  regRA <- getReg ra
  let ea = regRA + sxt lit
  regRC <- getReg rc
  setMem ea regRC

ld ra lit rc = do
  -- Reg[Rc] ← Mem[Reg[Ra] + SEXT(literal)]
  regRA <- getReg ra
  val <- getMem $ regRA + sxt lit
  setReg rc val

opInst oper rc ra rb = do
  incPC
  rx <- getReg ra
  ry <- getReg rb
  setReg rc (oper (fromIntegral rx) (fromIntegral ry))

beq ra lit rc = do
  -- Reg[Rc] ← PC + 4; if Reg[Ra] = 0 then PC ← PC + 4 + 4*SEXT(literal)
  pc <- getPC
  setReg rc pc
  regRA <- getReg ra
  when (regRA == 0) $ setPC (pc + 4 + 4 * sxt(lit))
  
bne ra lit rc = do
  pc <- getPC
  setReg rc (pc + 4)
  regRA <- getReg ra
  when (regRA /= 0) $ setPC (pc + 4 + 4 * sxt(lit))

st rc lit ra = do
  -- Mem[Reg[Ra] + SEXT(literal)] ← Reg[Rc]
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
  put $ Mach rf pc (DM.fromList $ zip [0, 4 ..] words) 





fetch :: Mac Instruction
fetch = do
  pc <- getPC
  word <- getMem (pc .&. 0x7FFFFFFF)
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

divc c l a = simpleOPC div c a l
mulc c l a = simpleOPC (*) c a l  
orc c l a = simpleOPC (.|.) c a l
shlc rc lit ra = do
  incPC
  regRA <- getReg ra
  setReg rc (shiftL regRA (fromIntegral (sxt lit)))
shrc rc lit ra = do
  incPC
  regRA <- getReg ra
  setReg rc (shiftR regRA (fromIntegral (sxt lit)))
  
addc c l a  = simpleOPC (+) c a l
subc c l a = simpleOPC (-) c a l
andc c l a = simpleOPC (.&.) c a l
xorc c l a = simpleOPC DB.xor c a l
xnorc c l a = simpleOPC (\x y -> DB.complement (DB.xor x y)) c a l

ldr :: Reg -> Word16 -> Mac ()
ldr rc lit = do
  pc <- getPC
  let literal = (lit - (fromIntegral pc)) `div` 4 - 1
  incPC
  let ea = pc + (4 * (sxt literal))
  val <- getMem ea
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
    -- 0x1F -> ldr
    0x32 -> mulc rc lit ra
    0x39 -> orc rc lit ra
    0x3C -> shlc rc lit ra
    0x3D -> shrc rc lit ra 
    -- 0x3E -> srac
    -- 0x19 -> st
    0x31 -> subc rc lit ra
    0x3B -> xnorc rc lit ra
    0x3A -> xorc rc lit ra
    _ -> throwError msg
  
step :: Mac ()
step = do
  inst <- fetch
  case inst of
    OPC _ _ _ _ -> runOPC inst
    OP _ _ _ _ -> runOP inst

stepN n = do
  if n == 0
    then return ()
    else step >> stepN (n - 1)

go :: Mac ()
go = do
  pc1 <- getPC
  step
  pc2 <- getPC
  if pc1 == pc2
    then return ()
    else go
  



jsonState :: Mac G.JSValue
jsonState = liftM G.toJSON get

loadSample1 :: Mac ()
loadSample1 = loadWords [ 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        , 0xAC000000
                        ]
  

