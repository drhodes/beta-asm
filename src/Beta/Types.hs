{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Beta.Types where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Bits as DB
import           Data.Functor.Identity
import qualified Data.Map as DM
import           Data.Word
import           Data.Generics
import           Text.JSON.Generic
import qualified Text.PrettyPrint.Leijen as PP
import           Uasm.Pretty
import qualified Uasm.SymbolTable as SymTab
import           Uasm.Types
data Mnemonic = ADD | ADDC | AND | ANDC | BEQ | BNE | CMPEQ 
              | CMPEQC | CMPLE | CMPLEC | CMPLT | CMPLTC | DIV 
              | DIVC | JMP | LD | LDR | MUL | MULC 
              | OR | ORC | SHL | SHLC | SHR 
              | SHRC | SRA | SRAC | ST | SUB 
              | SUBC | XNOR | XNORC | XOR | XORC
              deriving (Show, Eq, Typeable, Data)

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
         | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
         | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23
         | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31 
           deriving (Show, Eq, Ord, Enum, Typeable, Data)

xp = R30
sp = R29
lp = R28
bp = R27

data Lit = Lit Word16
           deriving (Show, Eq, Typeable, Data)

data Opcode = Opcode Int
            deriving (Show, Eq, Typeable, Data)

mkOpcode n = Opcode $ n DB..&. 0b111111

data Instruction = OP Opcode Reg Reg Reg
                 | OPC Opcode Reg Reg Word16
                   deriving (Show, Eq, Typeable, Data)
                   
mkOp n ra rb rc = OP (mkOpcode n) ra rb rc
mkOpc n ra lit rc = OPC (mkOpcode n) ra rc lit

type RegFile = DM.Map Reg Word32

mkRegFile :: RegFile
mkRegFile = DM.fromList $ zip [R0 .. R31] (repeat 0)

sxt :: Word16 -> Word32
sxt n = let topBitIsOne = DB.shiftR n 15 == 1
        in if topBitIsOne
           then 0xFFFF0000 DB..|. (fromIntegral n)
           else 0x00000000 DB..|. (fromIntegral n)

type Ram = DM.Map Word32 Word32

mkRam = DM.empty
  
data Mach = Mach { cpuRegFile :: RegFile
                 , cpuPC :: Word32
                 , cpuRam :: Ram
                 } deriving (Show, Eq, Typeable, Data)

type Mac b = forall m. ( MonadState Mach m,
                         MonadError String m ) => m b

                          
                      
 



