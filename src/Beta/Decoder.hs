{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BinaryLiterals #-}

module Beta.Decoder where

import Numeric (showHex, showIntAtBase)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Bits as DB
import           Data.Functor.Identity
import qualified Data.Map as DM
import           Data.Word
import qualified Text.PrettyPrint.Leijen as PP
import           Uasm.Pretty
import qualified Uasm.SymbolTable as SymTab
import           Beta.Types

bitRange :: Word32 -> Int -> Int -> Word32
bitRange x hi lo =
  let n = DB.shiftR x lo
      hi' = 31 - (hi - lo)
      mask = DB.shiftR 0xFFFFFFFF hi'
  in n DB..&. mask

decode :: Word32 -> Mac Instruction
decode word =
  let opcode = mkOpcode $ fromIntegral $ DB.shiftR word 26
      instructionClass = DB.shiftR word 30
      bits x y = toEnum $ fromIntegral $ bitRange word x y
  in case instructionClass of
    0b10 -> -- without literal
      let rc = bits 25 21
          rb = bits 15 11
          ra = bits 20 16
      in return $ OP opcode rc rb ra
    0b11 -> -- with literal
      let rc = bits 25 21
          ra = bits 20 16
          lit = bitRange word 15 0
      in return $ OPC opcode rc ra (fromIntegral lit)
    0b01 -> -- special class
      let rc = bits 25 21
          ra = bits 20 16
          lit = bitRange word 15 0
      in return $ OPC opcode rc ra (fromIntegral lit)
    x -> throwError $ "Invalid instruction: " ++ (showHex x "")


test1 = bitRange 0b000111000 5 3 == 7
test2 = bitRange 0b001111000 6 3 == 15 
test3 = bitRange 0b11111111111111000 4 2 == 6





  
-- mnemonicToOpcode m =
--   case m of
--     ADD -> 0x20
--     ADDC -> 0x30
--     AND -> 0x28
--     ANDC -> 0x38
--     BEQ -> 0x1C
--     BNE -> 0x1D
--     CMPEQ -> 0x24
--     CMPEQC -> 0x34
--     CMPLE -> 0x26
--     CMPLEC -> 0x36
--     CMPLT -> 0x25
--     CMPLTC -> 0x35
--     DIV -> 0x23
--     DIVC -> 0x33
--     JMP -> 0x1B
--     LD -> 0x18
--     LDR -> 0x1F
--     MUL -> 0x22
--     MULC -> 0x32
--     OR -> 0x29
--     ORC -> 0x39
--     SHL -> 0x2C
--     SHLC -> 0x3C
--     SHR -> 0x2D
--     SHRC -> 0x3D
--     SRA -> 0x2E
--     SRAC -> 0x3E
--     ST -> 0x19
--     SUB -> 0x21
--     SUBC -> 0x31
--     XNOR -> 0x2B
--     XNORC -> 0x3B
--     XOR -> 0x2A
--     XORC -> 0x3A

opcodeToMnemonic (Opcode n) =
  case n of
    0x20 -> ADD
    0x30 -> ADDC
    0x28 -> AND
    0x38 -> ANDC
    0x1C -> BEQ
    0x1D -> BNE
    0x24 -> CMPEQ
    0x34 -> CMPEQC
    0x26 -> CMPLE
    0x36 -> CMPLEC
    0x25 -> CMPLT
    0x35 -> CMPLTC
    0x23 -> DIV
    0x33 -> DIVC
    0x1B -> JMP
    0x18 -> LD
    0x1F -> LDR
    0x22 -> MUL
    0x32 -> MULC
    0x29 -> OR
    0x39 -> ORC
    0x2C -> SHL
    0x3C -> SHLC
    0x2D -> SHR
    0x3D -> SHRC
    0x2E -> SRA
    0x3E -> SRAC
    0x19 -> ST
    0x21 -> SUB
    0x31 -> SUBC
    0x2B -> XNOR
    0x3B -> XNORC
    0x2A -> XOR
    0x3A -> XORC
