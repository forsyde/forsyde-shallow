{-# LANGUAGE TemplateHaskell #-}

-- This modules implements an Linear Feedback Shiftregister

-- Source:
-- Ben Cohen. VHDL Answers to Frequently Answered Questions. Kluwer. 1998

module LFSR where

import ForSyDe     -- Definition of Bit data type
import Data.Bits   -- Operations for Bit data type

-- ********************
--
-- Naive Implementation
--
-- ********************

-- Design has one XOR-component
xor2f :: ProcFun (Bit -> Bit -> Bit)
xor2f = $(newProcFun [d|xor2f :: Bit -> Bit -> Bit
                        xor2f x1 x0 = xor x1 x0    |])

xor2Proc :: Signal Bit -> Signal Bit -> Signal Bit
xor2Proc = zipWithSY "xor2" xor2f

xor2Sys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit)
xor2Sys = $(newSysDef 'xor2Proc ["in1", "in0"] ["out0"])


-- Design has four D-Flip-Flops
dffProc :: Signal Bit -> Signal Bit
dffProc = delaySY "dff" H -- Starting Sequence must not be LL...L

dffSys :: SysDef (Signal Bit -> Signal Bit)
dffSys = $(newSysDef 'dffProc ["in1"] ["in0"])

-- Netlist of LFSR
lfsr :: Signal Bit
lfsr = output 
       where output = $(instantiate "xor2" 'xor2Sys) s1 s4
             s1 = $(instantiate "dff1" 'dffSys) output
             s2 = $(instantiate "dff2" 'dffSys) s1
             s3 = $(instantiate "dff3" 'dffSys) s2
             s4 = $(instantiate "dff4" 'dffSys) s3

lfsrSys :: SysDef (Signal Bit)
lfsrSys = $(newSysDef 'lfsr [] ["out1"])

simlfsr :: [Bit]
simlfsr = $(simulate 'lfsrSys)

createVHDL = writeVHDL lfsrSys


