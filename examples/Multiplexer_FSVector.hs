{-# LANGUAGE TemplateHaskell #-}
-- multiplexer
module Multiplexer_FSVector where

import ForSyDe
import Language.Haskell.TH.Lift
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- I used tuples, but a vector of bits is what I would like to have
selectf :: ProcFun(FSVec D2 Bit -> FSVec D4 Bit -> Bit)
selectf = $(newProcFun [d| select1 :: FSVec D2 Bit 
				   -> FSVec D4 Bit -> Bit   
			   select1 sel input =
		             if (sel!d1 == L) && (sel!d0 == L) then
				input!d0
			     else 
				if (sel!d1 == L) && (sel!d0 == H) then
				   input!d1
				else
				   if (sel!d1 == H) && (sel!d0 == L) then
				      input!d2
				   else
				      input!d3   |])


-- System function (or process) which uses 'selectf'
selectProc :: Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit) -> Signal Bit
selectProc = zipWithSY "select1" selectf

-- System definition associated to the system process 'selectProc' 
muxSysDef :: SysDef (Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit) -> Signal Bit)
muxSysDef = $(newSysDefTHName 'selectProc ["sel", "data"] ["out1"])

-- we simulate the system
simMux :: [FSVec D2 Bit] -> [FSVec D4 Bit] -> [Bit]
simMux = simulate muxSysDef

selIn = [$(vectorTH [L,L]), $(vectorTH [H,L]), $(vectorTH [L,H]) ]
dataIn = [$(vectorTH [L,H,H,H]), $(vectorTH [H,L,H,H]), $(vectorTH [H,H,L,H])]

-- Create VHDL
createVHDL = writeVHDL muxSysDef

