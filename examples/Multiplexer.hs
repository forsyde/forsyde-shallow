{-# LANGUAGE TemplateHaskell #-}
-- multiplexer
module Multiplexer where

import ForSyDe
import Language.Haskell.TH.Lift

-- I used tuples, but a vector of bits is what I would like to have
selectf :: ProcFun((Bit, Bit) -> (Bit, Bit, Bit, Bit) -> Bit)
selectf = $(newProcFun [d| select1 :: (Bit, Bit) 
				  -> (Bit, Bit, Bit, Bit) -> Bit   
			   select1 (s1, s0) (x3, x2, x1, x0) =
		             if (s1 == L) && (s0 == L) then
				x0
			     else 
				if (s1 == L) && (s0 == H) then
				   x1
				else
				   if (s1 == H) && (s0 == L) then
				      x2
				   else
				      x3   |])


-- System function (or process) which uses 'selectf'
selectProc :: Signal (Bit, Bit) -> Signal (Bit, Bit, Bit, Bit) -> Signal Bit
selectProc = zipWithSY "select1" selectf

-- System definition associated to the system process 'selectProc' 
muxSysDef :: SysDef (Signal (Bit, Bit) -> Signal (Bit, Bit, Bit, Bit) -> Signal Bit)
muxSysDef = $(newSysDefTHName 'selectProc ["sel", "data"] ["out1"])

-- we simulate the system
simMux :: [(Bit, Bit)] -> [(Bit, Bit, Bit, Bit)] -> [Bit]
simMux = simulate muxSysDef

selIn = [(L,L),(L,H),(H,L)]
dataIn = [(L,L,L,H), (L,L,L,H), (L,H,L,L)]


