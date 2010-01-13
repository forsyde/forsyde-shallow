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
muxSysDef = newSysDef selectProc "muxSys" ["sel", "data"] ["out1"]

-- we simulate the system
simMux :: [(Bit, Bit)] -> [(Bit, Bit, Bit, Bit)] -> [Bit]
simMux = simulate muxSysDef


selIn = [(L,L),(L,H),(H,L)]
dataIn = [(L,L,L,H), (L,L,L,H), (L,H,L,L)]


-- translate to VHDL, write output and run in QUartus
vhdlMux :: IO ()
vhdlMux = writeVHDLOps defaultVHDLOps{execQuartus=Just quartusOps} muxSysDef
 where quartusOps = QuartusOps FullCompilation
                               (Just 50)
                               (Just ("CycloneII", Just "EP2C35F672C6"))
                               [("sel.tup_1","PIN_AF14"),
                                ("sel.tup_2","PIN_AD13"),
                                ("data.tup_1","PIN_N25"),
                                ("data.tup_2","PIN_N26"),
                                ("data.tup_3","PIN_P25"),
                                ("data.tup_4","PIN_AE14"),
                                ("out1","PIN_AE23")]



