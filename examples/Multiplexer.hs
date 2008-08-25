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


-- translate to VHDL, write output and run in QUartus
vhdlMux :: IO ()
vhdlMux = writeVHDLOps defaultVHDLOps{execQuartus=Just quartusOps} muxSysDef
 where quartusOps = QuartusOps FullCompilation
                               (Just 50)
                               (Just ("CycloneII", Just "EP2C35F672C6"))
                               [("sel.tup1","PIN1"),
                                ("sel.tup2","PIN2"),
                                ("data.tup1","PIN3"),
                                ("data.tup2","PIN4"),
                                ("data.tup3","PIN5"),
                                ("data.tup4","PIN6"),
                                ("out1","PIN7")]



