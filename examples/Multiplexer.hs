{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

-- Example implements a multiplexer
module Multiplexer where

import ForSyDe
import Data.Typeable
import Language.Haskell.TH.Lift

-- A data type Bit is really needed!
data Bit = H | L deriving (Eq, Show, Typeable)
$(deriveLift1 ''Bit)

-- I used tuples, but a vector of bits is what I would like to have
selectf :: ProcFun((Bit, Bit) -> (Bit, Bit, Bit, Bit) -> Bit)
selectf = $(newProcFun [d| select :: (Bit, Bit) 
				  -> (Bit, Bit, Bit, Bit) -> Bit   
			   select (s1, s0) (x3, x2, x1, x0) =
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
selectProc = zipWithSY "select" selectf

-- System definition associated to the system process 'selectProc' 
muxSysDef :: SysDef (Signal (Bit, Bit) -> Signal (Bit, Bit, Bit, Bit) -> Signal Bit)
muxSysDef = $(newSysDef 'selectProc ["sel", "data"] ["out"])

-- we simulate the system
simMux :: [(Bit, Bit)] -> [(Bit, Bit, Bit, Bit)] -> [Bit]
simMux = $(simulate 'muxSysDef)

selIn = [(L,L),(L,H),(H,L)]
dataIn = [(L,L,L,H), (L,L,L,H), (L,H,L,L)]
