{-# LANGUAGE TemplateHaskell #-}

-- The following example is designed as tutorial example for the paper.
--
-- It implements a simple ALU, which has the following modes
--   HH: out <- a AND b 
--   HL: out <- a OR b 
--   LH: (cout, out) <- ADD a b
--   LL: out <- shiftl a 0
--
-- The expressiveness of the compiler could be clearly improved, if the
-- higher-order function 'zipWith' that works on vectors would be synthesizable. 
-- Check 'and4BitFun'!
--
-- We could improve the example even further, if we use enumeration types for the 
-- multiplexer. So far I have not done it, but should only be a matter of time!
 
module ALU where

import ForSyDe
import Data.Bits
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases


----- AND - 4 Bit

and4BitFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit)
and4BitFun = $(newProcFun [d| and4BitFun :: FSVec D4 Bit 
                                         -> FSVec D4 Bit 
                                         -> FSVec D4 Bit
                              --and4BitFun a b = Data.Param.FSVec.zipWith (.&.) a b 
                              and4BitFun a b =  (a!d3 .&. b!d3) +> (a!d2 .&. b!d2) +> (a!d1 .&. b!d1) +> (a!d0 .&. b!d0) +> empty
                            |])


and4BitProc :: Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit)
and4BitProc = zipWithSY "and4" and4BitFun
              

and4BitSys :: SysDef (Signal (FSVec D4 Bit) 
                   -> Signal (FSVec D4 Bit) 
                   -> Signal (FSVec D4 Bit))
and4BitSys =  newSysDef and4BitProc "and4Bit" ["a", "b"] ["y"]

simAnd4Bit :: [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simAnd4Bit = simulate and4BitSys

vhdlAnd4Bit = writeVHDL and4BitSys


----- OR - 4 Bit

or4BitFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit)
or4BitFun = $(newProcFun [d| or4BitFun :: FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit
                              --or4BitFun a b = Data.Param.FSVec.zipWith (.&.) a b 
                             or4BitFun a b =  (a!d3 .|. b!d3) +> (a!d2 .|. b!d2) +> (a!d1 .|. b!d1) +> (a!d0 .|. b!d0) +> empty
                            |])


or4BitProc :: Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit)
or4BitProc = zipWithSY "or4" or4BitFun
              

or4BitSys :: SysDef (Signal (FSVec D4 Bit) 
                   -> Signal (FSVec D4 Bit) 
                   -> Signal (FSVec D4 Bit))
or4BitSys =  newSysDef or4BitProc "or4Bit" ["a", "b"] ["y"]

simOr4Bit :: [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simOr4Bit = simulate or4BitSys

vhdlOr4Bit = writeVHDL or4BitSys

----- Logical Shift Left

lslFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit) 
lslFun = $(newProcFun [d| lslFun :: FSVec D4 Bit -> FSVec D4 Bit
                          lslFun a = shiftl a L
                        |])

lslProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
lslProc = mapSY "lsl" lslFun

lslSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit))
lslSys = newSysDef lslProc "lsl" ["in"] ["out"]

simLsl :: [FSVec D4 Bit] -> [FSVec D4 Bit]
simLsl = simulate lslSys

vhdlLsl = writeVHDL lslSys

----- Mux 4/1 (no use of FSVec as input, only for selection)

mux41Fun :: ProcFun (FSVec D2 Bit 
                  -> FSVec D4 Bit  
                  -> FSVec D4 Bit  
                  -> FSVec D4 Bit  
                  -> FSVec D4 Bit  
                  -> FSVec D4 Bit)
mux41Fun = $(newProcFun [d| mux41Fun :: FSVec D2 Bit 
                                     -> FSVec D4 Bit  
                                     -> FSVec D4 Bit  
                                     -> FSVec D4 Bit  
                                     -> FSVec D4 Bit  
                                     -> FSVec D4 Bit
                            mux41Fun sel x3 x2 x1 x0 
                               = if sel == H +> H +> empty then
                                    x3
                                 else 
                                    if sel == H +> L +> empty then
                                       x2
                                    else
                                       if sel == L +> H +> empty then 
                                          x1
                                       else
                                          x0
                          |])

mux41Proc :: Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit) 
          -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)  
          -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)
mux41Proc = zipWith5SY "mux41" mux41Fun

mux41Sys :: SysDef (Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit) 
                 -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)  
                 -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit))
mux41Sys = newSysDef mux41Proc "mux41" ["sel", "d3", "d2", "d1", "d0"] ["out"]

simMux41 :: [FSVec D2 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit] 
         -> [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simMux41 = simulate mux41Sys

vhdlMux41 = writeVHDL mux41Sys

----- Convert FSVector to Tuple (Size 4)

convFromFSVec4Fun :: ProcFun (FSVec D4 Bit -> (Bit, Bit, Bit, Bit))
convFromFSVec4Fun = $(newProcFun [d| convFromFSVec4Fun :: FSVec D4 Bit -> (Bit, Bit, Bit, Bit)
				     convFromFSVec4Fun v
                                       = (v!d3, v!d2, v!d1, v!d0)
                                   |])

convFromFSVec4Proc :: Signal (FSVec D4 Bit) -> (Signal Bit, Signal Bit, Signal Bit, Signal Bit)
convFromFSVec4Proc = (unzip4SY "unzip4" . mapSY "conv4" convFromFSVec4Fun)   

convFromFSVec4Sys :: SysDef (Signal (FSVec D4 Bit) -> (Signal Bit, Signal Bit, Signal Bit, Signal Bit))
convFromFSVec4Sys = newSysDef convFromFSVec4Proc "convFromFSVec" ["vector4"] ["v3", "v2", "v1", "v0"]

simConvFromFSVec4 :: [FSVec D4 Bit] -> ([Bit], [Bit], [Bit], [Bit])
simConvFromFSVec4 = simulate convFromFSVec4Sys

vhdlConvFromFSVec4 =  writeVHDL convFromFSVec4Sys 

----- Convert To FSVector (Size 4)

convToFSVec4Fun :: ProcFun (Bit -> Bit -> Bit -> Bit -> FSVec D4 Bit)
convToFSVec4Fun = $(newProcFun [d| convToFSVec4Fun :: Bit -> Bit -> Bit -> Bit -> FSVec D4 Bit
                                   convToFSVec4Fun x3 x2 x1 x0 
                                      = x3 +> x2 +> x1 +> x0 +> empty |])

convToFSVec4Proc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D4 Bit)
convToFSVec4Proc = zipWith4SY "toVector4" convToFSVec4Fun

convToFSVec4Sys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D4 Bit))
convToFSVec4Sys =  newSysDef convToFSVec4Proc "convToFSVec" ["x3", "x2", "x1", "x0"] ["vec4"]

simConvToFSVec4 = simulate convToFSVec4Sys

vhdlConvToFSVec4 = writeVHDL convToFSVec4Sys

------ Full Adder

fullAddFun :: ProcFun (Bit -> Bit -> Bit -> (Bit, Bit))
fullAddFun = $(newProcFun 
  [d|fullAddFun :: Bit -> Bit -> Bit -> (Bit, Bit)
     fullAddFun a b c_in = (c_out, sum)
       where c_out :: Bit
             c_out = (a .&. b) .|. (a .&. c_in) .|. (b .&. c_in)
             sum :: Bit
             sum = (a `xor` b) `xor` c_in  |])
   

--fullAddProc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal (Bit, Bit)
--fullAddProc = zipWith3SY "fulladd" fullAddFun
fullAddProc :: Signal Bit -> Signal Bit -> Signal Bit -> (Signal Bit, Signal Bit)
fullAddProc a b c_in = (unzipSY "unzipSY") $ (zipWith3SY "fulladd" fullAddFun a b c_in)

fullAddSys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> (Signal Bit, Signal Bit))
fullAddSys = newSysDef fullAddProc "fullAddSys" ["a", "b", "c_in"] ["cout", "sum"]

simFullAdd :: [Bit] -> [Bit] -> [Bit] -> ([Bit], [Bit])
simFullAdd = simulate fullAddSys

--vhdlFullAdd = writeVHDL fullAddSys

a_in = [L,L,L,L,H,H,H,H]
b_in = [L,H,L,H,L,H,L,H]
c_in = [L,L,H,H,L,L,H,H]

------ 4-bit Adder Chain

fourBitAdder :: Signal Bit   -- C_IN
	     -> Signal Bit   -- A3
             -> Signal Bit   -- A2
             -> Signal Bit   -- A1
	     -> Signal Bit   -- A0
             -> Signal Bit   -- B3
	     -> Signal Bit   -- B2
	     -> Signal Bit   -- B1
	     -> Signal Bit   -- B0
	     -> (Signal Bit, -- C_OUT
	         Signal Bit, -- SUM3
	         Signal Bit, -- SUM2
	         Signal Bit, -- SUM1
	         Signal Bit) -- SUM0
fourBitAdder c_in a3 a2 a1 a0 b3 b2 b1 b0 
                = (c_out, sum3, sum2, sum1, sum0)
		  where (c_out, sum3) = (instantiate "add3" fullAddSys) a3 b3 c2
		        (c2, sum2)    = (instantiate "add2" fullAddSys) a2 b2 c1
		        (c1, sum1)    = (instantiate "add1" fullAddSys) a1 b1 c0
		        (c0, sum0)    = (instantiate "add0" fullAddSys) a0 b0 c_in

fourBitAdderSys :: SysDef (Signal Bit   -- C_IN
			-> Signal Bit   -- A3
			-> Signal Bit   -- A2
			-> Signal Bit   -- A1
			-> Signal Bit   -- A0
			-> Signal Bit   -- B3
			-> Signal Bit   -- B2
			-> Signal Bit   -- B1
			-> Signal Bit   -- B0
			-> (Signal Bit, -- C_OUT
			    Signal Bit, -- SUM3
			    Signal Bit, -- SUM2
			    Signal Bit, -- SUM1
			    Signal Bit)) -- SUM0 	
fourBitAdderSys = newSysDef fourBitAdder "fourBitAdder" ["C_IN", "A3", "A2", "A1", "A0",
                                             "B3", "B2", "B1", "B0"]
                                            ["C_OUT", "SUM3", "SUM2", "SUM1", "SUM0"]

simFourBitAdder = simulate fourBitAdderSys

vhdlFourBitAdder = writeVHDL fourBitAdderSys

a0 = [L,L,L,L,L,L,L,L,H,H,H,H,H,H,H,H]
a1 = [L,L,L,L,H,H,H,H,L,L,L,L,H,H,H,H]
a2 = [L,L,H,H,L,L,H,H,L,L,H,H,L,L,H,H]
a3 = [L,H,L,H,L,H,L,H,L,H,L,H,L,H,L,H]
zero = [L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L]
one = [H,H,H,H,H,H,H,H,H,H,H,H,H,H,H,H]


simAdd_0 = simFourBitAdder zero a3 a2 a1 a0 a3 a2 a1 a0 
simAdd_1 = simFourBitAdder one a3 a2 a1 a0 a3 a2 a1 a0  


----- FSVec Adder
add4FSVecProc :: Signal (FSVec D4 Bit)
              -> Signal (FSVec D4 Bit)
              -> (Signal Bit, Signal (FSVec D4 Bit))
add4FSVecProc a b = (cout, sum)
                    where
                       (a3,a2,a1,a0) = (instantiate "split_a" convFromFSVec4Sys) a
                       (b3,b2,b1,b0) = (instantiate "split_b" convFromFSVec4Sys) b 
                       (cout, s3, s2, s1, s0) = (instantiate "adder" fourBitAdderSys) zero a3 a2 a1 a0 b3 b2 b1 b0                
                       sum = (instantiate "merge" convToFSVec4Sys) s3 s2 s1 s0
                       zero = instantiate "Zero" zeroSys          

add4FSVecSys :: SysDef (Signal (FSVec D4 Bit)
              -> Signal (FSVec D4 Bit)
              -> (Signal Bit, Signal (FSVec D4 Bit)))
add4FSVecSys = newSysDef add4FSVecProc "add4FSVec" ["a", "b"] ["cout", "sum"]

simAdd4FSVecSys = simulate add4FSVecSys

vhdlAdd4FSVecSys = writeVHDL add4FSVecSys

----- Constant Signals

-- Constant input 'H' modeled with constSY
oneProc :: Signal Bit
oneProc = constSY "high" H

oneSys :: SysDef (Signal Bit)
oneSys = newSysDef oneProc "one" [] ["one"]

-- Constant input 'L' modeled with constSY
zeroProc :: Signal Bit
zeroProc = constSY "low" L
 
zeroSys :: SysDef (Signal Bit)
zeroSys = newSysDef zeroProc "zero" [] ["zero"]

----- ALU

aluProc :: Signal(FSVec D2 Bit) 
        ->  Signal (FSVec D4 Bit)
        ->  Signal (FSVec D4 Bit)
        ->  (Signal Bit, Signal (FSVec D4 Bit))
aluProc sel a b = (cout, out)
                  where
                     andOut = (instantiate "and" and4BitSys) a b
                     orOut = (instantiate "or" or4BitSys) a b
                     (cout, sum) = (instantiate "add" add4FSVecSys) a b
                     lslOut = (instantiate "lsl" lslSys) a
                     out =  (instantiate "mux" mux41Sys) sel andOut orOut sum lslOut

                     

aluSys :: SysDef (Signal(FSVec D2 Bit) 
              ->  Signal (FSVec D4 Bit)
              ->  Signal (FSVec D4 Bit)
              ->  (Signal Bit, Signal (FSVec D4 Bit)))
aluSys = newSysDef aluProc "alu" ["sel", "a", "b"] ["cout", "data"]
    

simALU :: [FSVec D2 Bit] ->  [FSVec D4 Bit] -> [FSVec D4 Bit] -> ([Bit], [FSVec D4 Bit])
simALU = simulate aluSys

vhdlALU = writeVHDL aluSys
                                      

----- Test-Inputs

sel = [reallyUnsafeVector [H,H], -- AND
       reallyUnsafeVector [H,L], -- OR
       reallyUnsafeVector [L,H], -- ADD
       reallyUnsafeVector [L,L]] -- LSL

a = [reallyUnsafeVector [L,H,H,H], -- LSB D1 D2 MSB
     reallyUnsafeVector [L,H,L,H],
     reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [L,H,H,H]]

b = [reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [H,H,L,H],
     reallyUnsafeVector [H,L,L,L], 
     reallyUnsafeVector [H,H,H,H]]
