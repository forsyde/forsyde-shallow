{-# LANGUAGE TemplateHaskell #-}

-- This module models a 16-bit carry select adder


module CarrySelectAdder where

import ForSyDe     -- Definition of Bit data type
import Data.Bits   -- Operations for Bit data type
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- First we create a four bit carry select adder.
-- It uses eight full adders and five 2/1 muxes.

------ Full Adder

fullAddFun :: ProcFun (Bit -> Bit -> Bit -> (Bit, Bit))
fullAddFun = $(newProcFun [d|fullAddFun :: Bit -> Bit -> Bit -> (Bit, Bit)
                             -- I would like to use 'where'-clauses. Is this possible?
                             -- fullAddFun a b c_in = (c_out, sum)
                             --  where c_out = (a .&. b) .|. (a .&. c_in) .|. (b .&. c_in)
                             --        sum = (a `xor` b) `xor` c_in  |])
                             fullAddFun a b c_in = ((a .&. b) .|. (a .&. c_in) .|. (b .&. c_in),
						    (a `xor` b) `xor` c_in) |])

--fullAddProc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal (Bit, Bit)
--fullAddProc = zipWith3SY "fulladd" fullAddFun
fullAddProc :: Signal Bit -> Signal Bit -> Signal Bit -> (Signal Bit, Signal Bit)
fullAddProc a b c_in = (unzipSY "unzipSY") $ (zipWith3SY "fulladd" fullAddFun a b c_in)

fullAddSys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> (Signal Bit, Signal Bit))
fullAddSys = $(newSysDef 'fullAddProc ["a", "b", "c_in"] ["cout", "sum"])

simFullAdd :: [Bit] -> [Bit] -> [Bit] -> ([Bit], [Bit])
simFullAdd = $(simulate 'fullAddSys)

--vhdlFullAdd = writeVHDL fullAddSys

a_in = [L,L,L,L,H,H,H,H]
b_in = [L,H,L,H,L,H,L,H]
c_in = [L,L,H,H,L,L,H,H]


------ 2/1 Multiplexer

--mux21Fun :: ProcFun (Bit -> Bit -> Bit -> Bit)
--mux21Fun = $(newProcFun [d|mux21Fun :: Bit -> Bit -> Bit -> Bit
--                           mux21Fun sel d1 d0 = if (sel == L) then
--                                                   d0
--                                                else
--                                                   d1 |])

mux21Proc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit
mux21Proc = zipWith3SY "mux21" mux21Fun

mux21Sys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit)
mux21Sys = $(newSysDef 'mux21Proc ["sel", "d1", "d0"] ["out"])

simMux21 :: [Bit] -> [Bit] -> [Bit] -> [Bit]
simMux21 = $(simulate 'mux21Sys)

vhdlMux21 = writeVHDL mux21Sys

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
		  where (c_out, sum3) = $(instantiate "add3" 'fullAddSys) a3 b3 c2
		        (c2, sum2)    = $(instantiate "add2" 'fullAddSys) a2 b2 c1
		        (c1, sum1)    = $(instantiate "add1" 'fullAddSys) a1 b1 c0
		        (c0, sum0)    = $(instantiate "add0" 'fullAddSys) a0 b0 c_in

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
fourBitAdderSys = $(newSysDef 'fourBitAdder ["C_IN", "A3", "A2", "A1", "A0",
                                             "B3", "B2", "B1", "B0"]
                                            ["C_OUT", "SUM3", "SUM2", "SUM1", "SUM0"])

simFourBitAdder = $(simulate 'fourBitAdderSys)

vhdlFourBitAdder = writeVHDL fourBitAdderSys

a0 = [L,L,L,L,L,L,L,L,H,H,H,H,H,H,H,H]
a1 = [L,L,L,L,H,H,H,H,L,L,L,L,H,H,H,H]
a2 = [L,L,H,H,L,L,H,H,L,L,H,H,L,L,H,H]
a3 = [L,H,L,H,L,H,L,H,L,H,L,H,L,H,L,H]
zero = [L,L,L,L,L,L,L,L,L,L,L,L,L,L,L,L]
one = [H,H,H,H,H,H,H,H,H,H,H,H,H,H,H,H]


simAdd_0 = simFourBitAdder zero a3 a2 a1 a0 a3 a2 a1 a0 
simAdd_1 = simFourBitAdder one a3 a2 a1 a0 a3 a2 a1 a0  

----- fourBitCarrySelectAdder


fourBitCSAdder :: Signal Bit   -- C_IN
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
	          Signal Bit) -- SUM0fourBitCSAdder :: 
fourBitCSAdder c_in a3 a2 a1 a0 b3 b2 b1 b0
               = (c_out, sum3, sum2, sum1, sum0)
                 where c_out = $(instantiate "mux4" 'mux21Sys) c_in c_out_1 c_out_0
                       sum3  = $(instantiate "mux3" 'mux21Sys) c_in sum3_1 sum3_0
                       sum2  = $(instantiate "mux2" 'mux21Sys) c_in sum2_1 sum2_0
                       sum1  = $(instantiate "mux1" 'mux21Sys) c_in sum1_1 sum1_0 
                       sum0  = $(instantiate "mux0" 'mux21Sys) c_in sum0_1 sum0_0
                       (c_out_1, sum3_1, sum2_1, sum1_1, sum0_1) 
                             = $(instantiate "Adder_1" 'fourBitAdderSys) 
                                one a3 a2 a1 a0 b3 b2 b1 b0
                       (c_out_0, sum3_0, sum2_0, sum1_0, sum0_0) 
                             = $(instantiate "Adder_1" 'fourBitAdderSys) 
                                zero a3 a2 a1 a0 b3 b2 b1 b0
                       one = $(instantiate "One" 'oneSys)
                       zero = $(instantiate "Zero" 'zeroSys)

fourBitCSAdderSys = $(newSysDef 'fourBitCSAdder ["C_IN", "A3", "A2", "A1", "A0",
                                             "B3", "B2", "B1", "B0"]
                                            ["C_OUT", "SUM3", "SUM2", "SUM1", "SUM0"])


simFourBitCSAdder = $(simulate 'fourBitCSAdderSys)

vhdlFourBitCSAdder = writeVHDL fourBitCSAdderSys
                           
simCSAdd_0 = simFourBitAdder zero a3 a2 a1 a0 a3 a2 a1 a0 
simCSAdd_1 = simFourBitAdder one a3 a2 a1 a0 a3 a2 a1 a0  

----- Convert Output to FSVec

convertToFSVec5 :: a -> a -> a -> a -> a -> FSVec D5 a
convertToFSVec5 x4 x3 x2 x1 x0 = x4 +> x3 +> x2 +> x1 +> (singleton x0)

convOutput :: ProcFun (Bit -> Bit -> Bit -> Bit -> Bit -> FSVec D5 Bit)
convOutput = $(newProcFun [d|convOutput :: Bit -> Bit -> Bit -> Bit -> Bit -> FSVec D5 Bit
                             convOutput x4 x3 x2 x1 x0 
                                    = convertToFSVec5 x4 x3 x2 x1 x0 |])

convOutputProc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D5 Bit)
convOutputProc = zipWith5SY "toVector5" convOutput

convOutputSys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D5 Bit))
convOutputSys =  $(newSysDef 'convOutputProc ["x4", "x3", "x2", "x1", "x0"] ["vec5"])

fourBitCSAdder' :: Signal Bit   -- C_IN
	        -> Signal Bit   -- A3
                -> Signal Bit   -- A2
                -> Signal Bit   -- A1
	        -> Signal Bit   -- A0
                -> Signal Bit   -- B3
	        -> Signal Bit   -- B2
	        -> Signal Bit   -- B1
	        -> Signal Bit   -- B0
	        -> Signal (FSVec D5 Bit) -- <C_OUT, SUM3, SUM2, SUM1, SUM0>
fourBitCSAdder' c_in a3 a2 a1 a0 b3 b2 b1 b0 = out5
               where
                  out5 = $(instantiate "toVector5" 'convOutputSys) c_out sum3 sum2 sum1 sum0
                  (c_out, sum3, sum2, sum1, sum0) = $(instantiate "Adder" 'fourBitCSAdderSys) c_in a3 a2 a1 a0 b3 b2 b1 b0  

fourBitCSAdderSys2 :: SysDef (Signal Bit   -- C_IN
	        -> Signal Bit   -- A3
                -> Signal Bit   -- A2
                -> Signal Bit   -- A1
	        -> Signal Bit   -- A0
                -> Signal Bit   -- B3
	        -> Signal Bit   -- B2
	        -> Signal Bit   -- B1
	        -> Signal Bit   -- B0
	        -> Signal (FSVec D5 Bit))
fourBitCSAdderSys2 = $(newSysDef 'fourBitCSAdder' ["C_IN", "A3", "A2", "A1", "A0",
                                             "B3", "B2", "B1", "B0"]
                                            ["vect5"])

simFourBitCSAdder2 = $(simulate 'fourBitCSAdderSys2)

vhdlFourBitCSAdder2 = writeVHDL fourBitCSAdderSys2

simCSAdd_0' = simFourBitCSAdder2 zero a3 a2 a1 a0 a3 a2 a1 a0 
simCSAdd_1' = simFourBitCSAdder2 one a3 a2 a1 a0 a3 a2 a1 a0  


----- Four Bit Adder 
--
-- The four bit adder is modelled as a set of equations. 
-- It uses fixed-size vectors for the 4-bit input and output values.

adder4BitFun :: ProcFun (Bit -> FSVec D4 Bit -> FSVec D4 Bit -> (Bit, FSVec D4 Bit))
adder4BitFun = $(newProcFun [d|adder4BitFun :: Bit -> FSVec D4 Bit -> FSVec D4 Bit -> (Bit, FSVec D4 Bit)
                               adder4BitFun cin a b = (cout, sum) where
                                  cout = (a!d3 .&. b!d3) .|. (a!d3 .&. c2) .|. (b!d3 .&. c2)
                                  c2 = (a!d2 .&. b!d2) .|. (a!d2 .&. c1) .|. (b!d2 .&. c1)
                                  c1 = (a!d1 .&. b!d1) .|. (a!d1 .&. c0) .|. (b!d1 .&. c0)
                                  c0 = (a!d0 .&. b!d0) .|. (a!d0 .&. cin) .|. (b!d0 .&. cin)
                                  s3 = (a!d3 `xor` b!d3) `xor` c2
                                  s2 = (a!d2 `xor` b!d2) `xor` c1
                                  s1 = (a!d1 `xor` b!d1) `xor` c0
                                  s0 = (a!d0 `xor` b!d0) `xor` cin
                                  sum = s0 +> s1 +> s2 +> s3 +> empty |])

adder4BitProc :: Signal Bit -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> (Signal Bit, Signal (FSVec D4 Bit))
adder4BitProc cin a b = (unzipSY "unzip") $ (zipWith3SY "4BitAdder" adder4BitFun cin a b)

adder4BitSys :: SysDef (Signal Bit -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> (Signal Bit, Signal (FSVec D4 Bit)))
adder4BitSys = $(newSysDef 'adder4BitProc ["cin", "a", "b"] ["cout", "sum"])

simAdder4Bit = $(simulate 'adder4BitSys)

vhdlAdder4Bit = writeVHDL adder4BitSys

----- 4-Bit 2/1 Multiplexer
--
-- The multiplexer uses fixed size vectors for input and output values.

mux21Fun :: ProcFun (Bit -> a -> a -> a)
mux21Fun = $(newProcFun [d|mux21Fun :: Bit -> a -> a -> a
                           mux21Fun sel d1 d0 = if (sel == L) then
                                                   d0
                                                else
                                                   d1 |])

mux21_4BitProc :: Signal Bit -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)
mux21_4BitProc = zipWith3SY "mux21" mux21Fun

mux21_4BitSys :: SysDef (Signal Bit -> Signal ( FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit))
mux21_4BitSys = $(newSysDef 'mux21_4BitProc ["sel", "d1", "d0"] ["out"])

simMux21_4Bit :: [Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simMux21_4Bit = $(simulate 'mux21_4BitSys)

vhdlMux21_4Bit = writeVHDL mux21_4BitSys


----- 4-Bit Carry Select Adder

-- The carry select adder has two input that are connected to 'H' or 'L' respectively

-- Constant input 'H' modeled with constSY
oneProc :: Signal Bit
oneProc = constSY "high" H

oneSys :: SysDef (Signal Bit)
oneSys = $(newSysDef 'oneProc [] ["one"])

-- Constant input 'L' modeled with constSY
zeroProc :: Signal Bit
zeroProc = constSY "low" L
 
zeroSys :: SysDef (Signal Bit)
zeroSys = $(newSysDef 'zeroProc [] ["zero"])

-- The 4-Bit carry select adder is implemented as a composition of components
csAdder4BitProc :: Signal Bit -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> (Signal Bit, Signal (FSVec D4 Bit))
csAdder4BitProc cin a b = (cout, sum) where
                          cout = $(instantiate "mux" 'mux21Sys) cin cout_1 cout_0
                          sum = $(instantiate "mux4Bit" 'mux21_4BitSys) cin sum_1 sum_0
                          (cout_1, sum_1) = $(instantiate "adder1" 'adder4BitSys) one a b
                          (cout_0, sum_0) = $(instantiate "adder0" 'adder4BitSys) zero a b                  
                          one = $(instantiate "One" 'oneSys)
                          zero = $(instantiate "Zero" 'zeroSys)
                           
csAdder4BitSys :: SysDef (Signal Bit -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> (Signal Bit, Signal (FSVec D4 Bit)))
csAdder4BitSys = $(newSysDef 'csAdder4BitProc ["cin", "a", "b"] ["cout", "sum"])

simCSAdder4Bit = $(simulate 'csAdder4BitSys)

vhdlCSAdder4Bit = writeVHDL csAdder4BitSys

----- 16 Bit Adder

csAdder16BitProc :: Signal (FSVec D16 Bit) -> Signal (FSVec D16 Bit) -> (Signal Bit, Signal (FSVec D16 Bit))
csAdder16BitProc a b = (cout, sum) where
                       sum = zipWith4SY "concat4" concat4Fun sum3_0 sum7_4 sum11_8 sum15_12
                       (cout, sum15_12) = $(instantiate "csadder3" 'csAdder4BitSys) c11 a15_12 b15_12
                       (c11, sum11_8) = $(instantiate "csadder2" 'csAdder4BitSys) c7 a11_8 b11_8
                       (c7, sum7_4) = $(instantiate "csadder3" 'csAdder4BitSys) c3 a7_4 b7_4
                       (c3, sum3_0) = $(instantiate "csadder3" 'csAdder4BitSys) zero a3_0 b3_0
                       a15_12 = mapSY "a15_12" select15_12Fun a
                       a11_8  = mapSY "a11_8"  select11_8Fun a
                       a7_4   = mapSY "a7_4"   select7_4Fun a
                       a3_0   = mapSY "a3_0"   select3_0Fun a
                       b15_12 = mapSY "b15_12" select15_12Fun b
                       b11_8  = mapSY "b11_8"  select11_8Fun b
                       b7_4   = mapSY "b7_4"   select7_4Fun b
                       b3_0   = mapSY "b3 _0"  select3_0Fun b
                       zero = $(instantiate "Zero" 'zeroSys)         


csAdder16BitSys = $(newSysDef 'csAdder16BitProc ["a", "b"] ["cout", "sum"])

simCSAdder16Bit = $(simulate 'csAdder16BitSys)

vhdlCSAdder16Bit = writeVHDL csAdder16BitSys

-- Helper functions

select15_12Fun :: ProcFun (FSVec D16 Bit -> FSVec D4 Bit)
select15_12Fun = $(newProcFun [d|select15_12 :: FSVec D16 Bit -> FSVec D4 Bit
                                 select15_12 v = select d12 d1 d4 v |])

select11_8Fun :: ProcFun (FSVec D16 Bit -> FSVec D4 Bit)
select11_8Fun = $(newProcFun [d|select11_8 :: FSVec D16 Bit -> FSVec D4 Bit
                                select11_8 v = select d8 d1 d4 v |])

select7_4Fun :: ProcFun (FSVec D16 Bit -> FSVec D4 Bit)
select7_4Fun = $(newProcFun [d|select7_4 :: FSVec D16 Bit -> FSVec D4 Bit
                               select7_4 v = select d4 d1 d4 v |])

select3_0Fun :: ProcFun (FSVec D16 Bit -> FSVec D4 Bit)
select3_0Fun = $(newProcFun [d|select3_0 :: FSVec D16 Bit -> FSVec D4 Bit
                               select3_0 v = select d0 d1 d4 v |])

concat4Fun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit -> FSVec D16 Bit)
concat4Fun = $(newProcFun [d|concat4Fun :: FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit -> FSVec D16 Bit
                             concat4Fun v1 v2 v3 v4 = v1 Data.Param.FSVec.++ 
                                                      v2 Data.Param.FSVec.++ 
                                                      v3 Data.Param.FSVec.++ 
                                                      v4 |])

-- 16 Bit Test Values

v_0000 = reallyUnsafeVector [L,L,L,L, L,L,L,L, L,L,L,L, L,L,L,L]
v_8000 = reallyUnsafeVector [L,L,L,L, L,L,L,L, L,L,L,L, L,L,L,H] -- H is MSB
v_0001 = reallyUnsafeVector [H,L,L,L, L,L,L,L, L,L,L,L, L,L,L,L]
v_ffff = reallyUnsafeVector [H,H,H,H, H,H,H,H, H,H,H,H, H,H,H,H]

a16 = v_0000 : v_0000 : v_0001 : v_8000 : v_ffff : v_ffff : []
b16 = v_0000 : v_0001 : v_0001 : v_8000 : v_ffff : v_0001 : []

test16Bit = simCSAdder16Bit a16 b16

-- 4 Bit Test Values

v_0 = reallyUnsafeVector [L,L,L,L]
v_1 = reallyUnsafeVector [H,L,L,L]
v_8 = reallyUnsafeVector [L,L,L,H]
v_f = reallyUnsafeVector [H,H,H,H]

a4 = v_0 : v_0 : v_1 : v_8 : v_f : v_1 : []
b4 = v_0 : v_1 : v_1 : v_8 : v_f : v_f : []

c_low = [L,L,L,L,L,L]
c_high = [H,H,H,H,H,H] 

test4Bit = simCSAdder4Bit c_low a4 b4 

-- Test FSVector

fv = 0 +> 1 +> 2 +> 3 +> empty -- LSB fv!d0 = 0, MSB fv!d3 = 3
