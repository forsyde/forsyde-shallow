{-# LANGUAGE TemplateHaskell #-}

-- *** Shallow-Embedded ForSyDe Model ***
--
-- The following example is designed as tutorial example for the paper.
--
-- It implements a simple ALU, which has the following modes
--   HH: out <- a AND b 
--   HL: out <- a OR b 
--   LH: (cout, out) <- ADD a b
--   LL: out <- shiftl a 0
--
 
module ALU_Shallow where

import SynchronousLib
import ForSyDe.Bit
import Data.Bits
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
--import CarrySelectAdder


----- AND - 4 Bit

and4Bit :: Signal (FSVec D4 Bit) 
        -> Signal (FSVec D4 Bit) 
        -> Signal (FSVec D4 Bit)
and4Bit = zipWithSY (Data.Param.FSVec.zipWith (.&.)) 

----- OR - 4 Bit

or4Bit :: Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit) 
            -> Signal (FSVec D4 Bit)
or4Bit =  zipWithSY (Data.Param.FSVec.zipWith (.|.))

----- Logical Shift Left



lsl :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
lsl = mapSY shiftl_L where
         shiftl_L v = shiftl v L  

----- Mux 4/1 (no use of FSVec as input, only for selection)

mux41Fun :: FSVec D2 Bit 
            -> FSVec D4 Bit  
            -> FSVec D4 Bit  
            -> FSVec D4 Bit  
            -> FSVec D4 Bit  
            -> FSVec D4 Bit
mux41Fun sel x3 x2 x1 x0 = if sel == H +> H +> empty then
                              x3
                           else 
                              if sel == H +> L +> empty then
                                 x2
                              else
                                 if sel == L +> H +> empty then 
                                    x1
                                 else
                                    x0
                         

mux41 :: Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit) 
          -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)  
          -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)
mux41 = zipWith5SY mux41Fun

----- Convert FSVector to Tuple (Size 4)

convFromFSVec4 :: FSVec D4 a -> (a, a, a, a)
convFromFSVec4 v = (v!d3, v!d2, v!d1, v!d0)

----- Convert To FSVector (Size 4)

convToFSVec4 :: a -> a -> a -> a -> FSVec D4 a
convToFSVec4 x3 x2 x1 x0 = x3 +> x2 +> x1 +> x0 +> empty

----- 4-Bit Adder

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
		  where (c_out, sum3) = unzipSY $ zipWith3SY fullAdd a3 b3 c2
		        (c2, sum2)    = unzipSY $ zipWith3SY fullAdd a2 b2 c1
		        (c1, sum1)    = unzipSY $ zipWith3SY fullAdd a1 b1 c0
		        (c0, sum0)    = unzipSY $ zipWith3SY fullAdd a0 b0 c_in

fullAdd a b cin = (cout, sum) 
                  where
                     cout = (a .&. b) .|. (a .&. cin) .|. (b .&. cin)
                     sum  = (a `xor` b) `xor` cin

----- FSVec Adder
add4FSVec :: Signal (FSVec D4 Bit)
          -> Signal (FSVec D4 Bit)
          -> (Signal Bit, Signal (FSVec D4 Bit))
add4FSVec a b = (cout, sum)
                 where
                    (a3,a2,a1,a0) = unzip4SY $ mapSY convFromFSVec4 a
                    (b3,b2,b1,b0) = unzip4SY $ mapSY convFromFSVec4 b 
                    (cout, s3, s2, s1, s0) = fourBitAdder zero a3 a2 a1 a0 b3 b2 b1 b0                
                    sum = zipWith4SY convToFSVec4 s3 s2 s1 s0
                    zero = sourceSY id L         

----- ALU

alu :: Signal(FSVec D2 Bit) 
    -> Signal (FSVec D4 Bit)
    -> Signal (FSVec D4 Bit)
     ->  (Signal Bit, Signal (FSVec D4 Bit))
alu sel a b = (cout, out)
              where
                  andOut = and4Bit a b
                  orOut = or4Bit a b
                  (cout, sum) = add4FSVec a b
                  lslOut = lsl a
                  out = mux41 sel andOut orOut sum lslOut

                  

----- Test-Inputs

sel = signal [reallyUnsafeVector [H,H], -- AND
       reallyUnsafeVector [H,L], -- OR
       reallyUnsafeVector [L,H], -- ADD
       reallyUnsafeVector [L,L]] -- LSL

a = signal [reallyUnsafeVector [L,H,H,H],
     reallyUnsafeVector [L,H,L,H],
     reallyUnsafeVector [L,H,H,H],
     reallyUnsafeVector [L,H,H,H]]

b = signal [reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [H,H,L,H],
     reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [H,H,H,H]]

c = signal [reallyUnsafeVector [L,L,L,L], -- LSB D1 D2 MSB
     reallyUnsafeVector [H,L,L,L],
     reallyUnsafeVector [L,L,L,H],
     reallyUnsafeVector [L,L,L,H]]

d = signal [reallyUnsafeVector [L,L,L,L],
     reallyUnsafeVector [L,L,L,L],
     reallyUnsafeVector [L,L,L,H],
     reallyUnsafeVector [H,H,H,H]]

x0 = signal [L,L,L,L]
x1 = signal [H,H,H,H]
