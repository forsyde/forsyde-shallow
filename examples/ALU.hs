{-# LANGUAGE TemplateHaskell #-}

module SimpleALU where

import ForSyDe
import Data.Bits
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
import CarrySelectAdder
--import Multiplexer_FSVector

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
and4BitSys =  $(newSysDef 'and4BitProc ["a", "b"] ["y"])

simAnd4Bit :: [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simAnd4Bit = $(simulate 'and4BitSys)

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
or4BitSys =  $(newSysDef 'or4BitProc ["a", "b"] ["y"])

simOr4Bit :: [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simOr4Bit = $(simulate 'or4BitSys)

vhdlOr4Bit = writeVHDL or4BitSys

----- LSL 

lslFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit) 
lslFun = $(newProcFun [d| lslFun :: FSVec D4 Bit -> FSVec D4 Bit
                          lslFun a = shiftl a L
                        |])

lslProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
lslProc = mapSY "lsl" lslFun

lslSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit))
lslSys = $(newSysDef 'lslProc ["in"] ["out"])

simLsl :: [FSVec D4 Bit] -> [FSVec D4 Bit]
simLsl = $(simulate 'lslSys)

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
mux41Sys = $(newSysDef 'mux41Proc ["sel", "d3", "d2", "d1", "d0"] ["out"])

simMux41 :: [FSVec D2 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit] 
         -> [FSVec D4 Bit] -> [FSVec D4 Bit] -> [FSVec D4 Bit]
simMux41 = $(simulate 'mux41Sys)

vhdlMux41 = writeVHDL mux41Sys

----- Convert FSVector to Tuple (Size 4)

convFromFSVec4 :: FSVec D4 a -> (a, a, a, a)
convFromFSVec4 v = (v!d3, v!d2, v!d1, v!d0)

convFromFSVec4Fun :: ProcFun (FSVec D4 Bit -> (Bit, Bit, Bit, Bit))
convFromFSVec4Fun = $(newProcFun [d| convFromFSVec4Fun v 
                                       = convFromFSVec4 v 
                                   |])

convFromFSVec4Proc :: Signal (FSVec D4 Bit) -> (Signal Bit, Signal Bit, Signal Bit, Signal Bit)
convFromFSVec4Proc = (unzip4SY "unzip4" . mapSY "conv4" convFromFSVec4Fun)   

convFromFSVec4Sys :: SysDef (Signal (FSVec D4 Bit) -> (Signal Bit, Signal Bit, Signal Bit, Signal Bit))
convFromFSVec4Sys = $(newSysDef 'convFromFSVec4Proc ["vector4"] ["v3", "v2", "v1", "v0"])

simConvFromFSVec4 :: [FSVec D4 Bit] -> ([Bit], [Bit], [Bit], [Bit])
simConvFromFSVec4 = $(simulate 'convFromFSVec4Sys)

vhdlConvFromFSVec4 =  writeVHDL convFromFSVec4Sys 

----- Convert To FSVector (Size 4)

convToFSVec4 :: a -> a -> a -> a -> FSVec D4 a
convToFSVec4 x3 x2 x1 x0 = x3 +> x2 +> x1 +> x0 +> empty

convToFSVec4Fun :: ProcFun (Bit -> Bit -> Bit -> Bit -> FSVec D4 Bit)
convToFSVec4Fun = $(newProcFun [d| convToFSVec4Fun :: Bit -> Bit -> Bit -> Bit -> FSVec D4 Bit
                                   convToFSVec4Fun x3 x2 x1 x0 
                                      = convToFSVec4 x3 x2 x1 x0 |])

convToFSVec4Proc :: Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D4 Bit)
convToFSVec4Proc = zipWith4SY "toVector4" convToFSVec4Fun

convToFSVec4Sys :: SysDef (Signal Bit -> Signal Bit -> Signal Bit -> Signal Bit -> Signal (FSVec D4 Bit))
convToFSVec4Sys =  $(newSysDef 'convToFSVec4Proc ["x3", "x2", "x1", "x0"] ["vec4"])

simConvToFSVec4 = $(simulate 'convToFSVec4Sys)

vhdlConvToFSVec4 = writeVHDL convToFSVec4Sys


----- FSVec Adder
add4FSVecProc :: Signal (FSVec D4 Bit)
              -> Signal (FSVec D4 Bit)
              -> (Signal Bit, Signal (FSVec D4 Bit))
add4FSVecProc a b = (cout, sum)
                    where
                       (a3,a2,a1,a0) = $(instantiate "split_a" 'convFromFSVec4Sys) a
                       (b3,b2,b1,b0) = $(instantiate "split_b" 'convFromFSVec4Sys) b 
                       (cout, s3, s2, s1, s0) = $(instantiate "adder" 'fourBitAdderSys) zero a3 a2 a1 a0 b3 b2 b1 b0                
                       sum = $(instantiate "merge" 'convToFSVec4Sys) s3 s2 s1 s0
                       zero = $(instantiate "Zero" 'zeroSys)          

add4FSVecSys :: SysDef (Signal (FSVec D4 Bit)
              -> Signal (FSVec D4 Bit)
              -> (Signal Bit, Signal (FSVec D4 Bit)))
add4FSVecSys = $(newSysDef 'add4FSVecProc ["a", "b"] ["cout", "sum"])

simAdd4FSVecSys = $(simulate 'add4FSVecSys)

vhdlAdd4FSVecSys = writeVHDL add4FSVecSys


----- ALU

aluProc :: Signal(FSVec D2 Bit) 
        ->  Signal (FSVec D4 Bit)
        ->  Signal (FSVec D4 Bit)
        ->  (Signal Bit, Signal (FSVec D4 Bit))
aluProc sel a b = (cout, out)
                  where
                     andOut = $(instantiate "and" 'and4BitSys) a b
                     orOut = $(instantiate "or" 'or4BitSys) a b
                     (cout, sum) = $(instantiate "add" 'add4FSVecSys) a b
                     lslOut = $(instantiate "lsl" 'lslSys) a
                     out =  $(instantiate "mux" 'mux41Sys) sel andOut orOut sum lslOut

                     

aluSys :: SysDef (Signal(FSVec D2 Bit) 
              ->  Signal (FSVec D4 Bit)
              ->  Signal (FSVec D4 Bit)
              ->  (Signal Bit, Signal (FSVec D4 Bit)))
aluSys = $(newSysDef 'aluProc ["sel", "a", "b"] ["cout", "data"])
    

simALU :: [FSVec D2 Bit] ->  [FSVec D4 Bit] -> [FSVec D4 Bit] -> ([Bit], [FSVec D4 Bit])
simALU = $(simulate 'aluSys)

vhdlALU = writeVHDL aluSys
                                      

----- Test-Inputs

sel = [reallyUnsafeVector [H,H], 
       reallyUnsafeVector [H,L],
       reallyUnsafeVector [L,H],
       reallyUnsafeVector [L,L]]

a = [reallyUnsafeVector [L,H,H,H],
     reallyUnsafeVector [L,H,L,H],
     reallyUnsafeVector [L,H,H,H],
     reallyUnsafeVector [L,H,H,H]]

b = [reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [H,H,L,H],
     reallyUnsafeVector [H,H,H,H],
     reallyUnsafeVector [H,H,H,H]]