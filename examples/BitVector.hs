{-# LANGUAGE TemplateHaskell #-}
-- example module to test bit conversion functions
module BitVector where

import ForSyDe
import Data.Int (Int32)
import Data.Param.FSVec
import Data.TypeLevel.Num (D32)

to32BitFun :: ProcFun (Int32 -> FSVec D32 Bit)
to32BitFun = $(newProcFun
 [d| to32Bit :: Int32 -> FSVec D32 Bit
     to32Bit i = toBitVector32 i |])

to32BitProc :: Signal Int32 -> Signal (FSVec D32 Bit)
to32BitProc = mapSY "to32Proc" to32BitFun 

to32BitSysDef :: SysDef (Signal Int32 -> Signal (FSVec D32 Bit))
to32BitSysDef = newSysDef to32BitProc "to32BitSys" ["in1"] ["out1"]
