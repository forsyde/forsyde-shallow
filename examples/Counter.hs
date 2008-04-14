{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module Counter where

import ForSyDe
import Data.Int

counter :: Signal Int32
counter  = sourceSY "counterSource" add1 (0 :: Int32)
 where add1 = $(newProcFun [d| add1 :: Int32 -> Int32 
                               add1 a = a + 1   |]) 


counterSys :: SysDef (Signal Int32)
counterSys = $(newSysDef 'counter [] ["countVal"])

simCounter :: [Int32]
simCounter = $(simulate 'counterSys)
