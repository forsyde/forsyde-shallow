{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module Counter where

import ForSyDe

counter :: Signal Int
counter  = sourceSY "counterSource" add1 (0 :: Int)
 where add1 = $(newProcFun [d| add1 :: Int -> Int 
                               add1 a = a + 1   |]) 


counterSys :: SysDef (Signal Int)
counterSys = $(newSysDef 'counter [] ["countVal"])

simCounter :: [Int]
simCounter = $(simulate 'counterSys)
