{-# LANGUAGE TemplateHaskell #-}

-- A counter, the simplest system with which to test netlist loops

module Counter where

import ForSyDe

counter :: Signal Int -> Signal Int
counter _ = sourceSY "counterSource" add1 (0 :: Int)
 where add1 = $(newProcFun [d| add1 :: Int -> Int 
                               add1 a = a + 1   |]) 


counterSys :: SysDef (Signal Int -> Signal Int)
counterSys = $(newSysDef 'counter ["undefined"] ["countVal"])

simCounter :: [Int] -> [Int]
simCounter = $(simulate 'counterSys)
