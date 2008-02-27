{-# LANGUAGE TemplateHaskell #-}
-- Naive example to test instances working in paralell
-- Adds a signal to four signals
module ParAddFour where

import ForSyDe


add :: ProcFun (Int -> Int -> Int)
add = $(newProcFun [d| add :: Int -> Int -> Int
                       add a b = a + b |])

addProc :: Signal Int -> Signal Int -> Signal Int
addProc s1 s2 = zipWithSY "zip1" add s1 s2


addSys :: SysDef (Signal Int -> Signal Int -> Signal Int)
addSys = $(newSysDef 'addProc ["in1","in2"] ["sum"])

simAdd :: [Int] -> [Int] -> [Int]
simAdd = $(simulate 'addSys)

parAddFour :: Signal Int 
           -> Signal Int -> Signal Int -> Signal Int -> Signal Int 
           -> (Signal Int, Signal Int, Signal Int, Signal Int)
parAddFour toAdd s1 s2 s3 s4 = (sum1, sum2, sum3, sum4)
  where sum1 = $(instantiate "adder1" 'addSys) toAdd s1
        sum2 = $(instantiate "adder2" 'addSys) toAdd s2
        sum3 = $(instantiate "adder3" 'addSys) toAdd s3
        sum4 = $(instantiate "adder4" 'addSys) toAdd s4

parAddFourSys :: SysDef (Signal Int 
           -> Signal Int -> Signal Int -> Signal Int -> Signal Int 
           -> (Signal Int, Signal Int, Signal Int, Signal Int))
parAddFourSys = $(newSysDef 'parAddFour ["toAdd","s1","s2","s3","s4"]
                                        ["sum1","sum2","sum3","sum4"])


simParAddFour :: [Int]
              -> [Int] -> [Int] -> [Int] -> [Int]
              -> ([Int], [Int], [Int], [Int]) 
simParAddFour = $(simulate 'parAddFourSys)