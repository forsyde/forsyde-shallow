{-# LANGUAGE TemplateHaskell #-}
-- Naive example to test instances working in paralell
-- Adds a signal to four signals
module ParAddFour where

import ForSyDe
import Data.Int


add :: ProcFun (Int32 -> Int32 -> Int32)
add = $(newProcFun [d| add :: Int32 -> Int32 -> Int32
                       add a b = a + b |])

addProc :: Signal Int32 -> Signal Int32 -> Signal Int32
addProc s1 s2 = zipWithSY "zip1" add s1 s2


addSys :: SysDef (Signal Int32 -> Signal Int32 -> Signal Int32)
addSys = newSysDef addProc "add" ["in1","in2"] ["sum"]

simAdd :: [Int32] -> [Int32] -> [Int32]
simAdd = simulate addSys

parAddFour :: Signal Int32 
           -> Signal Int32 -> Signal Int32 -> Signal Int32 -> Signal Int32 
           -> (Signal Int32, Signal Int32, Signal Int32, Signal Int32)
parAddFour toAdd s1 s2 s3 s4 = (sum1, sum2, sum3, sum4)
  where sum1 = (instantiate "adder1" addSys) toAdd s1
        sum2 = (instantiate "adder2" addSys) toAdd s2
        sum3 = (instantiate "adder3" addSys) toAdd s3
        sum4 = (instantiate "adder4" addSys) toAdd s4

parAddFourSys :: SysDef (Signal Int32 
           -> Signal Int32 -> Signal Int32 -> Signal Int32 -> Signal Int32 
           -> (Signal Int32, Signal Int32, Signal Int32, Signal Int32))
parAddFourSys = newSysDef parAddFour "parAddFour" ["toAdd","s1","s2","s3","s4"]
                                        ["sum1","sum2","sum3","sum4"]


simParAddFour :: [Int32]
              -> [Int32] -> [Int32] -> [Int32] -> [Int32]
              -> ([Int32], [Int32], [Int32], [Int32]) 
simParAddFour = simulate parAddFourSys
