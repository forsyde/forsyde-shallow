{-# LANGUAGE TemplateHaskell #-}

-- This module is aimed at testing a naive use-case of 
-- the instantiation mechanism

-- We'll build a system which uses four single-adder components connected
-- sequentially to add four units to the values of the input signal

module SeqAddFour where

import ForSyDe

-- A process function which adds one to its input
addOnef :: ProcFun (Int -> Int)
addOnef = $(newProcFun [d|addOnef :: Int -> Int 
                          addOnef n = n + 1     |])

-- System function (or process) which uses addOnef
addOneProc :: Signal Int -> Signal Int
addOneProc = mapSY "addOne" addOnef


-- System definition associated to the system function
addOneSysDef :: SysDef (Signal Int -> Signal Int)
addOneSysDef = $(newSysDef 'addOneProc ["in1"] ["out1"])


-- Finally, we create the sequential add four system function
addFour :: Signal Int -> Signal Int
addFour = $(instantiate "addOne3" 'addOneSysDef) .
          $(instantiate "addOne2" 'addOneSysDef) .
          $(instantiate "addOne1" 'addOneSysDef) .
          $(instantiate "addOne0" 'addOneSysDef)

-- We build the system
addFourSys :: SysDef (Signal Int -> Signal Int)
addFourSys = $(newSysDef 'addFour ["in1"] ["out1"])

-- we simulate the system
simAddFour :: [Int] -> [Int]
simAddFour = $(simulate 'addFourSys)
