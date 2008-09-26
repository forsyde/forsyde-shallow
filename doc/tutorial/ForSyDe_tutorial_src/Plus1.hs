{-# LANGUAGE TemplateHaskell #-}
module Plus1 where

import ForSyDe
import Data.Int (Int32)

-- A process function which adds one to its input
addOnef :: ProcFun (Int32 -> Int32)
addOnef = $(newProcFun [d|addOnef :: Int32 -> Int32 
                          addOnef n = n + 1     |])


-- System function (simply a process in this case) which uses addOnef
plus1Proc :: Signal Int32 -> Signal Int32
plus1Proc = mapSY "plus1Proc" addOnef


-- System definition associated to the system function
plus1SysDef :: SysDef (Signal Int32 -> Signal Int32)
plus1SysDef = newSysDef plus1Proc "plus1" ["inSignal"] ["outSignal"]

combLoopSysDef :: SysDef (Signal Int32)
combLoopSysDef = newSysDef s "combLoop" [] ["out"] 
 where s  = mapSY "addOne1" addOnef s'
       s' = mapSY "addOne2" addOnef s
