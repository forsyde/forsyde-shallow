{-# LANGUAGE TemplateHaskell #-}
module Counter where

import ForSyDe
import Data.Int (Int32)
import Plus1 (addOnef)

{- First version, without using sourceSY
counterProc :: Signal Int32
counterProc = out'
  where out  = mapSY "addOneProc" addOnef out'
        out' = delaySY "delayOne" 1 out
-}

counterProc :: Signal Int32
counterProc = sourceSY "counterProc" addOnef 1




counterSysDef :: SysDef (Signal Int32)
counterSysDef = newSysDef counterProc "counter" [] ["count"]