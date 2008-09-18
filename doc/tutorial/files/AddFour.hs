module AddFour where

import Plus1 (plus1SysDef)

import ForSyDe
import Data.Int (Int32)

addFourProc :: Signal Int32 -> Signal Int32
addFourProc = plus1Comp "plus1_1" . 
              plus1Comp "plus1_2" .
              plus1Comp "plus1_3" .
              plus1Comp "plus1_4" 
 where plus1Comp id = instantiate id plus1SysDef

addFourSysDef :: SysDef (Signal Int32 -> Signal Int32)
addFourSysDef = newSysDef addFourProc "addFour" ["in1"] ["out1"]