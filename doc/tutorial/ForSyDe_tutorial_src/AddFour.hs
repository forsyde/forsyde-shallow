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

compileQuartus :: IO ()
compileQuartus = writeVHDLOps vhdlOps addFourSysDef
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Three sample pin assignments
                               pinAssigs=[("in1[0]", "PIN_W1"),
                                          ("in1[1]", "PIN_W2"),
                                          ("in1[2]", "PIN_W3")]}

compileModelSim :: IO ()
compileModelSim = writeVHDLOps vhdlOps addFourSysDef
 where vhdlOps = defaultVHDLOps{compileModelsim=True}