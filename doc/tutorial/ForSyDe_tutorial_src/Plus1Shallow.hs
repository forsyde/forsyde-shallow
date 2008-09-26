module Plus1Shallow where

import ForSyDe.Shallow

import Data.Int(Int32)

plus1 :: Signal Int32 -> Signal Int32
plus1 = mapSY (+1)