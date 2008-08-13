-- This example shows how to mix shallow-embedded and
-- deep-embedded signals by building an heterogeneous
-- system which adds five to its input
module DeepShallow where

import SeqAddFour (addFourSys)
import ForSyDe(simulate)
import ForSyDe.Shallow
import Data.Int

-- addOne using shallow-embedded signals
addOne :: Signal Int32 -> Signal Int32
addOne = mapSY (+1)

-- addFourSys uses deep-embedded signals, but we can transform them to lists
-- using simulate
addFourLists :: [Int32] -> [Int32]
addFourLists = simulate addFourSys

-- addFive uses addOne and addFourLists
addFive :: Signal Int32 -> Signal Int32
addFive = signal.addFourLists.fromSignal.addOne