{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

module ForSyDe.Shallow.AdaptivityLib (applyfSY, applyfU) where

import ForSyDe.Shallow.Signal
import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.UntimedLib

applyfSY :: Signal (a -> b) -> Signal a -> Signal b
applyfSY = zipWithSY apply
           where apply f x = f x

applyfU :: Int -> Signal ([a] -> [b]) -> Signal a -> Signal b
applyfU tokenNum = comb2UC tokenNum apply
      where apply f x = f x
