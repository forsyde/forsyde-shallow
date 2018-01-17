-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.Adaptivity
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Adaptivity Library, yet to be completed.
-- 
-----------------------------------------------------------------------------
module ForSyDe.Shallow.MoC.Adaptivity (
  applyfSY, applyf2SY, applyf3SY, 
    applyfU
  ) where

import ForSyDe.Shallow.Core.Signal
import ForSyDe.Shallow.MoC.Synchronous.Lib
import ForSyDe.Shallow.MoC.Untimed

applyfSY :: Signal (a -> b) -> Signal a -> Signal b
applyfSY = zipWithSY ($)

applyf2SY :: Signal (a -> c -> d) 
      -> Signal a -> Signal c -> Signal d
applyf2SY = zipWith3SY ($)

applyf3SY :: Signal (a -> c -> d -> e) 
      -> Signal a -> Signal c -> Signal d -> Signal e
applyf3SY = zipWith4SY ($)

applyfU :: Int -> Signal ([a] -> [b]) -> Signal a -> Signal b
applyfU tokenNum = comb2UC tokenNum apply
  where apply f = f
