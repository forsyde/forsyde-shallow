-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.AdaptivityLib
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Adaptivity Library, yet to be completed.
-- 
-----------------------------------------------------------------------------
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
