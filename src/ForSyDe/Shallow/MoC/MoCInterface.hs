-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.MoCInterface
-- Copyright   :  KTH/ICT/ELE/ESY, 2017
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ingo@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines models of computation interfaces between the
-- different MOCs.
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.MoCInterface(
  -- * Interfaces between Synchronous MoC and Continuous Time MoC
  sy2ct, ct2sy) where

import ForSyDe.Shallow.MoC.CT
import ForSyDe.Shallow.Core.Signal
-- | The MoC interface 'sy2ct' converts a synchronous signal into a
-- continuous time signal. It uses the 'd2aConverter' function, which
-- currently is defined in the CT library.
sy2ct :: (Fractional a, Show a) =>
         DACMode             -- ^Mode of conversion
      -> Rational            -- ^Duration of input signal
      -> Signal a            -- ^Input signal (untimed MoC)
      -> Signal (SubsigCT a) -- ^Output signal (continuous time MoC)
sy2ct = d2aConverter     

-- | The MoC interface 'ct2sy' converts a synchronous signal into a
-- continuous time signal. It uses the 'a2dConverter' function, which
-- currently is defined in the CT library.
ct2sy :: (Num a, Show a) =>
         Rational            -- ^Sampling Period
      -> Signal (SubsigCT a) -- ^Input signal (continuous time)
      -> Signal a            -- ^Output signal (untimed) = d2aConverter
ct2sy = a2dConverter
