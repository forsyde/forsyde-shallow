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
  sy2ct, ct2sy, DACMode(..)) where

import ForSyDe.Shallow.MoC.CT
import ForSyDe.Shallow.Core.Signal

-- |For the digital-analog conversion we have two different possibilities
-- which is determined by this data type 'DACMode'.
data DACMode = DAlinear -- ^linear interpolation
             | DAhold   -- ^the last digital value is frozen
             deriving (Show, Eq)

-- | The MoC interface 'sy2ct' converts a synchronous signal into a
-- continuous time signal.
sy2ct :: (Fractional a, Show a) =>
         DACMode             -- ^Mode of conversion
      -> Rational            -- ^Duration of input signal
      -> Signal a            -- ^Input signal (untimed MoC)
      -> Signal (SubsigCT a) -- ^Output signal (continuous time MoC)
sy2ct mode c xs
    | mode == DAlinear = d2aLinear c 0.0 xs
    | otherwise = d2aHolder c 0.0 xs
  where
    d2aHolder :: (Num a, Show a) =>
                 Rational -> Rational -> Signal a -> Signal (SubsigCT a)
    d2aHolder _ _ NullS = NullS
    d2aHolder c holdT (x:-xs) = (SubsigCT (constRationalF x,(holdT,holdT+c)) )
                                :- d2aHolder c (holdT+c) xs

    d2aLinear :: (Fractional a, Show a) =>
                 Rational -> Rational -> Signal a -> Signal (SubsigCT a)
    d2aLinear _ _ NullS = NullS
    d2aLinear _ _ (_:-NullS) = NullS
    d2aLinear c holdT (x:-y:-xs)
      = (SubsigCT (linearRationalF c holdT x y,(holdT,holdT+c)) )
        :- d2aLinear c (holdT+c) (y:-xs)

    constRationalF :: (Num a) => a -> Rational -> a
    constRationalF = (\x _->x)

    linearRationalF :: (Fractional a) =>
                       Rational -> Rational -> a -> a -> Rational -> a
    linearRationalF c holdT m n x = (1-alpha)*m + alpha*n
      where alpha :: (Fractional a) => a
            alpha = fromRational ((x-holdT)/c)


-- | The MoC interface 'ct2sy' converts a synchronous signal into a
-- continuous time signal. .
ct2sy :: (Num a, Show a) =>
         Rational            -- ^Sampling Period
      -> Signal (SubsigCT a) -- ^Input signal (continuous time)
      -> Signal a            -- ^Output signal (untimed) = d2aConverter
ct2sy _ NullS = NullS
ct2sy c s | (duration (takeCT c s)) < c = NullS
                 | otherwise = f (takeCT c s)
                   +-+ ct2sy c (dropCT c s)
  where f :: (Num a, Show a) => Signal (SubsigCT a) -> Signal a
        f NullS = NullS
        f (SubsigCT (g,(a,_)) :- _) = signal [g a]
