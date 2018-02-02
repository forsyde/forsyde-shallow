-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.CSDF
-- Copyright   :  some copyright
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Experimental lib. Further test needed
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.CSDF (
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  -- do not have a state.
  mapCSDF
  ) where

import ForSyDe.Shallow.Core

------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS
------------------------------------------------------------------------

-- | The process constructor 'mapCSDF' takes the number of consumed
-- (@c@) and produced (@p@) tokens and a function @f@ that operates on
-- a list, and results in an CSDF-process that takes an input signal
-- and results in an output signal
mapCSDF :: [Int] -> [Int] -> [[a] -> [b]] -> Signal a -> Signal b
mapCSDF [] _ _ _ = error "mapCSDF: List of input tokens must not be empty"
mapCSDF _ [] _ _ = error "mapCSDF: List of output tokens must not be empty"
mapCSDF _ _ [] _ = error "mapCSDF: List of functions must not be empty"
mapCSDF _ _ _ NullS   = NullS
mapCSDF (c:cs) (p:ps) (f:fs) xs
  | length cs /= length ps = error "mapCSDF: List of input and output tokens must have the same length"
  | length cs /= length fs = error "mapCSDF: List of tokens and functions must have the same length"
  | c <= 0 = error "mapCSDF: Number of consumed tokens must be positive integer"
  | not $ sufficient_tokens c xs  = NullS
  | otherwise  = if length produced_tokens == p then
                   signal produced_tokens +-+ mapCSDF (cs++[c]) (ps++[p]) (fs++[f]) (dropS c xs)
                 else
                   error "mapCSDF: Function does not produce correct number of tokens"
  where consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens



------------------------------------------------------------------------
--
-- Helper functions (not exported!)
--
------------------------------------------------------------------------

sufficient_tokens :: (Num a, Eq a, Ord a) => a -> Signal t -> Bool
sufficient_tokens 0 _     = True
sufficient_tokens _ NullS = False
sufficient_tokens n (_:-xs)
 = if n < 0 then
     error "sufficient_tokens: n must not be negative"
   else
     sufficient_tokens (n-1) xs
