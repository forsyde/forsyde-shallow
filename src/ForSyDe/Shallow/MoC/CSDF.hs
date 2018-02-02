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
  mapCSDF, zipWithCSDF, zipWith3CSDF
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
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ mapCSDF (cs++[c]) (ps++[p]) (fs++[f]) (dropS c xs)
                else
                  error "mapCSDF: Function does not produce correct number of tokens"
  where consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithCSDF' takes a tuple @(c1, c2)@
-- denoting the number of consumed tokens and an integer @p@ denoting
-- the number of produced tokens and a function @f@
-- that operates on two lists, and results in an CSDF-process that takes two
-- input signals and results in an output signal
zipWithCSDF :: [(Int, Int)] -> [Int] -> [[a] -> [b] -> [c]]
            -> Signal a -> Signal b -> Signal c
zipWithCSDF [] _ _ _ _ = error "zipWithCSDF: List of input tokens must not be empty"
zipWithCSDF _ [] _ _ _ = error "zipWithCSDF: List of output tokens must not be empty"
zipWithCSDF _ _ [] _ _ = error "zipWithCSDF: List of functions must not be empty"
zipWithCSDF _ _ _ NullS _ = NullS
zipWithCSDF _ _ _ _ NullS = NullS
zipWithCSDF (c:cs) (p:ps) (f:fs) as bs
  | length cs /= length ps = error "zipWithCSDF: List of input and output tokens must have the same length"
  | length cs /= length fs = error "zipWithCSDF: List of tokens and functions must have the same length"
  | c1 <= 0 || c2 <= 0  = error "zipWithCSDF: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as) || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithCSDF (cs++[c]) (ps++[p]) (fs++[f]) (dropS c1 as) (dropS c2 bs)
                else
                  error "zipWithCSDF: Function does not produce correct number of tokens"
  where c1 = fst c
        c2 = snd c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs


-- | The process constructor 'zipWith3CSDF' takes a tuple @(c1, c2, c3)@
-- denoting the number of consumed tokens and an integer @p@ denoting
-- the number of produced tokens and a function @f@
-- that operates on three lists, and results in an SDF-process that takes three
-- input signals and results in an output signal
zipWith3CSDF :: [(Int, Int, Int)] -> [Int] -> [[a] -> [b] -> [c] -> [d]]
             -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3CSDF [] _ _ _ _ _ = error "zipWith3CSDF: List of input tokens must not be empty"
zipWith3CSDF _ [] _ _ _ _ = error "zipWith3CSDF: List of output tokens must not be empty"
zipWith3CSDF _ _ [] _ _ _ = error "zipWith3CSDF: List of functions must not be empty"
zipWith3CSDF _ _ _ NullS _ _ = NullS
zipWith3CSDF _ _ _ _ NullS _ = NullS
zipWith3CSDF _ _ _ _ _ NullS = NullS
zipWith3CSDF (c:css) (p:ps) (f:fs) as bs cs
  | length css /= length ps = error "zipWith3CSDF: List of input and output tokens must have the same length"
  | length css /= length fs = error "zipWith3CSDF: List of tokens and functions must have the same length"
  | c1 <= 0 || c2 <= 0 || c3 <= 0
  = error "zipWith3CSDF: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
  = NullS
  | otherwise
  = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith3CSDF (css++[c]) (ps++[p]) (fs++[f])
                                 (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
    else
      error "zipWith3CSDF: Function does not produce correct number of tokens"
  where (c1, c2, c3) = c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs



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
