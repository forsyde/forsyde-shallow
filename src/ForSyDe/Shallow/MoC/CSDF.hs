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
  mapCSDF, zipWithCSDF, zipWith3CSDF, zipWith4CSDF,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delayCSDF, delaynCSDF,
  -- * Processes
  -- | Processes to unzip a signal of tupels into a tuple of signals
  --unzipCSDF, unzip3CSDF, unzip4CSDF,
  -- * Actors
  -- | Based on the process constructors in the CSDF-MoC, the
  -- CSDF-library provides CSDF-actors with single or multiple inputs
  actor11CSDF, --actor12CSDF, actor13CSDF, actor14CSDF,
  actor21CSDF, --actor22CSDF, actor23CSDF, actor24CSDF,
  actor31CSDF, --actor32CSDF, actor33CSDF, actor34CSDF,
  actor41CSDF--, actor42CSDF, actor43CSDF, actor44CSDF
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
mapCSDF (c:cs) (p:ps) (f:fs) xs
  | length cs /= length ps = error "mapCSDF: List of input and output tokens must have the same length"
  | length cs /= length fs = error "mapCSDF: List of tokens and functions must have the same length"
  | c < 0 = error "mapCSDF: Number of consumed tokens must be a non-negative integer"
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
zipWithCSDF (c:cs) (p:ps) (f:fs) as bs
  | length cs /= length ps = error "zipWithCSDF: List of input and output tokens must have the same length"
  | length cs /= length fs = error "zipWithCSDF: List of tokens and functions must have the same length"
  | c1 < 0 || c2 < 0  = error "zipWithCSDF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as) || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithCSDF (cs++[c]) (ps++[p]) (fs++[f]) (dropS c1 as) (dropS c2 bs)
                else
                  error "zipWithCSDF: Function does not produce correct number of tokens"
  where (c1, c2) = c
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
zipWith3CSDF (c:css) (p:ps) (f:fs) as bs cs
  | length css /= length ps = error "zipWith3CSDF: List of input and output tokens must have the same length"
  | length css /= length fs = error "zipWith3CSDF: List of tokens and functions must have the same length"
  | c1 < 0 || c2 < 0 || c3 < 0
  = error "zipWith3CSDF: Number of consumed tokens must be a non-negative integer"
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


-- | The process constructor 'zipWith4CSDF' takes a tuple @(c1, c2, c3,c4)@
-- denoting the number of consumed tokens and an integer @p@
-- denoting the number of produced tokens and a function @f@ that
-- operates on three lists, and results in an CSDF-process that takes
-- three input signals and results in an output signal
zipWith4CSDF :: [(Int, Int, Int, Int)] -> [Int]
            -> [[a] -> [b] -> [c] -> [d] -> [e]]
            -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4CSDF [] _ _ _ _ _ _ = error "zipWith4CSDF: List of input tokens must not be empty"
zipWith4CSDF _ [] _ _ _ _ _ = error "zipWith4CSDF: List of output tokens must not be empty"
zipWith4CSDF _ _ [] _ _ _ _ = error "zipWith4CSDF: List of functions must not be empty"
zipWith4CSDF (c:css) (p:ps) (f:fs) as bs cs ds
  | length css /= length ps = error "zipWith4CSDF: List of input and output tokens must have the same length"
  | length css /= length fs = error "zipWith4CSDF: List of tokens and functions must have the same length"
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0
  = error "zipWith4CSDF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds)
  = NullS
  | otherwise
  = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith4CSDF (css++[c]) (ps++[p]) (fs++[f])
             (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
    else
      error "zipWith4CSDF: Function does not produce correct number of tokens"
  where (c1, c2, c3, c4) = c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        produced_tokens = f consumed_tokens_as consumed_tokens_bs
                            consumed_tokens_cs consumed_tokens_ds


-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delayCSDF' delays the signal one event
--   cycle by introducing an initial value at the beginning of the
--   output signal.  Note, that this implies that there is one event
--   (the first) at the output signal that has no corresponding event at
--   the input signal.  One could argue that input and output signals
--   are not fully synchronized, even though all input events are
--   synchronous with a corresponding output event. However, this is
--   necessary to initialize feed-back loops.
delayCSDF :: a -> Signal a -> Signal a
delayCSDF x xs = x :- xs


-- | The process constructor 'delaynCSDF' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delaynCSDF :: [a] -> Signal a -> Signal a
delaynCSDF initial_tokens xs = signal initial_tokens +-+ xs


------------------------------------------------------------------------
--
-- CSDF ACTORS
--
------------------------------------------------------------------------

-- > Actors with one output

-- | The process constructor 'actor11CSDF' constructs an CSDF actor with
-- one input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor11CSDF :: [Int] -> [Int] -> [[a] -> [b]] -> Signal a -> Signal b
actor11CSDF = mapCSDF

-- | The process constructor 'actor21CSDF' constructs an CSDF actor with
-- two input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor21CSDF :: [(Int, Int)] -> [Int] -> [[a] -> [b] -> [c]] -> Signal a -> Signal b -> Signal c
actor21CSDF = zipWithCSDF

-- | The process constructor 'actor31CSDF' constructs an CSDF actor with
-- three input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor31CSDF :: [(Int, Int, Int)] -> [Int] -> [[a] -> [b] -> [c] -> [d]]
       -> Signal a -> Signal b -> Signal c -> Signal d
actor31CSDF = zipWith3CSDF

-- | The process constructor 'actor41CSDF' constructs an CSDF actor with
-- four input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor41CSDF :: [(Int, Int, Int, Int)] -> [Int]
    -> [[a] -> [b] -> [c] -> [d] -> [e]]
    -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
actor41CSDF = zipWith4CSDF

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


------------------------------------------------------------------------
--
-- Test of Library (not exported)
--
------------------------------------------------------------------------

{-
test :: Num a => Signal a
test = s3
  where s3 = delaynCSDF [1,1] s2
        s2 = v2 s1
        s1 = v1 s4
        s4 = v3 s3

v1 :: Num a => Signal a -> Signal a
v1 = actor11CSDF [1,1,1] [1,0,0] [\[a] -> [a], \[a] -> [], \[a] -> []]

v2 :: Num a => Signal a -> Signal a
v2 = actor11CSDF [1,1] [0,2] [\[a] -> [], \[a] -> [a, 2*a]]

v3 :: Num a => Signal a -> Signal a
v3 = actor11CSDF [1] [3] [\[a] -> [a, 2*a, 3*a]]

output = takeS 10 test

-- Expected answer: {1,1,1,2,2,4,4,8,8,16}
-}
