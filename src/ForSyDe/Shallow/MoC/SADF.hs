-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.SADF
-- Copyright   :  (c) Ricardo Bonna, KTH/ICT/ES, ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Experimental lib. Further test needed
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.SADF (
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  -- do not have a state.
  mapSADF, zipWithSADF, zipWith3SADF, zipWith4SADF,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delaySADF, delaynSADF,
  -- * Processes
  -- | Processes to unzip a signal of tupels into a tuple of signals
  unzipSADF, unzip3SADF, unzip4SADF,
  -- * Actors
  -- | Based on the process constructors in the SADF-MoC, the
  -- SADF-library provides SADF-kernels with single or multiple inputs
  kernel11SADF, kernel12SADF, kernel13SADF, kernel14SADF,
  kernel21SADF, kernel22SADF, kernel23SADF, kernel24SADF,
  kernel31SADF, kernel32SADF, kernel33SADF, kernel34SADF,
  kernel41SADF, kernel42SADF, kernel43SADF, kernel44SADF
  ) where

import ForSyDe.Shallow.Core
import Data.List(unzip4)

------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS
------------------------------------------------------------------------

-- | The process constructor 'mapSADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes an input signal and results
-- in an output signal
mapSADF :: Signal (Int, Int, [a] -> [b]) -> Signal a -> Signal b
mapSADF NullS _ = NullS
mapSADF ct xs
  | c < 0 = error "mapSADF: Number of consumed tokens must be a non-negative integer"
  | not $ sufficient_tokens c xs  = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ mapSADF (tailS ct) (dropS c xs)
                else
                  error "mapSADF: Function does not produce correct number of tokens"
  where (c, p, f) = headS ct
        consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithSADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes two input signals and
-- results in an output signal
zipWithSADF :: Signal ((Int, Int), Int, [a] -> [b] -> [c])
            -> Signal a -> Signal b -> Signal c
zipWithSADF NullS _ _ = NullS
zipWithSADF ct as bs
  | c1 < 0 || c2 < 0  = error "zipWithSADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithSADF (tailS ct) (dropS c1 as) (dropS c2 bs)
                else
                  error "zipWithSADF: Function does not produce correct number of tokens"
  where ((c1,c2), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs


-- | The process constructor 'zipWith3SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes three input signals and
-- results in an output signal
zipWith3SADF :: Signal ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])
             -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3SADF NullS _ _ _ = NullS
zipWith3SADF ct as bs cs
  | c1 < 0 || c2 < 0 || c3 < 0
    = error "zipWith3SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith3SADF (tailS ct) (dropS c1 as)
                                                        (dropS c2 bs) (dropS c3 cs)
                else
                  error "zipWith3SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs


-- | The process constructor 'zipWith4SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes four input signals and
-- results in an output signal
zipWith4SADF :: Signal ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4SADF NullS _ _ _ _ = NullS
zipWith4SADF ct as bs cs ds
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0
    = error "zipWith4SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith4SADF (tailS ct) (dropS c1 as)
                                              (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
                else
                  error "zipWith4SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3, c4), p, f) = headS ct
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

-- | The process constructor 'delaySADF' delays the signal one event
--   cycle by introducing an initial value at the beginning of the
--   output signal.  Note, that this implies that there is one event
--   (the first) at the output signal that has no corresponding event at
--   the input signal.  One could argue that input and output signals
--   are not fully synchronized, even though all input events are
--   synchronous with a corresponding output event. However, this is
--   necessary to initialize feed-back loops.
delaySADF :: a -> Signal a -> Signal a
delaySADF x xs = x :- xs


-- | The process constructor 'delaynSADF' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delaynSADF :: [a] -> Signal a -> Signal a
delaynSADF initial_tokens xs = signal initial_tokens +-+ xs


------------------------------------------------------------------------
--
-- SADF ACTORS
--
------------------------------------------------------------------------

-- > Kernals with one output

-- | The process constructor 'kernel11SADF' constructs an SADF kernel with
-- one data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel11SADF :: Signal (Int, Int, [a] -> [b]) -> Signal a -> Signal b
kernel11SADF = mapSADF

-- | The process constructor 'kernel21SADF' constructs an SADF kernel with
-- two data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel21SADF :: Signal ((Int, Int), Int, [a] -> [b] -> [c])
             -> Signal a -> Signal b -> Signal c
kernel21SADF = zipWithSADF

-- | The process constructor 'kernel31SADF' constructs an SADF kernel with
-- three data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel31SADF :: Signal ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])
             -> Signal a -> Signal b -> Signal c -> Signal d
kernel31SADF = zipWith3SADF

-- | The process constructor 'kernel41SADF' constructs an SADF kernel with
-- four data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel41SADF :: Signal ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
kernel41SADF = zipWith4SADF


-- > Kernals with two outputs

-- | The process constructor 'kernel12SADF' constructs an SADF kernel with
-- one data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel12SADF :: Signal (Int, (Int, Int), [a] -> [([b], [c])])
             -> Signal a -> (Signal b, Signal c)
kernel12SADF ct xs = unzipSADF (get_prodToken ct) $ mapSADF (switch_prodToken ct) xs

-- | The process constructor 'kernel22SADF' constructs an SADF kernel with
-- two data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel22SADF :: Signal ((Int, Int), (Int, Int), [a] -> [b] -> [([c], [d])])
             -> Signal a -> Signal b -> (Signal c, Signal d)
kernel22SADF ct xs ys = unzipSADF (get_prodToken ct) $ zipWithSADF (switch_prodToken ct) xs ys

-- | The process constructor 'kernel32SADF' constructs an SADF kernel with
-- three data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel32SADF :: Signal ((Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [([d], [e])])
             -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e)
kernel32SADF ct as bs cs
  = unzipSADF (get_prodToken ct) $ zipWith3SADF (switch_prodToken ct) as bs cs

-- | The process constructor 'kernel42SADF' constructs an SADF kernel with
-- four data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel42SADF :: Signal ((Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> [([e], [f])])
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f)
kernel42SADF ct as bs cs ds
  = unzipSADF (get_prodToken ct) $ zipWith4SADF (switch_prodToken ct) as bs cs ds


-- > Kernals with three outputs

-- | The process constructor 'kernel13SADF' constructs an SADF kernel with
-- one data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel13SADF :: Signal (Int, (Int, Int, Int), [a] -> [([b], [c], [d])])
             -> Signal a -> (Signal b, Signal c, Signal d)
kernel13SADF ct xs = unzip3SADF (get_prodToken ct) $ mapSADF (switch_prodToken ct) xs

-- | The process constructor 'kernel23SADF' constructs an SADF kernel with
-- two data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel23SADF :: Signal ((Int, Int), (Int, Int, Int), [a] -> [b] -> [([c], [d], [e])])
             -> Signal a -> Signal b -> (Signal c, Signal d, Signal e)
kernel23SADF ct xs ys = unzip3SADF (get_prodToken ct) $ zipWithSADF (switch_prodToken ct) xs ys

-- | The process constructor 'kernel33SADF' constructs an SADF kernel with
-- three data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel33SADF :: Signal ((Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> [([d], [e], [f])])
             -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e, Signal f)
kernel33SADF ct as bs cs
  = unzip3SADF (get_prodToken ct) $ zipWith3SADF (switch_prodToken ct) as bs cs

-- | The process constructor 'kernel43SADF' constructs an SADF kernel with
-- four data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel43SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [([e], [f], [g])])
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g)
kernel43SADF ct as bs cs ds
  = unzip3SADF (get_prodToken ct) $ zipWith4SADF (switch_prodToken ct) as bs cs ds


-- > Kernals with four outputs

-- | The process constructor 'kernel14SADF' constructs an SADF kernel with
-- one data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel14SADF :: Signal (Int, (Int, Int, Int, Int), [a] -> [([b], [c], [d], [e])])
             -> Signal a -> (Signal b, Signal c, Signal d, Signal e)
kernel14SADF ct xs = unzip4SADF (get_prodToken ct) $ mapSADF (switch_prodToken ct) xs

-- | The process constructor 'kernel24SADF' constructs an SADF kernel with
-- two data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel24SADF :: Signal ((Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [([c], [d], [e], [f])])
             -> Signal a -> Signal b -> (Signal c, Signal d, Signal e, Signal f)
kernel24SADF ct xs ys = unzip4SADF (get_prodToken ct) $ zipWithSADF (switch_prodToken ct) xs ys

-- | The process constructor 'kernel34SADF' constructs an SADF kernel with
-- three data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel34SADF :: Signal ((Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [([d], [e], [f], [g])])
             -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e, Signal f, Signal g)
kernel34SADF ct as bs cs
  = unzip4SADF (get_prodToken ct) $ zipWith3SADF (switch_prodToken ct) as bs cs

-- | The process constructor 'kernel44SADF' constructs an SADF kernel with
-- four data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel44SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [([e], [f], [g], [h])])
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g, Signal h)
kernel44SADF ct as bs cs ds
  = unzip4SADF (get_prodToken ct) $ zipWith4SADF (switch_prodToken ct) as bs cs ds

------------------------------------------------------------------------
-- unzipSADF Processes
------------------------------------------------------------------------

unzipSADF :: [(Int, Int)] -> Signal ([a], [b])
          -> (Signal a, Signal b)
unzipSADF p xs = (s1, s2)
  where
    (p1, p2) = unzip p
    s1 = signal $ f1 p1 xs
    s2 = signal $ f2 p2 xs
    f1 [] _ = []
    f1 _ NullS     = []
    f1 (t:ts) ((as, _):-xs)
      = if length as == t then
          as ++ f1 ts xs
        else
          error "unzipSADF: Process does not produce correct number of tokens"
    f2 [] _ = []
    f2 _ NullS     = []
    f2 (t:ts) ((_, bs):-xs)
      = if length bs == t then
          bs ++ f2 ts xs
        else
          error "unzipSADF: Process does not produce correct number of tokens"


unzip3SADF :: [(Int, Int, Int)] -> Signal ([a], [b], [c])
           -> (Signal a, Signal b, Signal c)
unzip3SADF p xs = (s1, s2, s3)
  where
    (p1, p2, p3) = unzip3 p
    s1 = signal $ f1 p1 xs
    s2 = signal $ f2 p2 xs
    s3 = signal $ f3 p3 xs
    f1 [] _ = []
    f1 _ NullS      = []
    f1 (t:ts) ((as, _, _):-xs)
      = if length as == t then
          as ++ f1 ts xs
        else
          error "unzip3SADF: Process does not produce correct number of tokens"
    f2 [] _ = []
    f2 _ NullS      = []
    f2 (t:ts) ((_, bs, _):-xs)
      = if length bs == t then
          bs ++ f2 ts xs
        else
          error "unzip3SADF: Process does not produce correct number of tokens"
    f3 [] _ = []
    f3 _ NullS      = []
    f3 (t:ts) ((_, _, cs):-xs)
      = if length cs == t then
          cs ++ f3 ts xs
        else
          error "unzip3SADF: Process does not produce correct number of tokens"


unzip4SADF :: [(Int, Int, Int, Int)] -> Signal ([a], [b], [c], [d])
           -> (Signal a, Signal b, Signal c, Signal d)
unzip4SADF p xs = (s1, s2, s3, s4)
  where
    (p1, p2, p3, p4) = unzip4 p
    s1 = signal $ f1 p1 xs
    s2 = signal $ f2 p2 xs
    s3 = signal $ f3 p3 xs
    s4 = signal $ f4 p4 xs
    f1 [] _ = []
    f1 _ NullS      = []
    f1 (t:ts) ((as, _, _, _):-xs)
      = if length as == t then
          as ++ f1 (ts++[t]) xs
        else
          error "unzip4SADF: Process does not produce correct number of tokens"
    f2 [] _ = []
    f2 _ NullS      = []
    f2 (t:ts) ((_, bs, _, _):-xs)
      = if length bs == t then
          bs ++ f2 ts xs
        else
          error "unzip4SADF: Process does not produce correct number of tokens"
    f3 [] _ = []
    f3 _ NullS      = []
    f3 (t:ts) ((_, _, cs, _):-xs)
      = if length cs == t then
          cs ++ f3 ts xs
        else
          error "unzip4SADF: Process does not produce correct number of tokens"
    f4 [] _ = []
    f4 _ NullS      = []
    f4 (t:ts) ((_, _, _, ds):-xs)
      = if length ds == t then
          ds ++ f4 ts xs
        else
          error "unzip4SADF: Process does not produce correct number of tokens"

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


get_prodToken :: Signal (a,b,c) -> [b]
get_prodToken NullS = []
get_prodToken ((_, x, _):-xs) = x : get_prodToken xs


switch_prodToken :: Signal (a,b,c) -> Signal (a,Int,c)
switch_prodToken NullS = NullS
switch_prodToken ((a, _, c):-xs) = signal [(a, 1, c)] +-+ switch_prodToken xs
