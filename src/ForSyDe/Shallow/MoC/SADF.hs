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
  mapSADF, zipWithSADF, zipWith3SADF, zipWith4SADF, zipWith5SADF,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delaySADF, delaynSADF,
  -- * Processes
  -- | Processes to unzip a signal of tupels into a tuple of signals
  unzipSADF, unzip3SADF, unzip4SADF, unzip5SADF,
  -- * Kernels
  -- | Based on the process constructors in the SADF-MoC, the
  -- SADF-library provides SADF-kernels with single or multiple inputs
  kernel11SADF, kernel12SADF, kernel13SADF, kernel14SADF, kernel15SADF,
  kernel21SADF, kernel22SADF, kernel23SADF, kernel24SADF, kernel25SADF,
  kernel31SADF, kernel32SADF, kernel33SADF, kernel34SADF, kernel35SADF,
  kernel41SADF, kernel42SADF, kernel43SADF, kernel44SADF, kernel45SADF,
  kernel51SADF, kernel52SADF, kernel53SADF, kernel54SADF, kernel55SADF,
  -- * Detectors
  -- | Based on the process constructors in the SADF-MoC, the
  -- SADF-library provides SADF-detectors with single or multiple inputs
  detector11SADF, detector12SADF, detector13SADF, detector14SADF,
  detector21SADF, detector22SADF, detector23SADF, detector24SADF,
  detector31SADF, detector32SADF, detector33SADF, detector34SADF,
  detector41SADF, detector42SADF, detector43SADF, detector44SADF
  ) where

import ForSyDe.Shallow.Core
import Data.List(unzip4, unzip5)

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


-- | The process constructor 'zipWith5SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes five input signals and
-- results in an output signal
zipWith5SADF :: Signal ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
zipWith5SADF NullS _ _ _ _ _ = NullS
zipWith5SADF ct as bs cs ds es
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0 || c5 < 0
    = error "zipWith5SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds)
    || (not $ sufficient_tokens c5 es) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith5SADF (tailS ct) (dropS c1 as)
                                              (dropS c2 bs) (dropS c3 cs) (dropS c4 ds) (dropS c5 es)
                else
                  error "zipWith5SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3, c4, c5), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        consumed_tokens_es = fromSignal $ takeS c5 es
        produced_tokens = f consumed_tokens_as consumed_tokens_bs
                            consumed_tokens_cs consumed_tokens_ds consumed_tokens_es

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
-- SADF KERNELS
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

-- | The process constructor 'kernel51SADF' constructs an SADF kernel with
-- five data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel51SADF :: Signal ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
kernel51SADF = zipWith5SADF


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

-- | The process constructor 'kernel52SADF' constructs an SADF kernel with
-- five data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel52SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> [([f], [g])])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g)
kernel52SADF ct as bs cs ds es
  = unzipSADF (get_prodToken ct) $ zipWith5SADF (switch_prodToken ct) as bs cs ds es


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

-- | The process constructor 'kernel53SADF' constructs an SADF kernel with
-- five data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel53SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> [([f], [g], [h])])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h)
kernel53SADF ct as bs cs ds es
  = unzip3SADF (get_prodToken ct) $ zipWith5SADF (switch_prodToken ct) as bs cs ds es


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

-- | The process constructor 'kernel54SADF' constructs an SADF kernel with
-- five data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel54SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> [([f], [g], [h], [i])])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h, Signal i)
kernel54SADF ct as bs cs ds es
  = unzip4SADF (get_prodToken ct) $ zipWith5SADF (switch_prodToken ct) as bs cs ds es


-- > Kernals with five outputs

-- | The process constructor 'kernel15SADF' constructs an SADF kernel with
-- one data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel15SADF :: Signal (Int, (Int, Int, Int, Int, Int), [a] -> [([b], [c], [d], [e], [f])])
             -> Signal a -> (Signal b, Signal c, Signal d, Signal e, Signal f)
kernel15SADF ct xs = unzip5SADF (get_prodToken ct) $ mapSADF (switch_prodToken ct) xs

-- | The process constructor 'kernel25SADF' constructs an SADF kernel with
-- two data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel25SADF :: Signal ((Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [([c], [d], [e], [f], [g])])
             -> Signal a -> Signal b -> (Signal c, Signal d, Signal e, Signal f, Signal g)
kernel25SADF ct xs ys = unzip5SADF (get_prodToken ct) $ zipWithSADF (switch_prodToken ct) xs ys

-- | The process constructor 'kernel35SADF' constructs an SADF kernel with
-- three data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel35SADF :: Signal ((Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [([d], [e], [f], [g], [h])])
             -> Signal a -> Signal b -> Signal c
             -> (Signal d, Signal e, Signal f, Signal g, Signal h)
kernel35SADF ct as bs cs
  = unzip5SADF (get_prodToken ct) $ zipWith3SADF (switch_prodToken ct) as bs cs

-- | The process constructor 'kernel45SADF' constructs an SADF kernel with
-- four data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel45SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [([e], [f], [g], [h], [i])])
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g, Signal h, Signal i)
kernel45SADF ct as bs cs ds
  = unzip5SADF (get_prodToken ct) $ zipWith4SADF (switch_prodToken ct) as bs cs ds

-- | The process constructor 'kernel55SADF' constructs an SADF kernel with
-- five data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel55SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> [([f], [g], [h], [i], [j])])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h, Signal i, Signal j)
kernel55SADF ct as bs cs ds es
  = unzip5SADF (get_prodToken ct) $ zipWith5SADF (switch_prodToken ct) as bs cs ds es


------------------------------------------------------------------------
--
-- SADF DETECTORS
--
------------------------------------------------------------------------

-- > Detectors with one output

-- | The process constructor 'detector11SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with a single
-- data input and a single control output signals.
detector11SADF :: Int -> (e -> [a] -> e) -> (e -> [y]) -> e -> Signal a -> Signal y
detector11SADF c f g e0 as = outputFSM g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector21SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with two
-- data input and a single control output signals.
detector21SADF :: (Int, Int) -> (e -> [a] -> [b] -> e) -> (e -> [y])
               -> e -> Signal a -> Signal b -> Signal y
detector21SADF c f g e0 as bs = outputFSM g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector31SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with three
-- data input and a single control output signals.
detector31SADF :: (Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> e) -> (e -> [y])
               -> e -> Signal a -> Signal b -> Signal c -> Signal y
detector31SADF c f g e0 as bs cs = outputFSM g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector41SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with four
-- data input and a single control output signals.
detector41SADF :: (Int, Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> [d] -> e) -> (e -> [y])
               -> e -> Signal a -> Signal b -> Signal c -> Signal d -> Signal y
detector41SADF c f g e0 as bs cs ds = outputFSM g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF e0 next_state


-- > Detectors with two output

-- | The process constructor 'detector12SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with a single
-- data input and two control output signals.
detector12SADF :: Int -> (e -> [a] -> e) -> (e -> ([y1], [y2])) -> e -> Signal a -> (Signal y1, Signal y2)
detector12SADF c f g e0 as = unzipSADF p outs
  where (p, outs) = outputFSM2 g next_state
        next_state = nextStateFSM c f current_state as
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector22SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with two
-- data input and two control output signals.
detector22SADF :: (Int, Int) -> (e -> [a] -> [b] -> e) -> (e -> ([y1], [y2]))
               -> e -> Signal a -> Signal b -> (Signal y1, Signal y2)
detector22SADF c f g e0 as bs = unzipSADF p outs
  where (p, outs) = outputFSM2 g next_state
        next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector32SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with three
-- data input and two control output signals.
detector32SADF :: (Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> e) -> (e -> ([y1], [y2]))
               -> e -> Signal a -> Signal b -> Signal c -> (Signal y1, Signal y2)
detector32SADF c f g e0 as bs cs = unzipSADF p outs
  where (p, outs) = outputFSM2 g next_state
        next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector42SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with four
-- data input and two control output signals.
detector42SADF :: (Int, Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> [d] -> e) -> (e -> ([y1], [y2]))
               -> e -> Signal a -> Signal b -> Signal c -> Signal d -> (Signal y1, Signal y2)
detector42SADF c f g e0 as bs cs ds = unzipSADF p outs
  where (p, outs) = outputFSM2 g next_state
        next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF e0 next_state


-- > Detectors with three output

-- | The process constructor 'detector13SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with a single
-- data input and three control output signals.
detector13SADF :: Int -> (e -> [a] -> e) -> (e -> ([y1], [y2], [y3]))
               -> e -> Signal a -> (Signal y1, Signal y2, Signal y3)
detector13SADF c f g e0 as = unzip3SADF p outs
  where (p, outs) = outputFSM3 g next_state
        next_state = nextStateFSM c f current_state as
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector23SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with two
-- data input and three control output signals.
detector23SADF :: (Int, Int) -> (e -> [a] -> [b] -> e) -> (e -> ([y1], [y2], [y3]))
               -> e -> Signal a -> Signal b -> (Signal y1, Signal y2, Signal y3)
detector23SADF c f g e0 as bs = unzip3SADF p outs
  where (p, outs) = outputFSM3 g next_state
        next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector33SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with three
-- data input and three control output signals.
detector33SADF :: (Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> e) -> (e -> ([y1], [y2], [y3]))
               -> e -> Signal a -> Signal b -> Signal c -> (Signal y1, Signal y2, Signal y3)
detector33SADF c f g e0 as bs cs = unzip3SADF p outs
  where (p, outs) = outputFSM3 g next_state
        next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector43SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with four
-- data input and three control output signals.
detector43SADF :: (Int, Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> [d] -> e)
               -> (e -> ([y1], [y2], [y3])) -> e -> Signal a -> Signal b -> Signal c
               -> Signal d -> (Signal y1, Signal y2, Signal y3)
detector43SADF c f g e0 as bs cs ds = unzip3SADF p outs
  where (p, outs) = outputFSM3 g next_state
        next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF e0 next_state


-- > Detectors with four output

-- | The process constructor 'detector14SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with a single
-- data input and four control output signals.
detector14SADF :: Int -> (e -> [a] -> e) -> (e -> ([y1], [y2], [y3], [y4]))
               -> e -> Signal a -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector14SADF c f g e0 as = unzip4SADF p outs
  where (p, outs) = outputFSM4 g next_state
        next_state = nextStateFSM c f current_state as
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector24SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with two
-- data input and four control output signals.
detector24SADF :: (Int, Int) -> (e -> [a] -> [b] -> e) -> (e -> ([y1], [y2], [y3], [y4]))
               -> e -> Signal a -> Signal b -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector24SADF c f g e0 as bs = unzip4SADF p outs
  where (p, outs) = outputFSM4 g next_state
        next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector34SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with three
-- data input and four control output signals.
detector34SADF :: (Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> e)
               -> (e -> ([y1], [y2], [y3], [y4])) -> e -> Signal a -> Signal b
               -> Signal c -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector34SADF c f g e0 as bs cs = unzip4SADF p outs
  where (p, outs) = outputFSM4 g next_state
        next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF e0 next_state


-- | The process constructor 'detector44SADF' takes the number of consumed
-- (@c@) tokens, the state transition function (@f@), the output function (@g@)
-- and the initial state (@e0@), and constructs an SADF detector with four
-- data input and four control output signals.
detector44SADF :: (Int, Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> [d] -> e)
               -> (e -> ([y1], [y2], [y3], [y4])) -> e -> Signal a -> Signal b -> Signal c
               -> Signal d -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector44SADF c f g e0 as bs cs ds = unzip4SADF p outs
  where (p, outs) = outputFSM4 g next_state
        next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF e0 next_state

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


unzip5SADF :: [(Int, Int, Int, Int, Int)] -> Signal ([a], [b], [c], [d], [e])
           -> (Signal a, Signal b, Signal c, Signal d, Signal e)
unzip5SADF p xs = (s1, s2, s3, s4, s5)
  where
    (p1, p2, p3, p4, p5) = unzip5 p
    s1 = signal $ f1 p1 xs
    s2 = signal $ f2 p2 xs
    s3 = signal $ f3 p3 xs
    s4 = signal $ f4 p4 xs
    s5 = signal $ f5 p5 xs
    f1 [] _ = []
    f1 _ NullS      = []
    f1 (t:ts) ((as, _, _, _, _):-xs)
      = if length as == t then
          as ++ f1 (ts++[t]) xs
        else
          error "unzip5SADF: Process does not produce correct number of tokens"
    f2 [] _ = []
    f2 _ NullS      = []
    f2 (t:ts) ((_, bs, _, _, _):-xs)
      = if length bs == t then
          bs ++ f2 ts xs
        else
          error "unzip5SADF: Process does not produce correct number of tokens"
    f3 [] _ = []
    f3 _ NullS      = []
    f3 (t:ts) ((_, _, cs, _, _):-xs)
      = if length cs == t then
          cs ++ f3 ts xs
        else
          error "unzip5SADF: Process does not produce correct number of tokens"
    f4 [] _ = []
    f4 _ NullS      = []
    f4 (t:ts) ((_, _, _, ds, _):-xs)
      = if length ds == t then
          ds ++ f4 ts xs
        else
          error "unzip5SADF: Process does not produce correct number of tokens"
    f5 [] _ = []
    f5 _ NullS      = []
    f5 (t:ts) ((_, _, _, _, es):-xs)
      = if length es == t then
          es ++ f5 ts xs
        else
          error "unzip5SADF: Process does not produce correct number of tokens"
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


---------------------------------------------------------
-- Helper functios to the detector's Moore FSM
---------------------------------------------------------
nextStateFSM :: Int -> (e -> [a] -> e)
             -> Signal e -> Signal a -> Signal e
nextStateFSM _ _ NullS _ = NullS
nextStateFSM _ _ _ NullS = NullS
nextStateFSM c f es as
  | c <= 0 = error "nextStateFSM: Number of consumed tokens must be positive integer"
  | not $ sufficient_tokens c as = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM c f (tailS es) (dropS c as)
  where consumed_tokens_as = fromSignal $ takeS c as
        current_state = headS es
        next_state = f current_state consumed_tokens_as


nextStateFSM2 :: (Int, Int) -> (e -> [a] -> [b] -> e)
              -> Signal e -> Signal a -> Signal b -> Signal e
nextStateFSM2 _ _ NullS _ _ = NullS
nextStateFSM2 _ _ _ NullS _ = NullS
nextStateFSM2 _ _ _ _ NullS = NullS
nextStateFSM2 (c1, c2) f es as bs
  | c1 <= 0 || c2 <= 0 = error "nextStateFSM2: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM2 (c1, c2) f (tailS es) (dropS c1 as) (dropS c2 bs)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        current_state = headS es
        next_state = f current_state consumed_tokens_as consumed_tokens_bs


nextStateFSM3 :: (Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> e)
              -> Signal e -> Signal a -> Signal b -> Signal c -> Signal e
nextStateFSM3 _ _ NullS _ _ _ = NullS
nextStateFSM3 _ _ _ NullS _ _ = NullS
nextStateFSM3 _ _ _ _ NullS _ = NullS
nextStateFSM3 _ _ _ _ _ NullS = NullS
nextStateFSM3 (c1, c2, c3) f es as bs cs
  | c1 <= 0 || c2 <= 0 || c3 <= 0
    = error "nextStateFSM3: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM3 (c1, c2, c3) f (tailS es)
                                        (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        current_state = headS es
        next_state = f current_state consumed_tokens_as
                       consumed_tokens_bs consumed_tokens_cs


nextStateFSM4 :: (Int, Int, Int, Int) -> (e -> [a] -> [b] -> [c] -> [d] -> e)
              -> Signal e -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
nextStateFSM4 _ _ NullS _ _ _ _ = NullS
nextStateFSM4 _ _ _ NullS _ _ _ = NullS
nextStateFSM4 _ _ _ _ NullS _ _ = NullS
nextStateFSM4 _ _ _ _ _ NullS _ = NullS
nextStateFSM4 _ _ _ _ _ _ NullS = NullS
nextStateFSM4 (c1, c2, c3, c4) f es as bs cs ds
  | c1 <= 0 || c2 <= 0 || c3 <= 0 || c4 <= 0
    = error "nextStateFSM4: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM4 (c1, c2, c3, c4) f (tailS es)
                                        (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        current_state = headS es
        next_state = f current_state consumed_tokens_as
                       consumed_tokens_bs consumed_tokens_cs consumed_tokens_ds


outputFSM :: (e -> [a]) -> Signal e -> Signal a
outputFSM _ NullS = NullS
outputFSM f (e:-es) = signal (f e) +-+ outputFSM f es


outputFSM2 :: (e -> ([a], [b])) -> Signal e -> ([(Int, Int)] , Signal ([a], [b]))
outputFSM2 _ NullS = ([], NullS)
outputFSM2 f e = (p, outs)
  where outs = f1 e
        p = f2 e
        f1 NullS = NullS
        f1 (x:-xs) = signal [f x] +-+ f1 xs
        f2 NullS = []
        f2 (x:-xs) = (length as, length bs) : f2 xs
          where (as, bs) = f x


outputFSM3 :: (e -> ([a], [b], [c])) -> Signal e -> ([(Int, Int, Int)] , Signal ([a], [b], [c]))
outputFSM3 _ NullS = ([], NullS)
outputFSM3 f e = (p, outs)
  where outs = f1 e
        p = f2 e
        f1 NullS = NullS
        f1 (x:-xs) = signal [f x] +-+ f1 xs
        f2 NullS = []
        f2 (x:-xs) = (length as, length bs, length cs) : f2 xs
          where (as, bs, cs) = f x


outputFSM4 :: (e -> ([a], [b], [c], [d])) -> Signal e
           -> ([(Int, Int, Int, Int)] , Signal ([a], [b], [c], [d]))
outputFSM4 _ NullS = ([], NullS)
outputFSM4 f e = (p, outs)
  where outs = f1 e
        p = f2 e
        f1 NullS = NullS
        f1 (x:-xs) = signal [f x] +-+ f1 xs
        f2 NullS = []
        f2 (x:-xs) = (length as, length bs, length cs, length ds) : f2 xs
          where (as, bs, cs, ds) = f x


outputFSM5 :: (e -> ([a], [b], [c], [d], [g])) -> Signal e
           -> ([(Int, Int, Int, Int, Int)] , Signal ([a], [b], [c], [d], [g]))
outputFSM5 _ NullS = ([], NullS)
outputFSM5 f e = (p, outs)
  where outs = f1 e
        p = f2 e
        f1 NullS = NullS
        f1 (x:-xs) = signal [f x] +-+ f1 xs
        f2 NullS = []
        f2 (x:-xs) = (length as, length bs, length cs, length ds, length gs) : f2 xs
          where (as, bs, cs, ds, gs) = f x

------------------------------------------------------------------------
--
-- Test of Library (not exported)
--
------------------------------------------------------------------------

{-

---------------------------------------------------------
-- test1: kernel22SADF test
---------------------------------------------------------

test1 :: Signal ((Int, Int), (Int, Int), [a] -> [b] -> [([c], [d])])
      -> Signal a -> Signal b -> (Signal c, Signal d)
test1 = kernel22SADF

ct = signal [((1,1), (1,1), \[a] [b] -> [([2*a], [2*b])]),
             ((2,2), (1,1), \[a,b] [c,d] -> [([a+b], [c+d])]),
             ((1,2), (2,1), \[a] [b,c] -> [([b,c], [a])])]

x = signal [1..20]
y = signal [21 .. 40]

test1out = test1 ct x y

---------------------------------------------------------
-- test2: Anti Wind-up system
---------------------------------------------------------

-- State transition function for the detector
f :: (Num a, Ord a) => Int -> [a] -> [a] -> Int
f 1 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0) then 2 else 1
f 2 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0) then 2 else 1

-- Output function for the detector
g :: Num a => Int -> ([((Int, Int), Int, [a] -> [a] -> [a])], [(Int, (Int, Int), [a] -> [([a], [a])])])
g 1 = ([((1,1), 1, \[a] [b] -> [a+b])], [(1, (1,1), \[a] -> [([a], [a])])])
g 2 = ([((0,1), 1, \_ [b] -> [b])], [(1, (0,1), \[a] -> [([], [a])])])

-- Detector
detector :: (Num a, Ord a) => Signal a -> Signal a
         -> (Signal ((Int, Int), Int, [a] -> [a] -> [a]), Signal (Int, (Int, Int), [a] -> [([a], [a])]))
detector = detector22SADF (1,1) f g 1

syst :: (Num a, Ord a) => Signal a -> Signal a
syst input = output
  where output = integrator c1 s1 s3
        s3 = delaySADF 0 output
        (s1, s5) = fork c2 input
        s2 = delaySADF 0 s5
        (c1, c2) = detector s3 s2
        fork = kernel12SADF
        integrator = kernel21SADF


---------------------------------------------------------
-- test3: Register Bank (3 registers)
---------------------------------------------------------

-- Scenarios list
scenarios :: Int -> ((Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [a] -> [a] -> [a] -> [([a], [a], [a], [a])])
scenarios 0 = ((0,0,0,0), (0,0,0,0), \_ _ _ _ -> [([], [], [], [])])
scenarios 1 = ((0,1,0,0), (1,1,0,0), \_ [r1] _ _ -> [([r1], [r1], [], [])])
scenarios 2 = ((0,0,1,0), (1,0,1,0), \_ _ [r2] _ -> [([r2], [], [r2], [])])
scenarios 3 = ((0,0,0,1), (1,0,0,1), \_ _ _ [r3] -> [([r3], [], [], [r3])])
scenarios 4 = ((1,1,0,0), (0,1,0,0), \[r1] _ _ _ -> [([], [r1], [], [])])
scenarios 5 = ((1,0,1,0), (0,0,1,0), \[r2] _ _ _ -> [([], [], [r2], [])])
scenarios 6 = ((1,0,0,1), (0,0,0,1), \[r3] _ _ _ -> [([], [], [], [r3])])
scenarios _ = error "scenarios: outside the state list"

switchState :: Int -> [String] -> Int
switchState _ ["sc0"] = 0     -- No operation (kernel inactive)
switchState _ ["lr1"] = 1     -- Load r1
switchState _ ["lr2"] = 2     -- Load r2
switchState _ ["lr3"] = 3     -- Load r3
switchState _ ["sr1"] = 4     -- Store r1
switchState _ ["sr2"] = 5     -- Store r2
switchState _ ["sr3"] = 6     -- Store r3
switchState _ _ = error "switchState: Input not recognized"

regDetector = detector11SADF 1 switchState (\e -> [scenarios e]) 0
regKernel = kernel44SADF

registerBank inputControl inputData = output
  where ct = regDetector inputControl
        (output, r1, r2, r3) = regKernel ct inputData r1' r2' r3'
        r1' = delaySADF 0 r1
        r2' = delaySADF 0 r2
        r3' = delaySADF 0 r3

cInput = signal ["lr1","lr2","lr3","sr1","sr2","sr3","sc0","lr1","lr2","lr3","lr1","lr2","lr3"]
dInput = signal [1..10]

regOutput = registerBank cInput dInput
-- Expected output {0,0,0,1,2,3,1,2,3}


-}
