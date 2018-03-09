-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.CSDF
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

module ForSyDe.Shallow.MoC.CSDF (
  -- -- * Combinational Process Constructors
  -- -- | Combinational process constructors are used for processes that
  -- -- do not have a state.
  -- mapCSDF, zipWithCSDF, zipWith3CSDF, zipWith4CSDF,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delayCSDF, delaynCSDF,
  -- -- * Processes
  -- -- | Processes to unzip a signal of tupels into a tuple of signals
  -- unzipCSDF, unzip3CSDF, unzip4CSDF,
  -- * Actors
  -- | Based on the process constructors in the CSDF-MoC, the
  -- CSDF-library provides CSDF-actors with single or multiple inputs
  actor11CSDF, actor12CSDF, actor13CSDF, actor14CSDF,
  actor21CSDF, actor22CSDF, actor23CSDF, actor24CSDF,
  actor31CSDF, actor32CSDF, actor33CSDF, actor34CSDF,
  actor41CSDF, actor42CSDF, actor43CSDF, actor44CSDF
  ) where

import ForSyDe.Shallow.Core


-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delayCSDF' delays the signal one event
--   cycle by introducing an initial value at the beginning of the
--   output signal. Note, that this implies that there is one event
--   (the first) at the output signal that has no corresponding event at
--   the input signal. One could argue that input and output signals
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
-- one input and one output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor11CSDF :: [(Int, Int, [a] -> [b])] -> Signal a -> Signal b
actor11CSDF = mapCSDF

-- | The process constructor 'actor21CSDF' constructs an CSDF actor with
-- two input and one output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor21CSDF :: [((Int, Int), Int, [a] -> [b] -> [c])]
            -> Signal a -> Signal b -> Signal c
actor21CSDF = zipWithCSDF

-- | The process constructor 'actor31CSDF' constructs an CSDF actor with
-- three input and one output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor31CSDF :: [((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])]
            -> Signal a -> Signal b -> Signal c -> Signal d
actor31CSDF = zipWith3CSDF

-- | The process constructor 'actor41CSDF' constructs an CSDF actor with
-- four input and one output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor41CSDF :: [((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])]
            -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
actor41CSDF = zipWith4CSDF

-- > Actors with two outputs

-- | The process constructor 'actor12CSDF' constructs an CSDF actor with
-- one input and two output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor12CSDF :: [(Int, (Int, Int), [a] -> ([b], [c]))]
            -> Signal a -> (Signal b, Signal c)
actor12CSDF s xs = unzipCSDF (outputTokens s) $ mapCSDF (inpOut1n s) xs

-- | The process constructor 'actor22CSDF' constructs an CSDF actor with
-- two input and two output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor22CSDF :: [((Int, Int), (Int, Int), [a] -> [b] -> ([c], [d]))]
            -> Signal a -> Signal b -> (Signal c, Signal d)
actor22CSDF s xs ys = unzipCSDF (outputTokens s) $ zipWithCSDF (inpOut2n s) xs ys

-- | The process constructor 'actor32CSDF' constructs an CSDF actor with
-- three input and two output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor32CSDF :: [((Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> ([d], [e]))]
            -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e)
actor32CSDF s as bs cs
  = unzipCSDF (outputTokens s) $ zipWith3CSDF (inpOut3n s) as bs cs

-- | The process constructor 'actor42CSDF' constructs an CSDF actor with
-- four input and two output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor42CSDF :: [((Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f]))]
            -> Signal a -> Signal b -> Signal c -> Signal d
            -> (Signal e, Signal f)
actor42CSDF s as bs cs ds
  = unzipCSDF (outputTokens s) $ zipWith4CSDF (inpOut4n s) as bs cs ds

-- > Actors with three outputs

-- | The process constructor 'actor13CSDF' constructs an CSDF actor with
-- one input and three output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor13CSDF :: [(Int, (Int, Int, Int), [a] -> ([b], [c], [d]))]
            -> Signal a -> (Signal b, Signal c, Signal d)
actor13CSDF s xs = unzip3CSDF (outputTokens s) $ mapCSDF (inpOut1n s) xs

-- | The process constructor 'actor23CSDF' constructs an CSDF actor with
-- two input and three output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor23CSDF :: [((Int, Int), (Int, Int, Int), [a] -> [b] -> ([c], [d], [e]))]
            -> Signal a -> Signal b -> (Signal c, Signal d, Signal e)
actor23CSDF s xs ys
  = unzip3CSDF (outputTokens s) $ zipWithCSDF (inpOut2n s) xs ys

-- | The process constructor 'actor33CSDF' constructs an CSDF actor with
-- three input and three output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor33CSDF :: [((Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f]))]
            -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e, Signal f)
actor33CSDF s as bs cs
  = unzip3CSDF (outputTokens s) $ zipWith3CSDF (inpOut3n s) as bs cs

-- | The process constructor 'actor43CSDF' constructs an CSDF actor with
-- four input and three output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor43CSDF :: [((Int, Int, Int, Int), (Int, Int, Int),
            [a] -> [b] -> [c] -> [d] -> ([e], [f], [g]))]
            -> Signal a -> Signal b -> Signal c -> Signal d
            -> (Signal e, Signal f, Signal g)
actor43CSDF s as bs cs ds
  = unzip3CSDF (outputTokens s) $ zipWith4CSDF (inpOut4n s) as bs cs ds

-- > Actors with four outputs

-- | The process constructor 'actor14CSDF' constructs an CSDF actor with
-- one input and four output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor14CSDF :: [(Int, (Int, Int, Int, Int), [a] -> ([b], [c], [d], [e]))]
            -> Signal a -> (Signal b, Signal c, Signal d, Signal e)
actor14CSDF s xs = unzip4CSDF (outputTokens s) $ mapCSDF (inpOut1n s) xs

-- | The process constructor 'actor24CSDF' constructs an CSDF actor with
-- two input and four output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor24CSDF :: [((Int, Int), (Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f]))]
            -> Signal a -> Signal b
            -> (Signal c, Signal d, Signal e, Signal f)
actor24CSDF s xs ys
  = unzip4CSDF (outputTokens s) $ zipWithCSDF (inpOut2n s) xs ys

-- | The process constructor 'actor34CSDF' constructs an CSDF actor with
-- three input and four output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor34CSDF :: [((Int, Int, Int), (Int, Int, Int, Int),
            [a] -> [b] -> [c] -> ([d], [e], [f], [g]))]
            -> Signal a -> Signal b -> Signal c
            -> (Signal d, Signal e, Signal f, Signal g)
actor34CSDF s as bs cs
  = unzip4CSDF (outputTokens s) $ zipWith3CSDF (inpOut3n s) as bs cs

-- | The process constructor 'actor44CSDF' constructs an CSDF actor with
-- four input and four output signals. For each firing, the actor behaves
-- accordingly to the scenario (a tuple with the number of consumed tokens,
-- produced tokens and the function) defined in the list of tuples, given as
-- argument, in a cyclic fashion. The length of the list of scenarios gives the
-- actor's cycle period.
actor44CSDF :: [((Int, Int, Int, Int), (Int, Int, Int, Int),
            [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h]))]
            -> Signal a -> Signal b -> Signal c -> Signal d
            -> (Signal e, Signal f, Signal g, Signal h)
actor44CSDF s as bs cs ds
  = unzip4CSDF (outputTokens s) $ zipWith4CSDF (inpOut4n s) as bs cs ds


------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS
------------------------------------------------------------------------

-- | The process constructor 'mapCSDF' takes a list of scenarios, where each
-- scenario is a tuple @(c, p, f)@ containing the number of consumed tokens (@c@),
-- produced tokens (@p@) and corresponding functions (@f@) that operates on
-- a list, and results in an CSDF-process that takes an input signal
-- and results in an output signal
mapCSDF :: [(Int, Int, [a] -> [b])] -> Signal a -> Signal b
mapCSDF [] _ = error "mapCSDF: List of functions must not be empty"
mapCSDF (s:ss) xs
  | c < 0 = error "mapCSDF: Number of consumed tokens must be a non-negative integer"
  | not $ sufficient_tokens c xs  = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ mapCSDF (ss++[s]) (dropS c xs)
                else
                  error "mapCSDF: Function does not produce correct number of tokens"
  where (c, p, f) = s
        consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithCSDF' takes a list of scenarios, where each
-- scenario is a tuple @(c, p, f)@ containing the number of consumed tokens (@c@),
-- produced tokens (@p@) and corresponding functions (@f@)
-- that operates on two lists, and results in an CSDF-process that takes two
-- input signals and results in an output signal
zipWithCSDF :: [((Int, Int), Int, [a] -> [b] -> [c])]
            -> Signal a -> Signal b -> Signal c
zipWithCSDF [] _ _ = error "zipWithCSDF: List of functions must not be empty"
zipWithCSDF (s:ss) as bs
  | c1 < 0 || c2 < 0  = error "zipWithCSDF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as) || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithCSDF (ss++[s]) (dropS c1 as) (dropS c2 bs)
                else
                  error "zipWithCSDF: Function does not produce correct number of tokens"
  where (c, p, f) = s
        (c1, c2) = c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs


-- | The process constructor 'zipWith3CSDF' takes a list of scenarios, where each
-- scenario is a tuple @(c, p, f)@ containing the number of consumed tokens (@c@),
-- produced tokens (@p@) and corresponding functions (@f@)
-- that operates on three lists, and results in an SDF-process that takes three
-- input signals and results in an output signal
zipWith3CSDF :: [((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])]
             -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3CSDF [] _ _ _ = error "zipWith3CSDF: List of functions must not be empty"
zipWith3CSDF (s:ss) as bs cs
  | c1 < 0 || c2 < 0 || c3 < 0
  = error "zipWith3CSDF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs) = NullS
  | otherwise = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith3CSDF (ss++[s]) (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
    else
      error "zipWith3CSDF: Function does not produce correct number of tokens"
  where (c, p, f) = s
        (c1, c2, c3) = c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs


-- | The process constructor 'zipWith4CSDF' takes a list of scenarios, where each
-- scenario is a tuple @(c, p, f)@ containing the number of consumed tokens (@c@),
-- produced tokens (@p@) and corresponding functions (@f@) that
-- operates on three lists, and results in an CSDF-process that takes
-- three input signals and results in an output signal
zipWith4CSDF :: [((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])]
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4CSDF [] _ _ _ _ = error "zipWith4CSDF: List of functions must not be empty"
zipWith4CSDF (s:ss) as bs cs ds
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0
    = error "zipWith4CSDF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds) = NullS
  | otherwise = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith4CSDF (ss++[s])
             (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
    else
      error "zipWith4CSDF: Function does not produce correct number of tokens"
  where (c, p, f) = s
        (c1, c2, c3, c4) = c
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        produced_tokens = f consumed_tokens_as consumed_tokens_bs
                            consumed_tokens_cs consumed_tokens_ds



------------------------------------------------------------------------
-- unzipCSDF Processes
------------------------------------------------------------------------

unzipCSDF :: [(Int, Int)] -> Signal ([a], [b]) -> (Signal a, Signal b)
unzipCSDF [] _ = (NullS, NullS)
unzipCSDF _ NullS = (NullS, NullS)
unzipCSDF ((p1, p2) : ps) ((s1, s2) :- ss)
  | length s1 /= p1 || length s2 /= p2 = error "unzipCSDF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2)
  where (sr1, sr2) = unzipCSDF (ps ++ [(p1, p2)]) ss


unzip3CSDF :: [(Int, Int, Int)] -> Signal ([a], [b], [c])
           -> (Signal a, Signal b, Signal c)
unzip3CSDF [] _ = (NullS, NullS, NullS)
unzip3CSDF _ NullS = (NullS, NullS, NullS)
unzip3CSDF ((p1, p2, p3) : ps) ((s1, s2, s3) :- ss)
  | length s1 /= p1 || length s2 /= p2
    || length s3 /= p3 = error "unzip3CSDF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3)
  where (sr1, sr2, sr3) = unzip3CSDF (ps ++ [(p1, p2, p3)]) ss


unzip4CSDF :: [(Int, Int, Int, Int)] -> Signal ([a], [b], [c], [d])
           -> (Signal a, Signal b, Signal c, Signal d)
unzip4CSDF [] _ = (NullS, NullS, NullS, NullS)
unzip4CSDF _ NullS = (NullS, NullS, NullS, NullS)
unzip4CSDF ((p1, p2, p3, p4) : ps) ((s1, s2, s3, s4) :- ss)
  | length s1 /= p1 || length s2 /= p2
    || length s3 /= p3 || length s4 /= p4 = error "unzip4CSDF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3, signal s4 +-+ sr4)
  where (sr1, sr2, sr3, sr4) = unzip4CSDF (ps ++ [(p1, p2, p3, p4)]) ss


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


outputTokens :: [(a, b, c)] -> [b]
outputTokens [] = []
outputTokens ((_, b, _):xs) = b : outputTokens xs


inpOut1n :: [(it, ot, [a] -> y)] -> [(it, Int, [a] -> [y])]
inpOut1n [] = []
inpOut1n ((it, _, f):xs) = (it, 1, \a -> [f a]) : inpOut1n xs

inpOut2n :: [(it, ot, [a] -> [b] -> y)] -> [(it, Int, [a] -> [b] -> [y])]
inpOut2n [] = []
inpOut2n ((it, _, f):xs) = (it, 1, \a b -> [f a b]) : inpOut2n xs

inpOut3n :: [(it, ot, [a] -> [b] -> [c] -> y)] -> [(it, Int, [a] -> [b] -> [c] -> [y])]
inpOut3n [] = []
inpOut3n ((it, _, f):xs) = (it, 1, \a b c -> [f a b c]) : inpOut3n xs

inpOut4n :: [(it, ot, [a] -> [b] -> [c] -> [d] -> y)] -> [(it, Int, [a] -> [b] -> [c] -> [d] -> [y])]
inpOut4n [] = []
inpOut4n ((it, _, f):xs) = (it, 1, \a b c d -> [f a b c d]) : inpOut4n xs

------------------------------------------------------------------------
--
-- Test of Library (not exported)
--
------------------------------------------------------------------------

{-
---------------------------------------------------------
-- test1: CSDF graph from the paper Cyclo-Static Dataflow
---------------------------------------------------------

test1 :: Num a => Signal a
test1 = s3
  where s3 = delaynCSDF [1,1] s2
        s2 = v2 s1
        s1 = v1 s4
        s4 = v3 s3
        v1 = actor11CSDF [(1, 1, \[a] -> [a]), (1, 0, \_ -> []), (1, 0, \_ -> [])]
        v2 = actor11CSDF [(1, 0, \_ -> []), (1, 2, \[a] -> [a, 2*a])]
        v3 = actor11CSDF [(1, 3, \[a] -> [a, 2*a, 3*a])]

-- Shows the first 10 values of the output (signal s3)
test1out = takeS 10 test1

-- Expected answer: {1,1,1,2,2,4,4,8,8,16}

---------------------------------------------------------
-- test2: actor22CSDF test
---------------------------------------------------------

test2 :: Num a => Signal a -> Signal a -> (Signal a, Signal a)
test2 = actor22CSDF s
  where s = [((2,1), (0,1), \[a,b] [c] -> ([], [a+b+c])),
             ((1,3), (2,3), \[a] [b,c,d] -> ([a,b], [b, c, d]))]

-- Shows the output for the given inputs
test2out = test2 (signal [1..10]) (signal [11..20])

-- Expected answer: ({3,12,6,16},{14,12,13,14,24,16,17,18,34})

---------------------------------------------------------
-- test3: actor34CSDF test
---------------------------------------------------------

test3 :: (Num a, Enum b) => Signal a -> Signal a -> Signal b -> (Signal b, Signal b, Signal a, Signal a)
test3 = actor34CSDF s
  where s = [((1,0,1), (1,1,3,0), \[a] _ [b] -> ([b], [succ b], [a, 2*a, 3*a], [])),
             ((2,1,1), (0,2,1,1), \[a,b] [c] [d] -> ([], [d, succ d], [a+b], [c]))]

test3out = test3 (signal [1..10]) (signal [11..20]) (signal ['a'..'k'])

-- Expected answer: ({'a','c','e','g'},{'b','b','c','d','d','e','f','f','g','h'},
-- {1,2,3,5,4,8,12,11,7,14,21,17,10,20,30},{11,12,13})
-}
