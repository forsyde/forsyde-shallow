-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.SDF
-- Copyright   :  (c) Ingo Sander, KTH/ICT/ES, ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- SDFLib.hs, yet to be completed.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.SDF (
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  -- do not have a state.
  mapSDF, zipWithSDF, zipWith3SDF, zipWith4SDF,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delaySDF, delaynSDF,
  -- * Processes
  -- | Processes to unzip a signal of tupels into a tuple of signals
  unzipSDF, unzip3SDF, unzip4SDF,
  -- * Actors
  -- | Based on the process constructors in the SDF-MoC, the
  -- SDF-library provides SDF-actors with single or multiple inputs
  actor11SDF, actor12SDF, actor13SDF, actor14SDF,
  actor21SDF, actor22SDF, actor23SDF, actor24SDF,
  actor31SDF, actor32SDF, actor33SDF, actor34SDF,
  actor41SDF, actor42SDF, actor43SDF, actor44SDF
  ) where  

import ForSyDe.Shallow.Core

------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS
------------------------------------------------------------------------

-- | The process constructor 'mapSDF' takes the number of consumed
-- (@c@) and produced (@p@) tokens and a function @f@ that operates on
-- a list, and results in an SDF-process that takes an input signal
-- and results in an output signal
mapSDF :: Int -> Int -> ([a] -> [b]) -> Signal a -> Signal b
mapSDF _ _ _ NullS   = NullS
mapSDF c p f xs     
  | c <= 0 = error "mapSDF: Number of consumed tokens must be positive integer" 
  | not $ sufficient_tokens c xs  = NullS
  | otherwise  = if length produced_tokens == p then
                   signal produced_tokens +-+ mapSDF c p f (dropS c xs) 
                 else   
                   error "mapSDF: Function does not produce correct number of tokens" 
  where consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens

-- | The process constructor 'zipWithSDF' takes a tuple @(c1, c2)@
-- denoting the number of consumed tokens and an integer @p@ denoting
-- the number of produced tokens and a function @f@
-- that operates on two lists, and results in an SDF-process that takes two
-- input signals and results in an output signal
zipWithSDF :: (Int, Int) -> Int -> ([a] -> [b] -> [c])
           -> Signal a -> Signal b -> Signal c                  
zipWithSDF (_, _) _ _ NullS _ = NullS
zipWithSDF (_, _) _ _ _ NullS = NullS
zipWithSDF (c1, c2) p f as bs 
  | c1 <= 0 || c2 <= 0  = error "zipWithSDF: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as) 
    || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithSDF (c1, c2) p f (dropS c1 as) (dropS c2 bs)  
                else
                  error "zipWithSDF: Function does not produce correct number of tokens"
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs

-- | The process constructor 'zipWith3SDF' takes a tuple @(c1, c2, c3)@
-- denoting the number of consumed tokens and an integer @p@ denoting
-- the number of produced tokens and a function @f@
-- that operates on three lists, and results in an SDF-process that takes three
-- input signals and results in an output signal  
zipWith3SDF :: (Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d]) 
            -> Signal a -> Signal b -> Signal c -> Signal d                 
zipWith3SDF (_, _, _) _ _ NullS _ _= NullS
zipWith3SDF (_, _, _) _ _ _ NullS _= NullS
zipWith3SDF (_, _, _) _ _ _ _ NullS= NullS
zipWith3SDF (c1, c2, c3) p f as bs cs
  | c1 <= 0 || c2 <= 0 || c3 <= 0
  = error "zipWith3SDF: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as) 
    || (not $ sufficient_tokens c2 bs)    
    || (not $ sufficient_tokens c3 cs)
  = NullS
  | otherwise
  = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith3SDF (c1, c2, c3) p f
                                 (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
    else
      error "zipWith3SDF: Function does not produce correct number of tokens"
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs
  

-- | The process constructor 'zipWith4SDF' takes a tuple @(c1, c2, c3,c4)@
-- denoting the number of consumed tokens and an integer @p@
-- denoting the number of produced tokens and a function @f@ that
-- operates on three lists, and results in an SDF-process that takes
-- three input signals and results in an output signal
zipWith4SDF :: (Int, Int, Int, Int) -> Int 
            -> ([a] -> [b] -> [c] -> [d] -> [e]) 
            -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e 
zipWith4SDF (_, _, _, _) _ _ NullS _ _ _ = NullS
zipWith4SDF (_, _, _, _) _ _ _ NullS _ _ = NullS
zipWith4SDF (_, _, _, _) _ _ _ _ NullS _ = NullS
zipWith4SDF (_, _, _, _) _ _ _ _ _ NullS = NullS
zipWith4SDF (c1, c2, c3, c4) p f as bs cs ds
  | c1 <= 0 || c2 <= 0 || c3 <= 0 || c4 <= 0
  = error "zipWith4SDF: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as) 
    || (not $ sufficient_tokens c2 bs)    
    || (not $ sufficient_tokens c3 cs)    
    || (not $ sufficient_tokens c4 ds)    
  = NullS
  | otherwise    
  = if length produced_tokens == p then
      signal produced_tokens +-+ zipWith4SDF (c1, c2, c3, c4) p f
             (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
    else
      error "zipWith4SDF: Function does not produce correct number of tokens"
  where consumed_tokens_as = fromSignal $ takeS c1 as
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

-- | The process constructor 'delaySDF' delays the signal one event
--   cycle by introducing an initial value at the beginning of the
--   output signal.  Note, that this implies that there is one event
--   (the first) at the output signal that has no corresponding event at
--   the input signal.  One could argue that input and output signals
--   are not fully synchronized, even though all input events are
--   synchronous with a corresponding output event. However, this is
--   necessary to initialize feed-back loops.
delaySDF :: a -> Signal a -> Signal a
delaySDF x xs = x :- xs


-- | The process constructor 'delaynSDF' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.  
delaynSDF :: [a] -> Signal a -> Signal a 
delaynSDF initial_tokens xs = signal initial_tokens +-+ xs 

------------------------------------------------------------------------
--
-- SDF ACTORS
--
------------------------------------------------------------------------

-- > Actors with one output

-- | The process constructor 'actor11SDF' constructs an SDF actor with
-- one input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor11SDF :: Int -> Int -> ([a] -> [b]) -> Signal a -> Signal b
actor11SDF = mapSDF     

-- | The process constructor 'actor21SDF' constructs an SDF actor with
-- two input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor21SDF :: (Int, Int) -> Int -> ([a] -> [b] -> [c]) -> Signal a -> Signal b -> Signal c    
actor21SDF = zipWithSDF

-- | The process constructor 'actor31SDF' constructs an SDF actor with
-- three input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor31SDF :: (Int, Int, Int) -> Int -> ([a] -> [b] -> [c] -> [d])
       -> Signal a -> Signal b -> Signal c -> Signal d   
actor31SDF = zipWith3SDF

-- | The process constructor 'actor41SDF' constructs an SDF actor with
-- four input and one output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor41SDF :: (Int, Int, Int, Int) -> Int 
    -> ([a] -> [b] -> [c] -> [d] -> [e]) 
    -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e 
actor41SDF = zipWith4SDF


-- > Actors with two outputs

-- | The process constructor 'actor12SDF' constructs an SDF actor with
-- one input and two output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor12SDF :: Int -> (Int, Int) -> ([a] -> [([b], [c])])
           -> Signal a -> (Signal b, Signal c)
actor12SDF c (p1,p2) f xs = unzipSDF (p1, p2) $ mapSDF c 1 f xs  

-- | The process constructor 'actor22SDF' constructs an SDF actor with
-- two input and two output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor22SDF :: (Int, Int) -> (Int, Int) -> ([a] -> [b] -> [([c], [d])])
           -> Signal a -> Signal b -> (Signal c, Signal d)
actor22SDF (c1, c2) (p1, p2) f xs ys = unzipSDF (p1, p2) $ zipWithSDF (c1, c2) 1 f xs ys

-- | The process constructor 'actor32SDF' constructs an SDF actor with
-- three input and two output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor32SDF :: (Int, Int, Int) -> (Int, Int)
           -> ([a] -> [b] -> [c] -> [([d], [e])])
           -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e)
actor32SDF (c1, c2, c3) (p1, p2) f as bs cs
  = unzipSDF (p1, p2) $ zipWith3SDF (c1, c2, c3) 1 f as bs cs

-- | The process constructor 'actor42SDF' constructs an SDF actor with
-- four input and two output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor42SDF :: (Int, Int, Int, Int) -> (Int, Int) 
           -> ([a] -> [b] -> [c] -> [d] -> [([e], [f])]) 
           -> Signal a -> Signal b -> Signal c -> Signal d 
           -> (Signal e, Signal f)
actor42SDF (c1, c2, c3, c4) (p1, p2) f as bs cs ds 
  = unzipSDF (p1, p2)$ zipWith4SDF (c1, c2, c3, c4) 1 f as bs cs ds

-- > Actors with three outputs

-- | The process constructor 'actor13SDF' constructs an SDF actor with
-- one input and three output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor13SDF :: Int -> (Int, Int, Int) 
           -> ([a] -> [([b], [c], [d])]) 
           -> Signal a -> (Signal b, Signal c, Signal d)
actor13SDF c (p1, p2, p3) f xs = unzip3SDF (p1, p2, p3) $ mapSDF c 1 f xs  

-- | The process constructor 'actor23SDF' constructs an SDF actor with
-- two input and three output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor23SDF :: (Int, Int) -> (Int, Int, Int) 
           -> ([a] -> [b] -> [([c], [d], [e])]) 
           -> Signal a -> Signal b 
           -> (Signal c, Signal d, Signal e)
actor23SDF (c1, c2) (p1, p2, p3) f xs ys
  = unzip3SDF (p1, p2, p3) $ zipWithSDF (c1, c2) 1 f xs ys

-- | The process constructor 'actor33SDF' constructs an SDF actor with
-- three input and three output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor33SDF :: (Int, Int, Int) -> (Int, Int, Int) 
           -> ([a] -> [b] -> [c] -> [([d], [e], [f])]) 
           -> Signal a -> Signal b -> Signal c -> (Signal d, Signal e, Signal f)
actor33SDF (c1, c2, c3) (p1, p2, p3) f as bs cs
  = unzip3SDF (p1, p2, p3) $ zipWith3SDF (c1, c2, c3) 1 f as bs cs

-- | The process constructor 'actor43SDF' constructs an SDF actor with
-- four input and three output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor43SDF :: (Int, Int, Int, Int) -> (Int, Int, Int) 
           -> ([a] -> [b] -> [c] -> [d] -> [([e], [f], [g])]) 
           -> Signal a -> Signal b -> Signal c -> Signal d 
           -> (Signal e, Signal f, Signal g)
actor43SDF (c1, c2, c3, c4) (p1, p2, p3) f as bs cs ds 
  = unzip3SDF (p1, p2, p3)$ zipWith4SDF (c1, c2, c3, c4) 1 f as bs cs ds

-- > Actors with four outputs

-- | The process constructor 'actor14SDF' constructs an SDF actor with
-- one input and four output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor14SDF :: Int -> (Int, Int, Int, Int) 
           -> ([a] -> [([b], [c], [d], [e])]) 
           -> Signal a -> (Signal b, Signal c, Signal d, Signal e)
actor14SDF c (p1, p2, p3, p4) f xs = unzip4SDF (p1, p2, p3, p4) $ mapSDF c 1 f xs  

-- | The process constructor 'actor24SDF' constructs an SDF actor with
-- two input and four output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor24SDF :: (Int, Int) -> (Int, Int, Int, Int) 
       -> ([a] -> [b] -> [([c], [d], [e], [f])]) 
       -> Signal a -> Signal b 
       -> (Signal c, Signal d, Signal e, Signal f)
actor24SDF (c1, c2) (p1, p2, p3, p4) f xs ys
  = unzip4SDF (p1, p2, p3, p4) $ zipWithSDF (c1, c2) 1 f xs ys

-- | The process constructor 'actor34SDF' constructs an SDF actor with
-- three input and four output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor34SDF :: (Int, Int, Int) -> (Int, Int, Int, Int) 
           -> ([a] -> [b] -> [c] -> [([d], [e], [f], [g])]) 
           -> Signal a -> Signal b -> Signal c
           -> (Signal d, Signal e, Signal f, Signal g)
actor34SDF (c1, c2, c3) (p1, p2, p3, p4) f as bs cs 
  = unzip4SDF (p1, p2, p3, p4) $ zipWith3SDF (c1, c2, c3) 1 f as bs cs

-- | The process constructor 'actor14SDF' constructs an SDF actor with
-- four input and four output signals. For each input or output signal,
-- the process constructor takes the number of consumed and produced
-- tokens and the function of the actor as arguments.
actor44SDF :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) 
           -> ([a] -> [b] -> [c] -> [d] -> [([e], [f], [g], [h])]) 
           -> Signal a -> Signal b -> Signal c -> Signal d 
           -> (Signal e, Signal f, Signal g, Signal h)
actor44SDF (c1, c2, c3, c4) (p1, p2, p3, p4) f as bs cs ds 
  = unzip4SDF (p1, p2, p3, p4)$ zipWith4SDF (c1, c2, c3, c4) 1 f as bs cs ds

------------------------------------------------------------------------
-- unzipSDF Processes
------------------------------------------------------------------------

unzipSDF :: (Int, Int) -> Signal ([a], [b]) 
         -> (Signal a, Signal b)
unzipSDF (p1, p2) xs = (s1, s2) 
  where
    s1 = signal $ f1 xs
    s2 = signal $ f2 xs
    f1 NullS     = []
    f1 ((as, _):-xs)
      = if length as == p1 then 
          as ++ f1 xs
        else  
          error "unzipSDF: Process does not produce correct number of tokens"
    f2 NullS     = []
    f2 ((_, bs):-xs)
      = if length bs == p2 then 
          bs ++ f2 xs
        else  
          error "unzipSDF: Process does not produce correct number of tokens"  


unzip3SDF :: (Int, Int, Int) -> Signal ([a], [b], [c]) 
      -> (Signal a, Signal b, Signal c)
unzip3SDF (p1, p2, p3) xs = (s1, s2, s3) 
  where
    s1 = signal $ f1 xs
    s2 = signal $ f2 xs
    s3 = signal $ f3 xs
    f1 NullS      = []
    f1 ((as, _, _):-xs) 
      = if length as == p1 then
          as ++ f1 xs
        else  
          error "unzip3SDF: Process does not produce correct number of tokens"
    f2 NullS      = []
    f2 ((_, bs, _):-xs) 
      = if length bs == p2 then 
          bs ++ f2 xs
        else  
          error "unzip3SDF: Process does not produce correct number of tokens"  
    f3 NullS      = []
    f3 ((_, _, cs):-xs) 
      = if length cs == p3 then 
          cs ++ f3 xs
        else  
          error "unzip3SDF: Process does not produce correct number of tokens"  
  

unzip4SDF :: (Int, Int, Int, Int) -> Signal ([a], [b], [c], [d]) 
          -> (Signal a, Signal b, Signal c, Signal d)
unzip4SDF (p1, p2, p3, p4) xs = (s1, s2, s3, s4) 
  where
    s1 = signal $ f1 xs
    s2 = signal $ f2 xs
    s3 = signal $ f3 xs
    s4 = signal $ f4 xs
    f1 NullS      = []
    f1 ((as, _, _, _):-xs) 
      = if length as == p1 then
          as ++ f1 xs
        else  
          error "unzip4SDF: Process does not produce correct number of tokens"
    f2 NullS      = []
    f2 ((_, bs, _, _):-xs) 
      = if length bs == p2 then 
          bs ++ f2 xs
        else  
          error "unzip4SDF: Process does not produce correct number of tokens"  
    f3 NullS      = []
    f3 ((_, _, cs, _):-xs) 
      = if length cs == p3 then 
          cs ++ f3 xs
        else  
          error "unzip4SDF: Process does not produce correct number of tokens" 
    f4 NullS      = []
    f4 ((_, _, _, ds):-xs) 
      = if length ds == p4 then 
          ds ++ f4 xs
        else  
          error "unzip4SDF: Process does not produce correct number of tokens" 

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
s1 = takeS 10 $ signal [1..]
s2 = takeS 10 $ signal [10,20..]

f1 [x] = [([x,x], [x,x,x])]

s3 = unzipSDF (2,3) $ mapSDF 1 1 f1 s1    
         
s4 = actor12SDF 1 (2,3) f1 s1

s5 = signal [1.0,2.0,3.0,4.0,5.0]

multiply [x1,x2] [y] = [(x1+x2)* y]
multiply _   _   = error "Single list item expected"

feedback input = (i1,output) 
   where output = actor21SDF (2,1) 1 multiply input i1
     i1 = delaySDF 1 output
-}
