-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.DE
-- Copyright   :  (c) Ingo Sander, KTH ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Discrete event library, yet to be completed.
-- 
-----------------------------------------------------------------------------

-- ===== TODO =====
-- (1) Event: Implement an instance of read
-- (2) Event: Should Eq and Ord be defined for an event?
--            (==) can have two meanings, if Eq and Ord are defined.
-- (3) zipWithDE: should Float and Real be avoided for tags, since they
--                do not always correctly implement the order relation?
module ForSyDe.Shallow.MoC.DE (
  -- * Data Types
  Event (E), tag, value,
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  --   do not have a state.
  mapDE, zipWithDE,
  -- ** Sequential process constructors
  -- | Sequential process constructors are used for processes that
  --   have a state. One of the input parameters is the initial state.
  delayDE, scanlDE, scanldDE, mooreDE, mealyDE,
  -- * Support Functions
  mapTag, mapValue, getTags, getValues, showUntil, showInterval,
  -- * Test Signals and Processes
  s_1, s_2, s_3, s_inf, fsm
  ) where

import ForSyDe.Shallow.Core
    ( fromSignal, infiniteS, signal, Signal(..), takeS )

------------------------------------------------------------------------
--
-- TYPE DEFINITIONS
--
------------------------------------------------------------------------

-- | An 'Event' has a tag t and a value v. 
--   In the DE MoC all tags shall be comparable.
data Event t v = E t v

instance (Show t, Show v) => Show (Event t v) where
  showsPrec _ = showEvent

showEvent :: (Show t, Show v) => Event t v -> String -> String
showEvent (E t v) = (++) (show v ++ "@" ++ show t)

tag :: Event t v -> t
tag (E t _) = t


value :: Event t v -> v
value (E _ v) = v

------------------------------------------------------------------------
--
-- COMBINATIONAL PROCESS CONSTRUCTORS
--
-------------------------------------------------------------------------- 

-- | The process constructor 'mapDE' takes a combinational function f as
-- argument and returns a process with one input signal and one output
-- signal. The function f is applied on all values of the input signal.
--
-- >>> mapDE (+1) $ signal [E 0 0, E 1 1, E 2 2]
-- {1@0,2@1,3@2}

mapDE :: (Eq t, Num t) => (a -> b) -> Signal (Event t a) -> Signal (Event t b)
mapDE _ NullS = NullS
mapDE f (E t v :- es) 
  | t == 0    = mapValue f (E t v :- es)
  | otherwise = error "mapDE: Signals need to start at time 0" 

-- | The process constructor 'zipWithDE' takes a combinational function f as
-- argument and returns a process with two input signals and one output
-- signal. The function f is applied pairwise on all values of the two input signals.
--
-- >>> zipWithDE (+) (signal [E 0 0, E 2 2]) (signal [E 0 10, E 10 10])
-- {10@0,12@2,12@10}
zipWithDE :: (Ord t, Num t) =>
             (a -> b -> c)
             -> Signal (Event t a)
             -> Signal (Event t b)
             -> Signal (Event t c)
zipWithDE _ NullS _      = NullS
zipWithDE _ (_:-_) NullS = NullS
zipWithDE f (E ta va :- as) (E tb vb :- bs)
   | ta == 0 && tb == 0 = E 0 (f va vb) :- zipWithDE' f va vb as bs
   | otherwise          = error "zipWithDE: Signals need to start at time 0" 

zipWithDE' :: (Ord t, Num t) => (a -> b -> c) -> a -> b -> Signal (Event t a) -> Signal (Event t b) -> Signal (Event t c)
zipWithDE' _ _ _ NullS NullS = NullS
zipWithDE' f _ oldb (E ta va :- as) NullS 
   = E ta (f va oldb) :- zipWithDE' f va oldb as NullS
zipWithDE' f olda _ NullS (E tb vb :- bs) 
   = E tb (f olda vb) :- zipWithDE' f olda vb NullS bs   
zipWithDE' f olda oldb (E ta va :- as) (E tb vb :- bs)
   | ta < tb   = E ta (f va oldb) :- zipWithDE' f va oldb as (E tb vb :- bs)
   | ta == tb  = E ta (f va vb) :- zipWithDE' f va vb as bs
   | otherwise = E tb (f olda vb) :- zipWithDE' f olda vb (E ta va :- as) bs

-- | The process constructor 'zipWith3DE' takes a combinational function f as
-- argument and returns a process with three input signals and one output
-- signal. The function f is applied to all current values of the input signals.
--
-- >>> zipWith3DE (+) (signal [E 0 0, E 2 2]) (signal [E 0 10, E 10 10]) (signal [E 0 10, E 7 100])
-- {1@0,2@1,3@2}
zipWith3DE :: (Ord t, Num t) =>
             (a -> b -> c -> d)
             -> Signal (Event t a)
             -> Signal (Event t b)
             -> Signal (Event t c)
             -> Signal (Event t d)
zipWith3DE = undefined

------------------------------------------------------------------------
--
-- SEQUENTIAL PROCESS CONSTRUCTORS
--
-------------------------------------------------------------------------- 

-- | The process constructor 'delayDE' delays the input signal a time t
-- and introduces an initial value v0 at time 0 in the
-- output signal.  Note, that this implies that there is one event
-- (the first) at the output signal that has no corresponding event at
-- the input signal. This is
-- necessary to initialize feed-back loops.
--
-- >>> delayDE 2 10 $ signal [E 0 0, E 2 2]
-- {10@0,0@2,2@4}

delayDE :: (Num t)
        => t -- ^Propagation delay
        -> v -- ^Intitial state
        -> Signal (Event t v) -- ^Input signal
        -> Signal (Event t v) -- ^Output signal 
delayDE t v0 es = E 0 v0 :- mapTag (+t) es


  
-- | The process constructor 'scanlDE' is used to construct a finite
-- state machine process without output decoder.  It takes a propagation
-- delay t, a function f for the next state, and an initial value v0.
-- The process constructor behaves similar to the Haskell prelude function
-- 'scanl' and has the value of the new state as its output value as
-- illustrated by the following example.
--
-- This is in contrast to the function 'scanldDE', which has its
-- {1@0,3@2,5@4}



scanlDE :: (Num t, Ord t)
        => t                  -- ^Propagation delay
        -> (a -> b -> a)      -- ^Function for next state
        -> a                  -- ^Initial state
        -> Signal (Event t b) -- ^Input signal 
        -> Signal (Event t a) -- ^Output signal
scanlDE t f v0 xs = s'
  where s' = zipWithDE f (delayDE t v0 s') xs 

-- | The process constructor 'scanldDE' is used to construct a finite
-- state machine process without output decoder. It takes a delay t,
-- a function f for the next state decoder, and an initial value v0.
-- In contrast to the process constructor 'scanlDE' here
-- the output value is the current state and not the one of the next
-- state.
--
-- >>> takeS 3 $ scanldDE 2 (+) 0 (signal  [E 0 1, E 2 2])
-- {0@0,1@2,3@4}

scanldDE :: (Num t, Ord t)
         => t -- ^Propagation Delay
         -> (a -> b -> a)    -- ^Function for next state
         -> a                  -- ^Initial state
         -> Signal (Event t b) -- ^Input signal 
         -> Signal (Event t a) -- ^Output signal
scanldDE t f v0 xs = s'
  where s' = delayDE t v0 $ zipWithDE f s' xs

-- | The process constructor 'mooreDE' is used to model state machines
-- of \"Moore\" type, where the output only depends on the current
-- state. The process constructor is based on the process constructor
-- 'scanldDE', since it is natural for state machines in hardware,
-- that the output operates on the current state and not on the next
-- state. The process constructors takes a delay t, a function to
-- calculate the next state, another function to calculate the output, 
-- and a value v0 for the initial state.
--
-- In contrast, the output of a process created by the process constructor 'mealyDE'
-- depends not only on the state, but also on the input values.
--
-- >>> takeS 3 $ mooreDE 2 (+) (*2) 0 $ signal  [E 0 1, E 2 2]
-- {0@0,2@2,6@4}
mooreDE ::  (Num t, Ord t)
        => t             -- ^Delay
        -> (a -> b -> a) -- ^Function for next state
        -> (a -> c)      -- ^Function for output
        -> a             -- ^Initial state
        -> Signal (Event t b) -- ^Input signal
        -> Signal (Event t c) -- ^Output signal
mooreDE t nextState output v0 
    = mapDE output . scanldDE t nextState v0

-- | The process constructor 'melayDE' is used to model state machines
-- of \"Mealy\" type, where the output only depends on the current
-- state and the input values. The process constructor is based on the
-- process constructor 'scanldDE', since it is natural for state
-- machines in hardware, that the output operates on the current state
-- and not on the next state.  The process constructors takes a delay t,
-- a function to calculate the next state, another function to calculate
-- the output, and a value v0 for the initial state.
--
-- In contrast, the output of a process created by the process
-- constructor 'mooreDE' depends only on the state, but not on the
-- input values.
--
-- >>> takeS 3 $ mealyDE 2 (+) (+) 0 $ signal [E 0 1, E 2 2]
-- {1@0,3@2,5@4}
mealyDE :: (Num t, Ord t)
        => t             -- ^Delay
        -> (a -> b -> a) -- ^Function for next state
        -> (a -> b-> c)  -- ^Function for output
        -> a             -- ^Initial state
        -> Signal (Event t b)      -- ^Input signal
        -> Signal (Event t c)      -- ^Output signal
mealyDE t nextState output  v0 sig
  = zipWithDE output state sig where
    state = scanldDE t nextState v0 sig

-- Support functions

-- Functions on events

-- | The function 'getTag' returns the tag of an event
getTag :: Event t v -> t
getTag (E t _) = t

-- | The function 'getValue' returns the value of an event
getValue :: Event t v -> v
getValue (E _ v) = v 

-- Functions on DE Signals

-- | The function 'mapTag' applies a function to all tags in a DE-signal   
mapTag :: (t -> t) -> Signal (Event t v) -> Signal (Event t v)
mapTag _ NullS         = NullS
mapTag f ((E t v):-es) = E (f t) v :- mapTag f es

-- | The function 'mapValue' applies a function to all values in a DE-signal
mapValue :: (a -> b) -> Signal (Event t a) -> Signal (Event t b)
mapValue _ NullS       = NullS 
mapValue f (E t v:-es) = E t (f v) :- mapValue f es

-- | The function 'getTags' returns all tags in a signal
getTags :: Signal (Event t v) -> [t]
getTags = map getTag . fromSignal

-- | The function 'getValues' returns all values in a signal
getValues :: Signal (Event t v) -> [v]
getValues = map getValue . fromSignal

-- | The function 'showUntil' returns the part of the signal until a certain time tag
showUntil :: (Ord t) => t -> Signal (Event t v) -> Signal (Event t v) 
showUntil _ NullS = NullS 
showUntil tmax (E t v :- es)  
   | t <= tmax = E t v :- showUntil tmax es
   | otherwise = NullS 

-- | The function 'ShowInterval' returns the part of signal within an interval defined by two time tags.
--   The given time tags are part of the interval.
showInterval :: (Ord t) => t -> t -> Signal (Event t v) -> Signal (Event t v) 
showInterval _ _ NullS = NullS 
showInterval tmin tmax (E t v :- es)  
   | t < tmin               = showInterval tmin tmax es
   | t >= tmin && t <= tmax = E t v :- showInterval tmin tmax es
   | otherwise              = NullS 

-- Tests

-- Test Signals
s_1 :: Signal (Event Int Int)
s_1 = signal [E 0 1, E 1 2]

s_2 :: Signal (Event Int Int)
s_2 = signal [E 0 1, E 10 10]

s_3 :: Signal (Event Int Int)
s_3 = signal [E 0 0]

s_inf :: Signal (Event Int Int)
s_inf = infiniteS incTag (E 0 0) where
  incTag (E t v) = E (t+1) v

-- Test Circuits
fsm :: Signal (Event Int Int) -> Signal (Event Int Int)
fsm s_in = s_out
   where s_out = delayDE 2 0 s_1
         s_1   = zipWithDE (+) s_in s_out
