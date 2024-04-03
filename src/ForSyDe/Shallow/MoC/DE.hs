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
-- (3) zipWithDE: non-exhaustive pattern matching rules

module ForSyDe.Shallow.MoC.DE (
  -- * Data Types
  Event (E),
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  --   do not have a state.
  mapDE, zipWithDE,
  -- * Support Functions
  mapTag, mapValue, getTags, getValues, showUntil, showInterval,
  -- * Test Signals
  s_1, s_2, s_3, s_inf
  ) where

import ForSyDe.Shallow.Core

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

-- Combinational process constructors

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
-- {1@0,2@1,3@2}
zipWithDE :: (Ord t, Num t) => (a -> b -> c)
             -> Signal (Event t a) -> Signal (Event t b) -> Signal (Event t c)
zipWithDE _ NullS _      = NullS
zipWithDE _ (_:-_) NullS = NullS
zipWithDE f (E ta va :- as) (E tb vb :- bs)
   | ta == 0 && tb == 0 = E 0 (f va vb) :- zipWithDE' f va vb as bs
   | otherwise          = error "mapDT: Signals need to start at time 0" 

zipWithDE' :: (Ord t, Num t) => (a -> b -> c) -> a -> b -> Signal (Event t a) -> Signal (Event t b) -> Signal (Event t c)
zipWithDE' _ _ _ NullS NullS = NullS
zipWithDE' f _ oldb (E ta va :- as) NullS 
   = E ta (f va oldb) :- zipWithDE' f va oldb as NullS
zipWithDE' f olda _ NullS (E tb vb :- bs) 
   = E tb (f olda vb) :- zipWithDE' f olda vb NullS bs   
zipWithDE' f olda oldb (E ta va :- as) (E tb vb :- bs)
   | ta < tb  = E ta (f va oldb) :- zipWithDE' f va oldb as (E tb vb :- bs)
   | ta == tb = E ta (f va vb) :- zipWithDE' f va vb as bs
   | ta > tb  = E tb (f olda vb) :- zipWithDE' f olda vb (E ta va :- as) bs 

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
