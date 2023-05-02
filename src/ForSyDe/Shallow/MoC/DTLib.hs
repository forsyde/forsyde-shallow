-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.DTLib
-- Copyright   :  (c) Ingo Sander, KTH ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- DTLib, yet to be completed.
-- 
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.DTLib (
  DTSignal,
  -- * Combinational Process Constructors
  -- | Combinational process constructors are used for processes that
  --   do not have a state.
  mapDT, zipWithDT, scanldDT,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  --   have a state. One of the input parameters is the initial state.
  delayDT
  -- * Processes
  -- | Processes to unzip a signal of tuples into a tuple of signals
  --unzipDT, unzip3DT, unzip4DT,
  ) where  

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.MoC.Synchronous( mapSY, zipWithSY, zipSY)

------------------------------------------------------------------------
--
-- TYPE DEFINITIONS
--
------------------------------------------------------------------------

type DiscreteTime = Integer
type DTSignal a = Signal (DiscreteTime, AbstExt a)

------------------------------------------------------------------------
--                                    
-- COMBINATIONAL PROCESS CONSTRUCTORS 
--
------------------------------------------------------------------------

-- | The process constructor 'mapDT' takes a function and a discrete time signal
--   as arguments. It returns a  discrete time signal, where the function has been
--   applied to all input values. The output signal has the same time tags as the
--   input signal.
--
-- λ> mapDT (liftDT (+1)) $ signal [(2, Prst 1), (5, Prst 7)]
-- {(0,_),(2,2),(5,8)}
mapDT :: (AbstExt a -> AbstExt b) -> DTSignal a -> DTSignal b
mapDT f xs = zipSY timeTags newValues 
   where newValues = mapSY f xValues
         xValues   = getValues timeTags xs
         timeTags  = getTimeTags xs

zipWithDT :: (AbstExt a -> AbstExt b -> AbstExt c)
          -> DTSignal a -> DTSignal b -> DTSignal c
zipWithDT f xs ys = out 
   where out = zipSY newTimeTags outValues
         newTimeTags = timeTags         
         outValues = zipWithSY f xValues yValues
         xValues = getValues timeTags xs
         yValues = getValues timeTags ys
         timeTags = getTimeTags2 xs ys

------------------------------------------------------------------------
--                                 
-- SEQUENTIAL PROCESS CONSTRUCTORS 
--                                
------------------------------------------------------------------------

-- | The process constructor 'mapDT' takes a function and a discrete time signal
--   as arguments, a discrete time delay, an initial value as arguments,
--   and returns the delayed discrete time signal.
--   The resulting signal will have the intial value for the given
--   delay time.
--
-- λ> delayDT 4 Abst $ signal [(2, Prst 1), (5, Prst 7)]
-- {(0,_),(6,1),(9,7)}
delayDT :: DiscreteTime -> AbstExt a -> DTSignal a -> DTSignal a
delayDT delay value xs = (0, value) :- zipSY newTags newValues
  where newTags = mapSY ((+ delay) . fst) xs
        newValues = mapSY snd xs

scanldDT :: DiscreteTime
         -> (AbstExt a -> AbstExt b -> AbstExt b)
         -> AbstExt b
         -> DTSignal a
         -> DTSignal b
scanldDT delay f init input = output
  where output = delayDT delay init internal
        internal = zipWithDT f input output

------------------------------------------------------------------------
--
-- SUPPORT FUNCTIONS - not exported
--
------------------------------------------------------------------------

getTag :: (DiscreteTime, a) -> DiscreteTime
getTag (tag, _) = tag

getValue :: (DiscreteTime, a) -> a
getValue (_, value) = value

getTimeTags :: DTSignal a -> Signal DiscreteTime
getTimeTags NullS   = NullS
getTimeTags (x:-xs) = if fst x /= 0 then
                        0 :- fst x :- mapSY fst xs
                      else
                        0 :- mapSY fst xs

getValues :: Signal DiscreteTime -> Signal (DiscreteTime, AbstExt a) -> Signal (AbstExt a)
getValues = getValues' Abst

getValues' :: a -> Signal DiscreteTime -> Signal (DiscreteTime, a) -> Signal a
getValues' init (t:-ts) (x:-xs) 
   | t  < getTag x = init :- getValues' init ts (x:-xs)
   | t == getTag x = getValue x :- getValues' (getValue x) ts xs
   | otherwise     = error "getValues: Timetag larger than signaltag"
getValues' init (t:-ts) NullS 
                   = init :- getValues' init ts NullS
getValues' init NullS   NullS 
                   = NullS


getTimeTags2 :: DTSignal a -> DTSignal b -> Signal DiscreteTime
getTimeTags2 NullS   NullS   = NullS
getTimeTags2 NullS   ys      = getTimeTags2' NullS ys
getTimeTags2 xs       NullS  = getTimeTags2' xs NullS
getTimeTags2 (x:-xs) (y:-ys)
  | fst x == 0 || fst y == 0 = getTimeTags2' (x:-xs) (y:-ys)
  | otherwise                = 0 :- getTimeTags2' (x:-xs) (y:-ys)
                                     
getTimeTags2' :: DTSignal a
              -> DTSignal b -> Signal DiscreteTime
getTimeTags2' (x:-xs) (y:-ys)
  | getTag x < getTag y  = getTag x :- getTimeTags2' xs (y:-ys)
  | getTag x == getTag y = getTag x :- getTimeTags2' xs ys 
  | otherwise            = getTag y :- getTimeTags2' (x:-xs) ys
getTimeTags2' NullS   ys      = mapSY getTag ys
getTimeTags2' xs      NullS   = mapSY getTag xs

liftDT :: (t -> a) -> AbstExt t -> AbstExt a
liftDT f Abst    = Abst
liftDT f (Prst x) = Prst (f x)

lift2DT :: (a -> b -> c) -> AbstExt a -> AbstExt b -> AbstExt c
lift2DT f Abst     _        = Abst
lift2DT f _        Abst     = Abst
lift2DT f (Prst x) (Prst y) = Prst (f x y)

------------------------------------------------------------------------
--
-- TEST SIGNALS - not exported
--
------------------------------------------------------------------------

s1 :: DTSignal Integer
s1 = signal [(2, Prst 1), (4, Prst 3)]

s2 :: DTSignal Integer
s2 = mapDT (liftDT (+1)) s1

s3 :: DTSignal Integer
s3 = signal [(1000, Prst 10)]

s4 :: DTSignal Integer
s4 = signal [(0, Prst 1)]

s5 :: DTSignal Integer
s5 = scanldDT 1 (lift2DT (+)) (Prst 0) s4
