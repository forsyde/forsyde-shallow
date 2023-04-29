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
  mapDT, 
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  --   have a state. One of the input parameters is the initial state.
  delayDT
  -- * Processes
  -- | Processes to unzip a signal of tuples into a tuple of signals
  --unzipDT, unzip3DT, unzip4DT,
  ) where  

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.MoC.Synchronous( mapSY, zipSY)

type DiscreteTime = Integer
type DTSignal a = Signal (DiscreteTime, AbstExt a)

-------------------------------------
--                                 --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--                                 --
-------------------------------------

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

----------------------------------------
--                                    --
-- COMBINATIONAL PROCESS CONSTRUCTORS --
--                                    --
----------------------------------------

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
         
-- Support functions, which are not exported by the module

getTag :: (DiscreteTime, a) -> DiscreteTime
getTag (tag, _) = tag

getValue :: (DiscreteTime, a) -> a
getValue (_, value) = value

getTimeTags :: Signal (DiscreteTime, a) -> Signal DiscreteTime
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

liftDT :: (t -> a) -> AbstExt t -> AbstExt a
liftDT f Abst    = Abst
liftDT f (Prst x) = Prst (f x)

-- Test Signals, not exported
s1 :: DTSignal Integer
s1 = signal [(2, Prst 1), (4, Prst 3)]

s2 :: DTSignal Integer
s2 = mapDT (liftDT (+1)) s1
