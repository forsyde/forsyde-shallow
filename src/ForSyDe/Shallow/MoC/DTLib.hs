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
  mapDT, zipWithDT, zipWith3DT,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  --   have a state. One of the input parameters is the initial state.
  delayDT, scanldDT, scanld2DT, mooreDT, moore2DT, mealyDT, mealy2DT,
  -- * Processes
  -- | Processes to unzip a signal of tuples into a tuple of signals
  zipDT, zip3DT, unzipDT, unzip3DT
  ) where  

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.MoC.Synchronous( mapSY, zipWithSY, zipWith3SY, zipSY)


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


zipWith3DT :: (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d)
           -> DTSignal a -> DTSignal b -> DTSignal c
           -> DTSignal d 
zipWith3DT f xs ys zs = out 
   where out = zipSY newTimeTags outValues
         newTimeTags = timeTags         
         outValues = zipWith3SY f xValues yValues zValues
         xValues = getValues timeTags xs
         yValues = getValues timeTags ys
         zValues = getValues timeTags zs
         timeTags = getTimeTags3 xs ys zs


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

scanldDT :: DiscreteTime -- * Delay in time units
         -> (AbstExt a -> AbstExt b -> AbstExt b) -- * Function on input and next state
         -> AbstExt b -- * Initial State
         -> DTSignal a -- * Input Signal
         -> DTSignal b -- * Output Signal
scanldDT delay f init input = state
  where state = delayDT delay init nextstate
        nextstate = zipWithDT f input state

scanld2DT :: DiscreteTime
          -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt c)
          -> AbstExt c
          -> DTSignal a
          -> DTSignal b
          -> DTSignal c
scanld2DT delay f init xs ys = state
  where state = delayDT delay init nextstate
        nextstate = zipWith3DT f xs ys state

mooreDT :: DiscreteTime
        -> (AbstExt a -> AbstExt b -> AbstExt b)
        -> (AbstExt b -> AbstExt c)
        -> AbstExt b
        -> DTSignal a
        -> DTSignal c
mooreDT delay f g init input = output
  where state = delayDT delay init nextstate
        nextstate = zipWithDT f input state
        output = mapDT g state

moore2DT :: DiscreteTime
         -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt c)
         -> (AbstExt c -> AbstExt d)
         -> AbstExt c
         -> DTSignal a
         -> DTSignal b
         -> DTSignal d
moore2DT delay f g init xs ys = output
  where state = delayDT delay init nextstate
        nextstate = zipWith3DT f xs ys state
        output = mapDT g state

        
mealyDT :: DiscreteTime
        -> (AbstExt a -> AbstExt b -> AbstExt b)
        -> (AbstExt a -> AbstExt b -> AbstExt c)
        -> AbstExt b
        -> DTSignal a
        -> DTSignal c
mealyDT delay f g init input = output
  where state = delayDT delay init nextstate
        nextstate = zipWithDT f input state
        output = zipWithDT g input state

mealy2DT :: DiscreteTime
        -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt c)
        -> (AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d)
        -> AbstExt c
        -> DTSignal a
        -> DTSignal b
        -> DTSignal d
mealy2DT delay f g init xs ys = output
  where state = delayDT delay init nextstate
        nextstate = zipWith3DT f xs ys state
        output = zipWith3DT g xs ys state

------------------------------------------------------------------------
--
-- ZIP, UNZIP PROCESSES
--
------------------------------------------------------------------------

zipDT :: DTSignal a -> DTSignal b -> Signal (DiscreteTime, (AbstExt a, AbstExt b))
zipDT xs ys = out 
   where out = zipSY newTimeTags outValues
         newTimeTags = timeTags         
         outValues = zipWithSY (,) xValues yValues
         xValues = getValues timeTags xs
         yValues = getValues timeTags ys
         timeTags = getTimeTags2 xs ys
         
zip3DT :: DTSignal a -> DTSignal b -> DTSignal c
       -> DTSignal (a, b, c)
zip3DT = undefined

unzipDT :: DTSignal (a, b) -> (DTSignal a, DTSignal b)
unzipDT = undefined

unzip3DT :: DTSignal (a, b, c) -> (DTSignal a, DTSignal b, DTSignal c)
unzip3DT = undefined

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
getTimeTags2 (x:-xs) NullS
  | fst x == 0 = getTimeTags2' (x:-xs) NullS
  | otherwise  = 0 :- getTimeTags2' (x:-xs) NullS
getTimeTags2 NullS   (y:-ys)
  | fst y == 0 = getTimeTags2' NullS (y:-ys)
  | otherwise  = 0 :- getTimeTags2' NullS (y:-ys)
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

getTimeTags3 :: DTSignal a -> DTSignal b -> DTSignal c
             -> Signal DiscreteTime
getTimeTags3 NullS   NullS   NullS = NullS
getTimeTags3 (x:-xs) NullS   NullS
  | fst x == 0 = getTimeTags3' (x:-xs) NullS NullS
  | otherwise  = 0 :- getTimeTags3' (x:-xs) NullS NullS
getTimeTags3 NullS   (y:-ys) NullS
  | fst y == 0 = getTimeTags3' NullS (y:-ys) NullS
  | otherwise  = 0 :- getTimeTags3' NullS (y:-ys) NullS  
getTimeTags3 NullS   NullS   (z:-zs)
  | fst z == 0 = getTimeTags3' NullS NullS (z:-zs)
  | otherwise  = 0 :- getTimeTags3' NullS NullS (z:-zs)
getTimeTags3 (x:-xs) (y:-ys) (z:-zs)
  | fst x == 0 || fst y == 0 || fst z == 0 = getTimeTags3' (x:-xs) (y:-ys) (z:-zs)
  | otherwise                              = 0 :- getTimeTags3' (x:-xs) (y:-ys) (z:-zs)

getTimeTags3' :: DTSignal a -> DTSignal b -> DTSignal c
              -> Signal DiscreteTime
getTimeTags3' = undefined
{-((xt,xv):-xs) ((yt,yv):-ys) ((zt,zv):-zs)
  | minimum [xt,yt,zt] == xt
    | xt == yt && 
getTimeTags3' xs      NullS  NullS  = mapSY getTag xs
getTimeTags3' NullS   ys     NullS = mapSY getTag ys
-}

  
{- IDEA DOES NOT WORK due to need of access elements in advance!
getTimeTags :: DTSignal a -> Signal DiscreteTime
getTimeTags xs = (signal . nub) xs'
  where xs' = 0 : (fromSignal . mapSY getTag) xs

getTimeTags2 :: DTSignal a -> DTSignal b -> Signal DiscreteTime
getTimeTags2 xs ys = (signal . nub . sort) (xs' ++ ys')
   where xs' = 0 : (fromSignal . mapSY getTag) xs
         ys' = (fromSignal . mapSY getTag) ys

getTimeTags3 ::  DTSignal a -> DTSignal b -> DTSignal c
             -> Signal DiscreteTime
getTimeTags3 xs ys zs = (signal . nub . sort . concat) [xs', ys', zs']
   where xs' = 0 : (fromSignal . mapSY getTag) xs
         ys' = (fromSignal . mapSY getTag) ys
         zs' = (fromSignal . mapSY getTag) zs
-}

liftDT :: (t -> a) -> AbstExt t -> AbstExt a
liftDT f Abst    = Abst
liftDT f (Prst x) = Prst (f x)

lift2DT :: (a -> b -> c)
        -> AbstExt a -> AbstExt b -> AbstExt c
lift2DT f Abst     _        = Abst
lift2DT f _        Abst     = Abst
lift2DT f (Prst x) (Prst y) = Prst (f x y)

lift3DT :: (a -> b -> c -> d)
        -> AbstExt a -> AbstExt b -> AbstExt c -> AbstExt d
lift3DT f Abst     _        _        = Abst
lift3DT f _        Abst     _        = Abst
lift3DT f _        _        Abst     = Abst
lift3DT f (Prst x) (Prst y) (Prst z) = Prst (f x y z)

------------------------------------------------------------------------
--
-- TEST SIGNALS - not exported
--
------------------------------------------------------------------------

s1 :: DTSignal Integer
s1 = signal [(2, Prst 1), (4, Prst 3)]

s2 = signal [(1, Prst 1)]

zipWithDT :: DTSignal Int -> DTSignal Int -> DTSignal Int

s2 :: DTSignal Integer
s2 = mapDT (liftDT (+1)) s1

s3 :: DTSignal Integer
s3 = signal [(1000, Prst 10)]

s4 :: DTSignal Integer
s4 = signal [(0, Prst 1)]

s5 :: DTSignal Integer
s5 = takeS 10 $ scanldDT 1 (lift2DT (+)) (Prst 0) s4

s6 :: DTSignal Integer
s6 = mapDT (liftDT (+1)) s1

s7 :: DTSignal Integer
s7 = zipWithDT (lift2DT (+)) s1 s3

s8 :: DTSignal Integer
s8 = zipWith3DT (lift3DT add3) s1 s3 s4
  where add3 x y z = x + y + z

s9 :: DTSignal Integer
s9 = delayDT 3 Abst s1

s10 :: DTSignal Integer
s10 = scanld2DT 2 (lift3DT add3) Abst s1 s3
  where add3 x y z = x + y + z
