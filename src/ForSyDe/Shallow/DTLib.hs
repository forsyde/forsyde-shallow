-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.DTLib
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

module DTLib where

import ForSyDe.Shallow.CoreLib
       
data DE_Data a = U
               | D a
                 deriving (Eq, Show)
                          
type Time = Integer

liftDE f U     = U
liftDE f (D x) = D (f x)

liftDE2 f U _         = U
liftDE2 f _ U         = U
liftDE2 f (D x) (D y) = D (f x y)

delayDT :: Time -> DE_Data a -> Signal (Time, DE_Data a) -> Signal (Time, DE_Data a)
delayDT delay init xs = (0,init) :- zipSY newTags newValues 
   where newTags = mapSY ((+ delay) . fst) xs
         newValues = mapSY snd xs

mapDT :: (DE_Data a -> DE_Data b) -> Signal (Time, DE_Data a) -> Signal (Time, DE_Data b)
mapDT f xs = zipSY newTags newValues 
   where newTags = mapSY fst xs
         newValues = mapSY f xValues
         xValues = getValues timeTags xs
         timeTags = getTimeTags xs         
         
zipWithDT :: (DE_Data a -> DE_Data b -> DE_Data c) -> Signal (Time, DE_Data a) -> Signal (Time, DE_Data b) -> Signal (Time, DE_Data c)
zipWithDT f xs ys = out 
   where out = zipSY newTimeTags outValues
         newTimeTags = timeTags         
         outValues = zipWithSY f xValues yValues
         xValues = getValues timeTags xs
         yValues = getValues timeTags ys
         timeTags = getTimeTags2 xs ys


-- ??? Infinite Loop... Why??? -- Seems to work now! Check!
scanldDT :: Time -> (DE_Data a -> DE_Data b -> DE_Data b)
     -> DE_Data b
     -> Signal (Time, DE_Data a)
     -> Signal (Time, DE_Data b)
scanldDT delay f init input = output
  where output = delayDT delay init internal
        internal = zipWithDT f input output
        
runningSum input = output
   where internal = zipWithDT (liftDE2 (+)) input output
         output = delayDT 2 (D 0) internal


-- Helper Functions
         
getTimeTags :: Signal (Time, a) -> Signal Time
getTimeTags (x:-xs) = if fst x == 0 then
                        0 :- mapSY fst xs
                      else
                        mapSY fst xs

getTimeTags2 :: Signal (Time, a) -> Signal (Time, b) -> Signal Time
getTimeTags2 (x:-xs) (y:-ys) = if fst x == 0 || fst y == 0 then
                                     getTimeTags2' (x:-xs) (y:-ys)
                                   else
                                     0 :- getTimeTags2' (x:-xs) (y:-ys)

getTimeTags2' (x:-xs) (y:-ys) = if getTag x < getTag y then
                                  getTag x :- getTimeTags2' xs (y:-ys)
                               else
                                  if getTag x == getTag y then
                                     getTag x :- getTimeTags2' xs ys 
                                  else
                                     getTag y :- getTimeTags2' (x:-xs) ys
getTimeTags2' NullS   ys      = mapSY getTag ys
getTimeTags2' xs      NullS   = mapSY getTag xs

getTag (tag, value) = tag

getValue (tag, value) = value

getValues ts xs = getValues' U ts xs

getValues' init (t:-ts) (x:-xs) 
   | t  < getTag x = init :- getValues' init ts (x:-xs)
   | t == getTag x = getValue x :- getValues' (getValue x) ts xs
   | otherwise     = error "getValues: Timetag larger than signaltag"
getValues' init (t:-ts) NullS 
                   = init :- getValues' init ts NullS
getValues' init NullS   NullS 
                   = NullS

-- NOT EXPORTED: Processes from Synchronous Library

-- | The process constructor 'mapSY' takes a combinational function as argument and returns a process with one input signal and one output signal.
mapSY		:: (a -> b) -> Signal a	-> Signal b
mapSY _ NullS	= NullS
mapSY f (x:-xs)	= f x :- (mapSY f xs)

-- | The process constructor 'zipWithSY' takes a combinational function as argument and returns a process with two input signals and one output signal.
zipWithSY	:: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWithSY _ NullS   _       = NullS
zipWithSY _ _       NullS   = NullS
zipWithSY f (x:-xs) (y:-ys) = f x y :- (zipWithSY f xs ys)

-- | The process constructor 'zipWith3SY' takes a combinational function as argument and returns a process with three input signals and one output signal.
zipWith3SY	:: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3SY _ NullS   _       _       = NullS
zipWith3SY _ _       NullS   _       = NullS
zipWith3SY _ _       _       NullS   = NullS
zipWith3SY f (x:-xs) (y:-ys) (z:-zs) = f x y z 
				       :- (zipWith3SY f xs ys zs)

-- | The process constructor 'zipWith4SY' takes a combinational function as argument and returns a process with four input signals and one output signal.
zipWith4SY	:: (a -> b -> c -> d -> e) -> Signal a -> Signal b 
		-> Signal c -> Signal d	-> Signal e
zipWith4SY _ NullS   _       _       _      = NullS
zipWith4SY _ _       NullS   _       _      = NullS
zipWith4SY _ _       _       NullS   _      = NullS
zipWith4SY _ _       _       _       NullS  = NullS
zipWith4SY f (w:-ws) (x:-xs) (y:-ys) (z:-zs) 
				      = f w x y z 
				        :- (zipWith4SY f ws xs ys zs)

-- | The process 'zipSY' \"zips\" two incoming signals into one signal of tuples.
zipSY        :: Signal a -> Signal b -> Signal (a,b)
zipSY (x:-xs) (y:-ys) = (x, y) :- zipSY xs ys
zipSY _	     _	     = NullS

-- | The process 'zip3SY' works as 'zipSY', but takes three input signals.
zip3SY       :: Signal a -> Signal b -> Signal c -> Signal (a,b,c)
zip3SY (x:-xs) (y:-ys) (z:-zs) = (x, y, z) :- zip3SY xs ys zs
zip3SY _	     _	_      = NullS

-- | The process 'zip4SY' works as 'zipSY', but takes four input signals.
zip4SY	     :: Signal a -> Signal b -> Signal c -> Signal d 
		-> Signal (a,b,c,d)
zip4SY (w:-ws) (x:-xs) (y:-ys) (z:-zs) = (w, x, y, z) 
					 :- zip4SY ws xs ys zs
zip4SY _	_      _	_      = NullS

-- | The process 'zip5SY' works as 'zipSY', but takes four input signals.
zip5SY	     :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e
		-> Signal (a,b,c,d,e)
zip5SY (x1:-x1s) (x2:-x2s) (x3:-x3s) (x4:-x4s) (x5:-x5s) 
    = (x1,x2,x3,x4,x5) :- zip5SY x1s x2s x3s x4s x5s
zip5SY _	 _         _	     _         _ = NullS

-- | The process 'zip6SY' works as 'zipSY', but takes four input signals.
zip6SY	     :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> Signal f -> Signal (a,b,c,d,e,f)
zip6SY (x1:-x1s) (x2:-x2s) (x3:-x3s) (x4:-x4s) (x5:-x5s)  (x6:-x6s) 
    = (x1,x2,x3,x4,x5,x6) :- zip6SY x1s x2s x3s x4s x5s x6s
zip6SY _	 _         _	     _         _          _ = NullS

-- | The process 'unzipSY' \"unzips\" a signal of tuples into two signals.
unzipSY      :: Signal (a,b) -> (Signal a,Signal b)
unzipSY NullS	     = (NullS, NullS)
unzipSY ((x, y):-xys) = (x:-xs, y:-ys) where (xs, ys) = unzipSY xys

-- | The process 'unzip3SY' works as 'unzipSY', but has three output signals.
unzip3SY     :: Signal (a, b, c) -> (Signal a, Signal b, Signal c)
unzip3SY NullS             = (NullS, NullS, NullS)
unzip3SY ((x, y, z):-xyzs) = (x:-xs, y:-ys, z:-zs) where
  			     (xs, ys, zs) = unzip3SY xyzs

-- | The process 'unzip4SY' works as 'unzipSY', but has four output signals.
unzip4SY     :: Signal (a,b,c,d) 
		-> (Signal a,Signal b,Signal c,Signal d)
unzip4SY NullS              = (NullS, NullS, NullS, NullS)
unzip4SY ((w,x,y,z):-wxyzs) = (w:-ws, x:-xs, y:-ys, z:-zs) where
  			      (ws, xs, ys, zs) = unzip4SY wxyzs

-- | The process 'unzip5SY' works as 'unzipSY', but has four output signals.
unzip5SY     :: Signal (a,b,c,d,e) 
		-> (Signal a,Signal b,Signal c,Signal d,Signal e)
unzip5SY NullS              = (NullS, NullS, NullS, NullS,NullS)
unzip5SY ((x1,x2,x3,x4,x5):-xs) = (x1:-x1s, x2:-x2s, x3:-x3s, x4:-x4s, x5:-x5s)
    where (x1s, x2s, x3s, x4s,x5s) = unzip5SY xs

-- | The process 'unzip6SY' works as 'unzipSY', but has four output signals.
unzip6SY     :: Signal (a,b,c,d,e,f) 
		-> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)
unzip6SY NullS              = (NullS, NullS, NullS, NullS,NullS,NullS)
unzip6SY ((x1,x2,x3,x4,x5,x6):-xs) 
    = (x1:-x1s, x2:-x2s, x3:-x3s, x4:-x4s, x5:-x5s, x6:-x6s)
    where (x1s, x2s, x3s, x4s, x5s, x6s) = unzip6SY xs

s1 :: Signal (Time, DE_Data Integer)
s1 = signal [(0, D 1), (1, D 2), (7, D 3)]
s2 :: Signal (Time, DE_Data Integer)
s2 = signal [(0, D 1), (4, D 2), (5, D 3)]
s3 = signal [(0,D 0), (1,D 1), (2,D 2), (3,D 3)]
s4 = signal [(1, D 1)]


-- Examples

-- 1) Simple process constructors

