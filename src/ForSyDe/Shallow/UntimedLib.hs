-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.UntimedLib
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The untimed library defines process constructors and processes for
-- the untimed computational model. A process constructor is a higher
-- order function which together with combinational function(s) and
-- values as arguments constructs a process.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.UntimedLib(  
                    -- * Combinational process constructors
		    -- | Combinational process constructors are used for processes that do not have a state.
                    combU, comb2U, comb2UC,
		    mapU,
           	    -- * Sequential process constructors
		    -- | Sequential process constructors are used for processes that have a state. One of the input parameters is the initial state.
                    scanU, mealyU, mooreU, sourceU, sinkU, initU,
		    -- * Zipping and unzipping signals
		    zipU, zipUs,
		    zipWithU, zipWith3U, zipWith4U,
		    unzipU
                  )
where

import ForSyDe.Shallow.CoreLib

----------------------------------------
--                                    --
-- COMBINATIONAL PROCESS CONSTRUCTORS --
--                                    --
----------------------------------------

combU :: Int -> ([a] -> [b]) -> Signal a -> Signal b
combU = mapU

comb2U :: Int -> Int -> ([a]->[b]->[c]) -> Signal a -> Signal b -> Signal c
comb2U = zipWithU

comb2UC :: Int -> (a->[b]->[c]) -> Signal a -> Signal b -> Signal c
comb2UC = zipWithUC

-- | The first parameter of 'mapU' is a constant integer defining the number of tokens consumed in every evaluation cycle. The second argument is a function on lists of the input type and returning a list of the output type. For instance,
--
-- > r2 = mapU 1 f
-- >   where f :: [Int] -> [Int]
-- >         f [x] = [2*x]
--
-- defines a process r2 which consumes one token in each evaluation cycle and multiplies it by two.
mapU :: Int -> ([a] -> [b]) -> Signal a -> Signal b
mapU _ _ NullS = NullS
mapU c f xs | lengthS (takeS c xs) < c = NullS
            | otherwise
	      = signal (f (takeL c xs)) +-+ (mapU c f (dropS c xs))
	      
--mapUC :: Int -> ([a] -> b) -> Signal a -> Signal b
--mapUC _ _ NullS = NullS
--mapUC c f xs | lengthS (takeS c xs) < c = NullS
--             | otherwise 
--	       =  signal [(f (takeL c xs))] +-+ (mapUC c f (dropS c xs))

---------------------------
--                       --
-- SYNCHRONOUS PROCESSES --
--                       --
---------------------------

-- | 'scanU' has an internal state which is visible at the output. The first argument is a function \'gamma\' which, given the state returns the number of tokens consumed next. The second argument is the next state function and the third is the initial state.
scanU :: (b->Int) -> (b->[a]->b) -> b -> Signal a -> Signal b
scanU _     _ _     NullS = NullS
scanU gamma g state xs 
    | length as == c = newstate :- scanU gamma g newstate (dropS c xs)
    | otherwise       = NullS
    where c        = gamma state
	  as       = takeL c xs
	  newstate = g state as



-- | The process constructor 'mooreU' creates a state machine of Moore type. In addition to the next state function they also have an output encoding function. The output depends directly on the internal state.
mooreU :: (b->Int) -> (b->[a]->b) -> (b -> [c]) -> b -> Signal a -> Signal c
mooreU _ _ _ _ NullS = NullS
mooreU gamma g f state xs
    | length as == c = (signal bs) +-+ mooreU gamma g f newstate (dropS c xs)
    | otherwise      = NullS
    where c        = gamma state
	  as       = takeL c xs
	  newstate = g state as 
	  bs       = f state

-- | The process constructor 'mealyU' creates a state machine of Moore type. In addition to the next state function they also have an output encoding function. The output depends directly on the internal state.
mealyU :: (b->Int) -> (b->[a]->b) -> (b -> [a] -> [c]) -> b 
       -> Signal a -> Signal c
mealyU _ _ _ _ NullS = NullS
mealyU gamma g f state xs
    | length as == c = (signal bs) 
		       +-+ mealyU gamma g f newstate (dropS c xs)
    | otherwise      = NullS
    where c        = gamma state
	  as       = takeL c xs
	  newstate = g state as
	  bs       = f state as


zipU  :: Signal (Int,Int) -> Signal a -> Signal b -> Signal ([a],[b])
zipU NullS _ _ = NullS
zipU _ NullS _ = NullS
zipU _ _ NullS = NullS
zipU ((c1,c2):-cs) xs ys
     | lengthS (takeS c1 xs) == c1 && lengthS (takeS c2 ys) == c2
       = (takeL c1 xs,takeL c2 ys) :- zipU cs (dropS c1 xs) (dropS c2 ys)
     | otherwise = NullS

zipUs :: Int -> Int ->       Signal a -> Signal b -> Signal ([a],[b])
zipUs _ _ NullS _ = NullS 
zipUs _ _ _ NullS = NullS 
zipUs c1 c2 xs ys 
      | lengthS (takeS c1 xs) == c1 && lengthS (takeS c2 ys) == c2
        = (takeL c1 xs,takeL c2 ys) 
	  :- zipUs c1 c2 (dropS c1 xs) (dropS c2 ys)
      | otherwise = NullS

zipWithU :: Int -> Int -> ([a]->[b]->[c]) -> Signal a -> Signal b -> Signal c
zipWithU _ _ _ NullS _     = NullS
zipWithU _ _ _ _     NullS = NullS
zipWithU c1 c2 f xs ys 
	 | lengthS (takeS c1 xs) == c1 && lengthS (takeS c2 ys) == c2
	   = signal (f (takeL c1 xs) (takeL c2 ys))
	     +-+ zipWithU c1 c2 f (dropS c1 xs) (dropS c2 ys)
         | otherwise = NullS

zipWithUC :: Int -> (a->[b]->[c]) -> Signal a -> Signal b -> Signal c
zipWithUC _ _ NullS _ = NullS
zipWithUC _ _ _ NullS = NullS
zipWithUC c1 f xs ys
	 | lengthS (takeS 1 xs) == 1 && lengthS (takeS c1 ys) == c1
	   = signal (f (headS xs) (takeL c1 ys))
	     +-+ zipWithUC c1 f (tailS xs) (dropS c1 ys)
         | otherwise = NullS

zipWith3U :: Int -> Int -> Int -> ([a]->[b]->[c]->[d]) -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3U _ _ _ _ NullS _ _ = NullS
zipWith3U _ _ _ _ _ NullS _ = NullS
zipWith3U _ _ _ _ _ _ NullS = NullS
zipWith3U c1 c2 c3 f xs ys zs
	 | lengthS (takeS c1 xs) == c1 && lengthS (takeS c2 ys) == c2 && lengthS (takeS c3 zs) == c3
	   = signal (f (takeL c1 xs) (takeL c2 ys) (takeL c3 zs))
	     +-+ zipWith3U c1 c2 c3 f (dropS c1 xs) (dropS c2 ys) (dropS c3 zs)
         | otherwise = NullS
         
zipWith4U :: Int -> Int -> Int -> Int -> ([a]->[b]->[c]->[d]->[e]) ->
               Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4U _ _ _ _ _ NullS _ _ _= NullS
zipWith4U _ _ _ _ _ _ NullS _ _ = NullS
zipWith4U _ _ _ _ _ _ _ NullS _ = NullS
zipWith4U _ _ _ _ _ _ _ _ NullS = NullS
zipWith4U c1 c2 c3 c4 f xs ys zs as
	 | lengthS (takeS c1 xs) == c1 && lengthS (takeS c2 ys) == c2 
           && lengthS (takeS c3 zs) == c3 && lengthS (takeS c4 as) == c4
	   = signal (f (takeL c1 xs) (takeL c2 ys) (takeL c3 zs) (takeL c4 as))
	     +-+ zipWith4U c1 c2 c3 c4 f (dropS c1 xs) (dropS c2 ys) (dropS c3 zs) (dropS c4 as)
         | otherwise = NullS

unzipU :: Signal ([a],[b]) -> (Signal a,Signal b)
unzipU NullS = (NullS,NullS)
unzipU ((as,bs):-xs) = (signal as +-+ ass, 
		        signal bs +-+ bss)
		     where (ass,bss) = unzipU xs

sourceU :: (a->a) -> a -> Signal a
sourceU g state = newstate :- sourceU g newstate
		where newstate = g state

sinkU :: (a->Int) -> (a->a) -> a -> Signal b -> Signal b
sinkU _ _ _ NullS = NullS
sinkU gamma g state xs 
      |  length as == c = sinkU gamma g newstate (dropS c xs)
      | otherwise      = NullS
      where as       = takeL c xs
	    c        = gamma state
	    newstate = g state


-- | 'initU' is used to initialise a signal. Its first argument is prepended to its second argument, a signal.
initU ::  [a] -> Signal a -> Signal a
initU initial s = (signal initial) +-+ s

takeL :: Int -> Signal a -> [a]
takeL c = fromSignal . (takeS c)






