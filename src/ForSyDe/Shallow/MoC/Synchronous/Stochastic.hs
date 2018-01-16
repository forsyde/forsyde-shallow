-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.Synchronous.Stochastic
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The stochastic library provides a few stochastic skeletons, which are
-- relatives to the skeletons of the synchronous library. These skeletons are
-- based on two elementary functions, 'sigmaUn' and 'sigmaGe'
-- which provide stochastic signals. The background and motivation for this
-- approach is described in the paper 
--
-- Axel Jantsch, Ingo Sander, and Wenbiao Wu,
-- \"The usage of stochastic processes in embedded system specifications\",
-- In /Proceedings of the Ninth International Symposium on Hardware and Software Codesign/, 
-- April 2001 (<http://web.it.kth.se/~axel/papers/2001/codes-2001.pdf>). 
--
-- Unfortunately, not all of the suggested skeletons are
-- implemented. In particular, consolidation-based process
-- constructors and all constructors for the untimed and the discrete
-- timed MoCs are missing.
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.Synchronous.Stochastic(
         -- * Select based synchronous process constructors
         selMapSY, selScanlSY, selMealySY, selMooreSY
         -- * Elementary stochastic processes
        , sigmaUn, sigmaGe) where

import ForSyDe.Shallow.MoC.Synchronous
import ForSyDe.Shallow.Core.Signal
import System.Random

-- | The skeleton 'selMapSY' is a stochastic variant of
-- 'mapSY'. It has an internal stochastic process and selects one
-- out of two combinatorial functions depending on the output of the
-- stochastic process.
selMapSY   :: Int  -- ^The seed for the stochastic process
       -> (a -> b) -- ^The first alternative function
       -> (a -> b) -- ^The second alternative function
       -> Signal a -- ^The input signal
       -> Signal b -- ^The output signal of the process
selMapSY _    _  _  NullS = NullS
selMapSY seed f0 f1 xs = selmap1 f0 f1 (sigmaUn seed (0,1)) xs
  where selmap1 :: (a->b)->(a->b)->(Signal Int) -> Signal a -> Signal b
        selmap1 _  _  _   NullS = NullS
        selmap1 f0 f1 (s:-ss) (x:-xs) 
          = (select1 s f0 f1 x) :- (selmap1 f0 f1 ss xs)
        selmap1 _  _  NullS _ = error "selMapSY: empty seed signal."

-- | The skeleton 'selScanlSY' is a stochastic variant of 'scanlSY'.
selScanlSY :: Int       -- ^The seed
       -> (a -> b -> a) -- ^The first alternative next-state function
       -> (a -> b -> a) -- ^The second alternative function
       -> a     -- ^The initial state
       -> Signal b  -- ^The input signal
       -> Signal a  -- ^The output signal
selScanlSY _    _  _  _   NullS   = NullS
selScanlSY seed f0 f1 mem xs  = selscan1 f0 f1 mem (sigmaUn seed (0,1)) xs
      where selscan1 :: (a -> b -> a) -> (a -> b -> a) -> a 
                     -> Signal Int -> Signal b -> Signal a
            selscan1 _  _  _   _   NullS = NullS
            selscan1 f0 f1 mem (s:-ss) (x:-xs)
              = newmem :- (selscan1 f0 f1 newmem ss xs)
              where newmem = (select2 s f0 f1 mem x) 
            selscan1 _  _  _   NullS _ 
              = error "selScanlSY: empty seed signal"

select1    :: Int -> (a -> b) -> (a->b) -> a -> b
select1 0 f0 _  x =  f0 x
select1 1 _  f1 x =  f1 x
select1 s _  _  _ =  error ("select1: seed value neither 0 nor 1: " 
            ++ (show s))

select2      :: Int -> (a -> b -> c) -> (a->b->c) 
     -> a -> b -> c
select2 0 f0 _  x y =  f0 x y
select2 1 _  f1 x y =  f1 x y
select2 s _  _  _ _ =  error ("select2: seed value neither 0 nor 1: " 
          ++ (show s))

-- | 'selMooreSY' is the stochastic variant of mooreSY. Both the 
--   next-state and the output function is randomly selected based on a 
--   uniform distribution.
selMooreSY :: Int       -- ^The seed for the next-state function
       -> Int       -- ^The seed for the output function
       -> (a -> b -> a) -- ^First alternative for the next-state function
       -> (a -> b -> a) -- ^Second alternative for the next-state function
       -> (a -> c)  -- ^First alternative for the output function
       -> (a -> c)  -- ^Second alternative for the output function
       -> a     -- ^The initial state
       -> Signal b  -- ^The input signal
       -> Signal c  -- ^The output signal
selMooreSY _ _ _ _ _ _ _ NullS = NullS
selMooreSY seedg seedf g0 g1 f0 f1 w0 s
    = ((selMapSY seedf f0 f1 ) . (selScanlSY seedg g0 g1 w0)) s

-- | 'selMealySY' is the stochastic variant of mealySY. Both the 
--   next-state and the output function is randomly selected based on a 
--   uniform distribution.
selMealySY :: Int       -- ^The seed for the next-state function
       -> Int       -- ^The seed for the output function
       -> (a -> b -> a) -- ^First alternative for the next-state function
       -> (a -> b -> a) -- ^Second alternative for the next-state function
       -> (a -> b -> c) -- ^First alternative for the output function
       -> (a -> b -> c) -- ^Second alternative for the output function
       -> a     -- ^The initial state
       -> Signal b  -- ^The input signal
       -> Signal c  -- ^The output signal
selMealySY _ _ _ _ _ _ _ NullS = NullS
selMealySY seedg seedf g0 g1 f0 f1 w0 s
    = ((selMapSY seedf f0' f1' ) . (zipSY s) . (selScanlSY seedg g0 g1 w0)) s
  where f0' (b, a) = f0 a b
        f1' (b, a) = f1 a b
-- |'sigmaUn' generates a signal list of uniformly distributed Int within
-- the given range and with a given seed. 
sigmaUn :: Int    -- ^The seed
    -> (Int, Int) -- ^The interval from which the stochastic values are 
          --  taken
    -> Signal Int -- ^The sequence of stochastic values
sigmaUn seed range = signal (stoch range (mkStdGen seed))
    where stoch :: (Int, Int) -> StdGen -> [Int]
          stoch range g = newNum `seq` 
                          (newNum : (stoch range newGen))
            where newNum = (fst (randomR range g)) 
                  newGen = snd (next g)

-- |'sigmaGe' is a more general stochastic process. The first argument is a
-- function f which describes the distribution. For each value v in the
-- given range (r1,r2), f(v) is the probability that v is generated.  
--
-- Note, that the user has to make sure that sum(f(v))=1 for v in (r1,r2).
--
-- For illustration consider the following example.
--
-- > pdist :: Float -> Float
-- > pdist d = 1\/\(2**d\)
-- > pdistsum 1 = pdist 1
-- > pdistsum d = \(pdist d\) + \(pdistsum \(d-1\)\)
--
-- > pdistnorm :: Float -> Float -> Float
-- > pdistnorm dmax d = 1\/((pdistsum dmax) * (2**d))
--
-- @pdistnorm dmax d@ gives the probability of a value <= d;
--
-- @pdistnorm dmax dmax@ is always 1.0
--
-- Hence, using pdistnorm as a function in 'sigmaGe' gives an exponantial
-- distribution for values in the range \[0, dmax\].
sigmaGe :: (Float -> Float) -- ^The stochastic distribution
    -> Int      -- ^The seed
    -> (Int, Int)   -- ^The range
    -> Signal Int   -- ^The sequence of stochastic values
sigmaGe f seed (r1,r2) = sigma2 (checkSum f (fromIntegral r1) 
               (fromIntegral r2)) f seed (r1,r2)
  where sigma2 s f seed (r1,r2) 
          | s > 0.999 = signal (sigma1 (mkStdGen seed) 
                                (mkdlist f (fromIntegral (r2-r1))))
          | otherwise = error 
                        ("sigmaGe: sum of probabilitites is "
                         ++ (show s) ++ ". It must be 1.")
        checkSum :: (Float -> Float) -> Float -> Float -> Float
        checkSum f c max | c == max = f c
                         | otherwise = f(c) + (checkSum f (c+1) max)

        sigma1 :: StdGen -> [Float] -> [Int]
        sigma1 g fl = (findk (fst (randomR (0.0,1.0) g)) fl)
                      : (sigma1 (snd (next g)) fl)

        findk :: Float -> [Float] -> Int
        findk r fs = findk1 0 r fs

        findk1 k r (f:fs) | r < f = k
                          | otherwise = findk1 (k+1) r fs
        findk1 k _ []  = k

        mkdlist :: (Float -> Float) -> Float  -> [Float]
        mkdlist f d = scanl (sumf f) 0.0 [1..d]

        sumf :: (Float -> Float) -> Float -> Float -> Float
        sumf g x y = x + (g y)

--pdist :: Float -> Float
--pdist d = 1/(2**d)

--pdistsum 1 = pdist 1
--pdistsum d = (pdist d) + (pdistsum (d-1))

-- The function pdistnorm can be used as a function in sigmaGe for an
-- exponantial distribution of values in the range [0, dmax]:
--pdistnorm :: Float -> Float -> Float
--pdistnorm dmax d = 1/((pdistsum dmax) * (2**d))

--pdnormsum dmax 1 = pdistnorm dmax 1
--pdnormsum dmax d = (pdistnorm dmax d) + (pdnormsum dmax (d-1))


-----------------------------------------------------------------------------
-- Test section:
--
-- These tests are commented to avoid warnings about not-used functions.
-- But the test functions work and are useful.
-- testAll = "test selMapSY: " ++ testSelMap 
--       ++ ", test selMooreSY: " ++ testSelMoore
--       ++ ", test selMealySY: " ++ testSelMealy

-- testSelMap = show so ++ " -> " ++ (cmpSig so (signal [0,3,4,5,4,5,8,9,8,11]))
--     where f0 x = x + 1
--       f1 x = x - 1
--       so = takeS 10 (selMapSY 876876 f0 f1 (signal [1,2..]))

-- testSelMoore = show so ++ " -> " 
--        ++ (cmpSig so (signal [10,2,3,-40,-5,0,7,-80,0,-100]))
--     where so = takeS 10 (selMooreSY 7667567 123234 g0 g1 f0 f1 w0 
--                 (signal [1,2..]))
--       g0 (0,y) x | even x     = (0,x)
--          | otherwise  = (1,x)
--       g0 (1,y) x | x `mod` 3 == 0 = (0,x)
--          | otherwise  = (1,x)
--       g1 (0,y) x | even x     = (1,x)
--          | otherwise  = (0, x)
--       g1 (1,y) x | x `mod` 3 == 0 = (0,0)
--          | otherwise  = (1,x)
--       f0 (0,y) = y
--       f0 (1,y) = -1 * y
--       f1 (0,y) = 10 * y
--       f1 (1,y) = -10 * y
--       w0 = (0,0)

-- testSelMealy = show so ++ " -> " 
--        ++ (cmpSig so (signal [10,2,3,-40,-5,0,7,-80,0,-100]))
--     where so = takeS 10 (selMealySY 7667567 123234 g0 g1 f0 f1 w0 
--                 (signal [1,2..]))
--       g0 (0,y) x | even x     = (0,x)
--          | otherwise  = (1,x)
--       g0 (1,y) x | x `mod` 3 == 0 = (0,x)
--          | otherwise  = (1,x)
--       g1 (0,y) x | even x     = (1,x)
--          | otherwise  = (0, x)
--       g1 (1,y) x | x `mod` 3 == 0 = (0,0)
--          | otherwise  = (1,x)
--       f0 (0,y) x = y
--       f0 (1,y) x = -1 * y
--       f1 (0,y) x = 10 * y
--       f1 (1,y) x = -10 * y
--       w0 = (0,0)
--
--
-- cmpSig :: Eq a => Signal a -> Signal a -> String
-- cmpSig s1 s2 | s1 == s2 = "OK"
--      | otherwise = "Not OK"
