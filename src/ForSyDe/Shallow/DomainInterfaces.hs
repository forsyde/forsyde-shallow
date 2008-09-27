-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.DomainInterfaces
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines domain interface constructors for the multi-rate computational 
-- model.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.DomainInterfaces(downDI, upDI, par2serxDI, ser2parxDI, 
			par2ser2DI, par2ser3DI, par2ser4DI, 
			ser2par2DI, ser2par3DI, ser2par4DI) where

import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.SynchronousLib


-- | The domain interface constructor 'downDI' takes a parameter 'k' and downsamples an input signal.
downDI     :: Num a => a -> Signal b -> Signal b

-- | The domain interface constructors 'upDI' takes a parameter 'k' and upsamples an input signal.
upDI       :: Num a => a -> Signal b -> Signal (AbstExt b)

-- | The domain interface constructor 'par2ser2DI' converts two parallel signals into one signal.
par2ser2DI :: Signal a -> Signal a -> Signal a

-- | The domain interface constructor 'par2ser3DI' converts three parallel signals into one signal
par2ser3DI :: Signal a -> Signal a -> Signal a -> Signal a

-- | The domain interface constructor 'par2ser4DI' converts four parallel signals into one signal
par2ser4DI :: Signal a -> Signal a -> Signal a -> Signal a 
	   -> Signal a


-- | The domain interface constructor 'par2serxDI' converts n parallel signals into one signal.
par2serxDI :: Vector (Signal a) -> Signal a

-- | The domain interface constructor 'ser2par2DI' converts one signal into two parallel signals.
ser2par2DI :: Signal a -> (Signal (AbstExt a), Signal (AbstExt a))

-- | The domain interface constructor 'ser2par3DI' converts one signal into three parallel signals.
ser2par3DI :: Signal a -> (Signal (AbstExt a), Signal (AbstExt a), Signal (AbstExt a))

-- | The domain interface constructor 'ser2par4DI' converts one signal into four parallel signals.
ser2par4DI :: Signal a 
	   -> (Signal (AbstExt a), Signal (AbstExt a), 
               Signal (AbstExt a), Signal (AbstExt a))

-- | The domain interface constructors 'ser2parxDI' converts one signal into n parallel signals.
ser2parxDI :: (Num a, Ord a) => a -> Signal (AbstExt b) 
				  -> Vector (Signal (AbstExt b))

-- Implementation

downDI n xs     = down1 n 1 xs 
  where down1 _ _ NullS   = NullS
	down1 1 1 (x:-xs) = x :- down1 1 1 xs
	down1 n 1 (x:-xs) = x :- down1 n 2 xs
	down1 n m (_:-xs) = if m == n then
			       down1 n 1 xs
			    else
			       down1 n (m+1) xs 

upDI _ NullS   = NullS
upDI n (x:-xs) = (Prst x) :- ((copyS (n-1) Abst) +-+ upDI n xs)

par2ser2DI xs ys  = par2ser2DI' (zipSY xs ys)
  where par2ser2DI' NullS = NullS
	par2ser2DI' ((x,y):-xys) = x:-y:-par2ser2DI' xys

par2ser3DI xs ys zs = par2ser3DI' (zip3SY xs ys zs)
  where par2ser3DI' NullS = NullS
	par2ser3DI' ((x,y,z):-xyzs) = x:- y :-z :- par2ser3DI' xyzs

par2ser4DI ws xs ys zs = par2ser4DI' (zip4SY ws xs ys zs)
  where par2ser4DI' NullS = NullS
        par2ser4DI' ((w,x,y,z):-wxyzs) 
            = w:-x:-y:-z:- par2ser4DI' wxyzs

ser2par2DI = unzipSY . group2SY . delaynSY Abst 2 . mapSY abstExt

ser2par3DI = unzip3SY . group3SY . delaynSY Abst 3 . mapSY abstExt

ser2par4DI = unzip4SY . group4SY . delaynSY Abst 4 . mapSY abstExt


par2serxDI = par2serxDI' . zipxSY 
  where par2serxDI' NullS    = NullS
	par2serxDI' (xv:-xs) = (signal . fromVector) xv 
			          +-+ par2serxDI' xs 

ser2parxDI n = unzipxSY . delaySY (copyV n Abst) 
			. filterAbstDI . group n

group2SY :: Signal t -> Signal (t, t)
group2SY NullS = NullS
group2SY (_:-NullS) = NullS
group2SY (x:-y:-xys) = (x, y) :- group2SY xys

group3SY :: Signal t -> Signal (t, t, t)
group3SY NullS = NullS
group3SY (_:-NullS) = NullS
group3SY (_:-_:-NullS) = NullS
group3SY (x:-y:-z:-xyzs) = (x, y, z) :- group3SY xyzs

group4SY :: Signal t -> Signal (t, t, t, t)
group4SY NullS = NullS
group4SY (_:-NullS) = NullS
group4SY (_:-_:-NullS) = NullS
group4SY (_:-_:-_:-NullS) = NullS
group4SY (w:-x:-y:-z:-wxyzs) = (w, x, y, z) :- group4SY wxyzs 


filterAbstDI :: Signal (AbstExt a) -> Signal a
filterAbstDI NullS          = NullS
filterAbstDI (Abst:-xs)     = filterAbstDI xs
filterAbstDI ((Prst x):-xs) = x :- filterAbstDI xs

group :: (Ord a, Num a) => a -> Signal a1 -> Signal (AbstExt (Vector a1))
group n xs = mapSY (output n) (scanlSY (addElement n)  (NullV, 0) xs)
	       where addElement m (vs, n) x | n < m  = (vs <: x, n+1)
      				            | n == m = (unitV x, 1)
                                            | otherwise = error "Vector of wrong size"
 		     output m (vs, n) | m == n = Prst vs
				      | otherwise = Abst

