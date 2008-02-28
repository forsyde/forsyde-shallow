{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- FIXME: Add header
-- | The module includes the standard Discrete Fourier Transform (DFT) function, and a fast Fourier transform (FFT) algorithm, for computing the DFT, when the size N is a power of 2.
module ForSyDe.DFT(dft, fft) where



import qualified Data.Param.FSVec as V
import Data.Param.FSVec 
import Data.TypeLevel (Nat, IsPowOf, D2)
import Data.Complex


-- | The function 'dft' performs a standard Discrete Fourier Transformation
dft :: forall s . Nat s => FSVec s (Complex Double) -> FSVec s (Complex Double)
dft v = V.map (bigX_k v) nVector
   where
     lT = V.lengthT v
     lV = V.genericLength v
     -- FIXME: dft does not type-check without this type signature:
     --        learn why!
     nVector :: Num a => FSVec s a
     nVector = kVector lT
     fullCircle = V.map (\n -> -2*pi*n/lV) nVector
     bigX_k v k = sumV (V.zipWith (*) v (bigW k))
     bigW k = V.map (** k) (V.map cis fullCircle)
     sumV = V.foldl (+) (0:+ 0)


-- | The function 'fft' implements a fast Fourier transform (FFT) algorithm, 
--   for computing the DFT, when the size N is a power of 2.
fft :: (Nat s, IsPowOf D2 s) => 
       FSVec s (Complex Double) -> FSVec s (Complex Double)
fft v = V.map (bigX v) (kVector lT)
   where lT = V.lengthT v

kVector :: (Num a, Nat s) => s -> FSVec s a      
kVector s = V.iterate s (+1) 0 


bigX :: (Nat s, IsPowOf D2 s) => 
        FSVec s (Complex Double) -> Integer ->  Complex Double
-- since there are no output length constraints (no vector is being returned)
-- we can simply obtain the list inside the vector and work with it directly
bigX v k = bigX' (V.genericLength v) (V.fromVector v) k
 where bigX' :: Integer -> [Complex Double] -> Integer ->  Complex Double
       -- The first argument is the length of the list (bigN)
       bigX' _ (x0:[x1]) k | even k = x0 + x1 * bigW 2 0
                           | odd k  = x0 - x1 * bigW 2 0
       bigX' l xs k = bigF_even + bigF_odd * bigW l k
           where bigF_even = bigX' halfl (evens xs) k
	         bigF_odd  = bigX' halfl (odds xs) k
                 halfl = l `div` 2

bigW :: Integer -> Integer -> Complex Double
bigW bigN k = cis (-2 * pi * (fromInteger k) / (fromInteger bigN))

evens :: [a] -> [a]
evens []  = []
evens [v1] = [v1] 
evens (v1:_:v) = v1 : evens v

odds :: [a] -> [a]
odds [] = []
odds [_] = []
odds (_:v2:v) = v2 : odds v


