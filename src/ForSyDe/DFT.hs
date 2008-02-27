-- FIXME: Add header
-- | The module includes the standard Discrete Fourier Transform (DFT) function, and a fast Fourier transform (FFT) algorithm, for computing the DFT, when the size N is a power of 2.
module ForSyDe.DFT(dft, fft) where

import ForSyDe.Vector
import Data.Complex

-- | The function 'dft' performs a standard Discrete Fourier Transformation
dft :: Integer -> Vector (Complex Double) -> Vector (Complex Double)
dft bigN x | bigN == (lengthV x) = mapV (bigX_k bigN x) (nVector x)
           | (lengthV x) == 0 = copyV bigN (toComplex 0.0)
	   | otherwise   = error "DFT: Vector has not the right size!"   
   where
     nVector x'       = iterateV (lengthV x') (+1) 0
     bigX_k bigN' x' k = sumV (zipWithV (*) x' (bigW' k bigN'))
     bigW' k' bigN'     = mapV (** k') (mapV cis (fullcircle bigN'))
     sumV            = foldlV (+) (0:+ 0)
     toComplex x = x :+ 0

fullcircle :: Integer -> Vector Double 
fullcircle n = fullcircle1 0 (fromInteger n) n
	  where
	     fullcircle1 l m n' 
		| l == m    = NullV
		| otherwise = -2*pi*l/(fromInteger n') 
			      :> fullcircle1 (l+1) m n' 

-- | The function 'fft' implements a fast Fourier transform (FFT) algorithm, for computing the DFT, when the size N is a power of 2.
fft :: Integer -> Vector (Complex Double) -> Vector (Complex Double)
fft bigN xv | bigN == (lengthV xv) = mapV (bigX xv) (kVector bigN)
	    | otherwise = error "FFT: Vector has not the right size!"


kVector :: (Num b, Num a) => a -> Vector b      
kVector bigN = iterateV bigN (+1) 0 


bigX :: Vector (Complex Double) -> Integer -> Complex Double
bigX (x0:>x1:>NullV) k | even k = x0 + x1 * bigW 2 0
		       | odd k  = x0 - x1 * bigW 2 0
bigX xv k = bigF_even k + bigF_odd k * bigW bigN (fromInteger k)
     where bigF_even k' = bigX (evens xv) k'
	   bigF_odd k' = bigX (odds xv) k'
	   bigN = lengthV xv

bigW :: Integer -> Integer -> Complex Double
bigW bigN k = cis (-2 * pi * (fromInteger k) / (fromInteger bigN))

evens :: Vector a -> Vector a
evens NullV       = NullV
evens (v1:>NullV) = v1 :> NullV
evens (v1:>_:>v)  = v1 :> evens v

odds :: Vector a -> Vector a
odds NullV        = NullV
odds (_:>NullV)   = NullV
odds (_:>v2:>v)   = v2 :> odds v





