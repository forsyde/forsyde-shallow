-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Utility.Gaussian
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- We follow the Box-Muller method to generate white gaussian noise, 
-- described at: <http://www.dspguru.com/howto/tech/wgn.htm>
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Utility.Gaussian (
      pGaussianNoise
    )
where

import ForSyDe.Shallow.MoC.Untimed
import ForSyDe.Shallow.Core.Signal

import System.Random

-- |To generate an infinite Signal of Gaussian values
pGaussianNoise:: Double -- Mean value of the Gaussian noise
    -> Double   -- Variance of the Gaussian noise
    -> Int  -- The seed
    -> Signal Double -- Output gaussian noise signal
pGaussianNoise mean variance = mapU 2 gaussianXY . pUnitNormXY
  where
    gaussianXY [x, y] = [mean + sqrt(variance) * x,
         mean + sqrt(variance) * y]
    gaussianXY _  = error "gaussianXY: unexpected pattern."

-- |To get a uniform random variable in the range [0, 1]
uniform :: (Fractional a, RandomGen g, Random a) => 
  g -> (a, g)
uniform rGen = randomR (0.0,1.0) rGen

-- |To generate an infinite signal of unit normal random variables,
-- with the specified seed
pUnitNormXY :: Int     -- The seed
     -> Signal Double  -- The infinite ouput signal
pUnitNormXY = mapU 3 unitNormXY . signal . svGenerator . mkStdGen
  where
    unitNormXY [s, v1, v2] = [sqrt(-2 * log(s) / s) * v1,
          sqrt(-2 * log(s) / s) * v2]
    unitNormXY _ = error "pUnitNormXY: Unexpected pattern."


-- |To generate the s, v1, v2 value
svGenerator :: StdGen -> [Double]
svGenerator s
    | sVal >=1 = []++ svGenerator newStdG
    | otherwise = svVal ++ svGenerator newStdG
  where
    svGen1 = svHelper  s
    svVal = fst svGen1
    sVal = head svVal
    newStdG = snd svGen1
    svHelper :: StdGen -> ([Double], StdGen)
    svHelper stdG = ([s, v1, v2], sNew2)
      where
        (u1, sNew1) = uniform stdG
        (u2, sNew2) = uniform sNew1
        v1=2 * u1 -1
        v2=2 * u2 -1
        s = v1*v1 + v2*v2
