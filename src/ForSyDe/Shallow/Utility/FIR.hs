----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Utility.FIR
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements a FIR filters for the synchronous computational model.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Utility.FIR (firSY) where

import ForSyDe.Shallow.MoC.Synchronous
import ForSyDe.Shallow.Core

-- | The function firSY implements a FIR-filter for the synchronous computational model. All kinds of FIR-filters can now be modeled by means of 'firSY'. The only argument needed is the list of coefficients, which is given as a vector of any size. To illustrate this, an 8-th order band pass filter is modeled as follows. 
--
-- > bp = firSY (vector [0.06318761339784, 0.08131651217682, 0.09562326700432, 
-- >         0.10478344432968, 0.10793629404886, 0.10478344432968, 
-- >         0.09562326700432, 0.08131651217682, 0.06318761339784 ])
-- 
firSY :: Fractional a => Vector a -> Signal a -> Signal a
firSY h = innerProdSY h . sipoSY k 0.0
    where k = lengthV h

-- sipoSY :: Int -> b -> Signal b -> Vector (Signal b) 
-- sipoSY n s0 = unzipxSY . scanlSY shiftrV initState
--     where initState = copyV n s0

-- | Create the tapped, or delayed signals for later consumption.
sipoSY :: Int -> b -> Signal b -> Vector (Signal b) 
sipoSY n s0 = iterateV n tap 
    where tap = delaySY s0

-- | Multiply an entire numeric signal by a constant.
gainSY :: (Num a) => a -> Signal a -> Signal a
gainSY h = zipWithSY (*) (infiniteS id h)

-- | Multiply a vector of signals so that every signal is amplified by one
-- entry of the given numeric vector.
gainParallelSYV :: (Num a) => Vector a -> Vector (Signal a) -> Vector (Signal a)
gainParallelSYV = zipWithV (gainSY)

-- | Perform an inner product with the heads of a vector of signals.
innerProdSY :: (Num a) => Vector a -> Vector (Signal a) -> Signal a
innerProdSY coeffs sigs = mapSY (sumV) $ zipxSY amplifiedSignals
   where amplifiedSignals = gainParallelSYV coeffs sigs
         sumV = foldlV (+) 0

