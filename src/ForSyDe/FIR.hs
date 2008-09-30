{-# LANGUAGE TemplateHaskell, RelaxedPolyRec, PatternGuards #-}
-- The PatternGuards are used to hush innapropiate compiler warnings
-- see http://hackage.haskell.org/trac/ghc/ticket/2017
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.FIR
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements FIR filters for the synchronous computational model.
-----------------------------------------------------------------------------
module ForSyDe.FIR (fir) where

import ForSyDe.Ids
import ForSyDe.Signal
import ForSyDe.Process

import Data.TypeLevel.Num (Nat, Pos)
import Data.Param.FSVec hiding ((++))
import qualified Data.Param.FSVec as V
import Data.Typeable

-- | 
-- All kinds of FIR-filters can now be modeled by means of 'fir'. The
-- only argument needed is the list of coefficients, which is given as
-- a vector of any size. To illustrate this, an 8-th order band pass
-- filter is modeled as follows.
--
-- > bp = fir "fir Id" $(vectorTH [0.06318761339784, 0.08131651217682, 0.09562326700432, 
-- >                               0.10478344432968, 0.10793629404886, 0.10478344432968, 
-- >                               0.09562326700432, 0.08131651217682, 0.06318761339784 ])
-- 

fir :: (Fractional b, ProcType b, Pos s, Typeable s) => 
       ProcId -> FSVec s b -> Signal b -> Signal b
fir id h = innerProd (id ++ "_innerProd") h . sipo (id ++ "_sipo") k 0.0
    where k = V.lengthT h

sipo :: (Pos s, Typeable s, Fractional a, ProcType a) =>
        ProcId -> s -> a -> Signal a -> FSVec s (Signal a)
sipo id n s0 = unzipxSY (id ++ "_unzipxSY") . scanldSY (id ++ "_scanldSY") srV initState
    where initState = V.copy n s0
          srV = $(newProcFun [d| srV :: Pos s => FSVec s a -> a -> FSVec s a
                                 srV v a = V.shiftr v a |])

innerProd :: (Fractional a, ProcType a, Nat s, Typeable s) =>
             ProcId -> FSVec s a -> FSVec s (Signal a) -> Signal a
innerProd id h = zipWithxSY id (ipV `defArgVal` h)
   where ipV = $(newProcFun 
                  -- We could make the inner product in one traverse 
                  -- but FSVecs don't allow recursive calls
                  -- (they don't allow to check the constraints statically)
                  -- Thus, we traverse the vector twice
                  [d| ipV :: (Nat s, Num a) => FSVec s a -> FSVec s a -> a
                      ipV v1 v2 = 
                          V.foldl (+) 0 $ V.zipWith (*) v1 v2 |])



