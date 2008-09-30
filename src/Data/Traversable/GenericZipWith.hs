---------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.GenericZipWith
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides generalized zipWith functions.
-- 
-----------------------------------------------------------------------------
module Data.Traversable.GenericZipWith 
 (zipWithTF,
  zipTF,
  zipWithTFA,
  zipWithTFM) where


import Data.Foldable
import Data.Traversable
import qualified Data.Traversable as T
import Control.Applicative
import Control.Monad.State 

-- | The state contains the list of values obtained form the foldable container
--   and a String indicating the name of the function currectly being executed
data ZipState a = ZipState {fName :: String,
                            list  :: [a]}

-- | State monad containing ZipState
type ZipM l a = State (ZipState l) a

-- | pops the first element of the list inside the state
pop :: ZipM l l
pop = do 
 st <- get 
 let xs = list st
     n = fName st
 case xs of
   (a:as) -> do put st{list=as}
                return a
   [] -> error $ n ++ ": insufficient input"

-- | pop a value form the state and supply it to the second 
--   argument of a binary function 
supplySecond :: (a -> b -> c) -> a -> ZipM b c
supplySecond f a = do b <- pop  
                      return $ f a b

zipWithTFError :: (Traversable t,Foldable f) => 
                  String -> (a -> b -> c) -> t a -> f b -> t c  
zipWithTFError str g t f = evalState (T.mapM (supplySecond g) t) 
                                     (ZipState str (toList f))


zipWithTF :: (Traversable t,Foldable f) => (a -> b -> c) -> t a -> f b -> t c
zipWithTF = zipWithTFError "GenericZip.zipWithTF"

zipTF :: (Traversable t, Foldable f) => t a -> f b -> t (a,b)
zipTF = zipWithTFError "GenericZip.zipTF"  (,) 


zipWithTFM :: (Traversable t,Foldable f,Monad m) => 
              (a -> b -> m c) -> t a -> f b -> m (t c)
zipWithTFM g t f = T.sequence (zipWithTFError "GenericZip.zipWithTFM"  g t f)
 
zipWithTFA :: (Traversable t,Foldable f,Applicative m) => 
              (a -> b -> m c) -> t a -> f b -> m (t c)
zipWithTFA g t f = sequenceA (zipWithTFError "GenericZip.zipWithTFA" g t f)
