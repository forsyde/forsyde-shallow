---------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable.TypeRepLib
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides basic functions related to Data.Typeables's 'TypeRep'.
-- 
-----------------------------------------------------------------------------
module Data.Typeable.TypeRepLib (unArrowT) where

import Data.Typeable


-- | Obtains the arguments and return type of a given 'TypeRep' 
--   (normally a function)
--   together with its 'Context' (non-empty if the type is polymorphic)
unArrowT :: TypeRep        -- ^ TypeRep to observe  
        ->  ([TypeRep], TypeRep) -- ^ (args 'TypeRep', ret 'TypeRep')
unArrowT rep
 | repCon == arrowTyCon = let (args', ret') = unArrowT  arrowArg2
                          in (arrowArg1:args', ret')
 | otherwise = ([], rep)
 where (repCon,~[arrowArg1,arrowArg2]) = splitTyConApp rep

arrowTyCon :: TyCon
arrowTyCon = (typeRepTyCon.typeOf) (undefined :: () -> ())