{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.SysFun
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides a function to check whether a 'Type' complies
-- with the expected type of a system function not.
--
----------------------------------------------------------------------------- 
module ForSyDe.System.SysFun (checkSysFType) where

import ForSyDe.Signal
import ForSyDe.ForSyDeErr

import Control.Monad (when)
import Text.Regex.Posix ((=~))
import Language.Haskell.TH
import Language.Haskell.TH.TypeLib

-- | Given a function type, check whether it complies with the expected
--   type of a system function and, in that case, provide the type and number
--   of the system inputs and outputs.
checkSysFType :: Type -> Q (([Type],Int),([Type],Int))
checkSysFType t = do let (inputTypes, retType, context) = unArrowT t
                         (outCons, outTypes,  _)        = unAppT retType     
                     -- Discard polymorphic system functions
                     -- FIXME: We could support polymorphic signals, but 
                     -- requires more work and we don't still know if it 
                     -- will be necessary anyway.
                     when (isPoly context) (qGiveUp name)
                     -- Check that all inputs are signals
                     when (not $ all isSignalT inputTypes) (qGiveUp name)
                     -- Check the outputs
                     outInfo <- checkSysFOutputs (outCons, outTypes)
                     return ((inputTypes, length inputTypes), outInfo)
 where name = "ForSyDe.System.checkSysFType"


-- | Check the output types of the system function given its
--   constructor and its type arguments
checkSysFOutputs :: (Type, [Type]) -> Q ([Type], Int)
-- The system function doesn't output any signals at all
checkSysFOutputs (ConT n, []) |  n == ''() = return ([],0)
-- The system function just returns a signal
checkSysFOutputs (ConT s, [arg]) | s == ''Signal  = 
 return ([ConT ''Signal  `AppT` arg ], 1)
-- The system function returns various signals in a tuple
-- NOTE: Unfortunatelly due to ghc's bug 1849 TupleT is never returned by reify
-- so we have to match the constructor with a regex
-- FIXME: Update this function whenever the bug is fixed 
--        http://hackage.haskell.org/trac/ghc/ticket/1849
checkSysFOutputs (ConT name, outTypes) 
 | show name =~ "^Data\\.Tuple\\.\\(,+\\)$" =  do
     when (not $ all isSignalT outTypes) (qGiveUp checkSysFOutputsNm)
     return (outTypes, length outTypes)
-- Otherwise the function output type is incorrect
checkSysFOutputs _ = qGiveUp checkSysFOutputsNm

checkSysFOutputsNm :: String
checkSysFOutputsNm = "ForSyDe.System.SysFun.checkSysFOutputs"

-- | Check if a type corresponds to a signal
isSignalT :: Type -> Bool
isSignalT ((ConT name)  `AppT` _ ) | name == ''Signal = True
isSignalT _                                           = False
