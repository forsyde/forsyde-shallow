{-# LANGUAGE ScopedTypeVariables, FlexibleInstances,
             UndecidableInstances, OverlappingInstances, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.SysFun.Instances
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides the 'ProcType' instances (and 'Data',
-- 'Typeable' and 'Lift' when necessary) . It cannot be included in
-- "ForSyDe.Process.ProcType" directly due to a Template Haskell bug
-- which prevents Template Haskell from executing functions defined in
-- the same module: <http://hackage.haskell.org/trac/ghc/ticket/1800>
--
----------------------------------------------------------------------------- 
module ForSyDe.Process.ProcType.Instances where

import ForSyDe.Config (maxTupleSize)
import ForSyDe.Process.ProcType

import Data.TypeLevel.Num.Sets (Nat, toInt)
import Data.Param.FSVec (FSVec, reallyUnsafeVector)

import Data.Char (isAlphaNum)
import Data.Generics
import Data.Maybe (fromJust)
import Control.Monad (liftM, liftM2)
import Text.ParserCombinators.ReadP
import Data.Set (empty, singleton)
import Language.Haskell.TH.Syntax (Lift(..), runIO)

-------------------------------------------------------------
-- Checking if a member of Data belongs to an enumerated type
-------------------------------------------------------------

-- | Phony type used to check if a data constructor is enumerate (has )
newtype IsConsEnum a = IsConsEnum {unIsConsEnum :: Bool}

-- | Tell if a member of "Data" belongs to an enumerated type
isConsEnum :: Data a => Constr -> IsConsEnum a
isConsEnum = gunfold  (\_ -> IsConsEnum False) (\_ -> IsConsEnum True) 

-- | Tell if a member of "Data" belongs to an enumerated type
--   and return its description.
getEnumAlgTy :: forall a . Data a => a -> Maybe EnumAlgTy
getEnumAlgTy a = case dataTypeRep dt of
  AlgRep cons -> do 
   strs <- mapM (\c -> toMaybe (unIsConsEnum (isConsEnum c :: IsConsEnum a)) 
                               (showConstr c)) cons 
   return (EnumAlgTy dn strs)
  _ -> Nothing
 where dt = dataTypeOf a
       dn = dataTypeName dt
       toMaybe bool c = if bool then Just c else Nothing

-------------
-- Instances
-------------

instance (Lift a, Data a) => ProcType a where
 getEnums _ = maybe empty singleton (getEnumAlgTy (undefined :: a))
 -- We add parenthesis and try to use gread. 
 -- In addition, since gread is broken for unit (), we create or own parser
 readProcType = if typeOf (undefined :: a) == typeOf ()
                   then do skipSpaces 
                           string "()"
                           return (fromJust $ cast $ ()) 
                   else do skipSpaces
                           str <- munch1 isAlphaNum 
                           readS_to_P (gread.('(':).((str ++ ")")++))


instance (Typeable s, Nat s, Lift a, ProcType a) => ProcType (FSVec s a) where
 getEnums _ = getEnums (undefined :: a)
 readProcType = do
          skipSpaces  
          char '<'
          elems <- countSepBy (toInt (undefined :: s))
                              readProcType 
                              (skipSpaces >> char ',' >> skipSpaces)
          char '>'
          return (reallyUnsafeVector elems)
   where countSepBy n p sep = 
            if n == 0 
               then return []
               else liftM2 (:) p (sequence (replicate (n-1) (sep >> p))) 
  

-- Tuple isntances
$(let concatMapM f xs = liftM concat (mapM f xs) 
      msg = "Generating and compiling " ++ show (maxTupleSize -2) ++ 
            " tuple instances of " ++
            show ''ProcType ++  
            ", this might take some time ... \n"
  in runIO (putStrLn $ msg) >>
     concatMapM genTupInstances [2..maxTupleSize])



