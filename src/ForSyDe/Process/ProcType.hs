{-# LANGUAGE ScopedTypeVariables, FlexibleInstances,
             UndecidableInstances, OverlappingInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process.ProcType
-- Copyright   :  (c) The ForSyDe Team 2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (non-standard instances)
--
-- This module includes and exports, the internal definition, instantiations
-- and related types of 'ProcType', a class used to constrain the arguments
-- taken by process constructors.
----------------------------------------------------------------------------- 
module ForSyDe.Process.ProcType (EnumAlgTy(..), ProcType(..)) where

import Data.Generics
import Data.Set
import Data.Param.FSVec (FSVec)
import Data.TypeLevel.Num.Sets (Nat)

-- | Data type describing an algebraic enumerated type (i.e. an algrebraic 
--   type whose data constructors have arity zero)
data EnumAlgTy = EnumAlgTy String [String] 
 deriving Show

instance Eq EnumAlgTy where
 (EnumAlgTy d1 _) == (EnumAlgTy d2 _) = d1 == d2

instance Ord EnumAlgTy where
 (EnumAlgTy d1 _) `compare` (EnumAlgTy d2 _) = d1 `compare` d2

-- | Class used to constrain the arguments (values and 'ProcFun's) taken by
--   process constructors
class Data a => ProcType a where
 -- | Get the associated enumerated type-definitions of certain value, 
 --   taking nesting in account.  
 -- 
 --   For example:  
 -- @
 --   data Colors = Blue | Red
 --    deriving (Data, Typeable)
 --   data Shapes = Circle | Square
 --    deriving (Data, Typeable)
 --
 --   getEnums ((Blue,1), Circle) =
 --   FIXME: complete example
 -- @
 getEnums :: a -> Set EnumAlgTy

instance Data a => ProcType a where
 getEnums _ = maybe empty singleton (getEnumAlgTy (undefined :: a))

instance (ProcType a, ProcType b) => ProcType (a,b) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)


instance (ProcType a, ProcType b, ProcType c) => ProcType (a,b,c) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)
     `union`  getEnums (undefined :: c)   


instance (ProcType a, ProcType b, ProcType c, ProcType d) 
      => ProcType (a,b,c,d) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)
     `union`  getEnums (undefined :: c) `union` getEnums (undefined :: d)     


instance (ProcType a, ProcType b, ProcType c, ProcType d, ProcType e) 
      => ProcType (a,b,c,d,e) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)
     `union`  getEnums (undefined :: c) `union` getEnums (undefined :: d)     
     `union`  getEnums (undefined :: e)     


instance (ProcType a, ProcType b, ProcType c, ProcType d, ProcType e, 
          ProcType f) 
      => ProcType (a,b,c,d,e,f) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)
      `union` getEnums (undefined :: c) `union` getEnums (undefined :: d)     
      `union` getEnums (undefined :: e) `union` getEnums (undefined :: f)      


instance (ProcType a, ProcType b, ProcType c, ProcType d, ProcType e, 
          ProcType f, ProcType g) 
      => ProcType (a,b,c,d,e,f,g) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)
      `union` getEnums (undefined :: c) `union` getEnums (undefined :: d)     
      `union` getEnums (undefined :: e) `union` getEnums (undefined :: f)      
      `union` getEnums (undefined :: g)

instance (Typeable c, Nat c, ProcType a) => ProcType (FSVec c a) where
 getEnums _ = getEnums (undefined :: a)

instance (ProcType a, ProcType b) => ProcType (a -> b) where
 getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b)

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

