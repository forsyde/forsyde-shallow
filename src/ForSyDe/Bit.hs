{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MultiParamTypeClasses,
             FunctionalDependencies, TypeSynonymInstances #-}  
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Bit
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Bit' Datatype. Note that the 'Num' instance is phony and shouldn't be used
-- 
-----------------------------------------------------------------------------
module ForSyDe.Bit (Bit(..), not, bitToBool, boolToBit, BitStream(..))where

import Language.Haskell.TH.Lift
import Data.Int
import Data.Bits
import Data.Generics (Data, Typeable)
import Prelude hiding (not)

import Data.Param.FSVec (FSVec, reallyUnsafeVector)
import qualified Data.Param.FSVec as V
import Data.TypeLevel.Num (D8,D16,D32,D64, Nat)

data Bit = H -- ^ High value 
         | L -- ^ Low value
 deriving (Eq, Show, Data, Typeable)

$(deriveLift1 ''Bit)

-- | Not operation over bits
not :: Bit -> Bit
not = complement


instance Bits Bit where
 H .&. x = x
 L .&. _ = L
 H .|. _ = H
 L .|. x = x
 xor H L = H
 xor L H = H
 xor _ _ = L
 complement L = H
 complement H = L
 shift x 0 = x
 shift _ _ = L
 rotate x _ = x
 bitSize _ = 1
 isSigned _ = False


-- Phony instance of Num
instance Num Bit where
 H + L = H
 H + H = L
 L + x = x
 -- since they are unsigned and there are only two elements, (-) == (+)
 (-) = (+)
 -- multiplication is equivalent to (.&.)
 (*) = (.&.)
 -- since a bit is unsigned, it is equivalent to identity
 abs = id
 signum _ = L
 fromInteger n = if n<=0 then L else H

bitToBool :: Bit -> Bool
bitToBool H = True
bitToBool L = False

boolToBit :: Bool -> Bit
boolToBit True = H
boolToBit False = L


-- | Datatypes  which can be converted to and from
--   vectors of bits
class BitStream d s | d -> s where
 toBitVector :: d -> FSVec s Bit
 fromBitVector :: FSVec s Bit -> d


instance BitStream Int8 D8 where
  toBitVector = reallyUnsafeToBitVector
  fromBitVector = fromBitVectorDef 0


instance BitStream Int16 D16 where
  toBitVector = reallyUnsafeToBitVector
  fromBitVector = fromBitVectorDef 0


instance BitStream Int32 D32 where
  toBitVector = reallyUnsafeToBitVector
  fromBitVector = fromBitVectorDef 0


instance BitStream Int64 D64 where
  toBitVector = reallyUnsafeToBitVector
  fromBitVector = fromBitVectorDef 0


-------------------
-- Helper functions
-------------------
reallyUnsafeToBitVector :: Bits a => a -> FSVec s Bit
reallyUnsafeToBitVector x = 
  reallyUnsafeVector $ map (boolToBit.(testBit x)) [size-1,size-2..0]
 where size = bitSize x

-- | version of fromBitVector supplying a default initial value from which to 
--   start working
fromBitVectorDef :: (Bits a, Nat s) => a -> FSVec s Bit -> a
fromBitVectorDef def vec = fst $ V.foldr f (def, 0) vec
  where f e (ac, pos)  = (copyBit e ac pos, pos+1)
        copyBit H = setBit
        copyBit L = clearBit
 

 
   
