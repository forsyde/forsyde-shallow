-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.OSharing
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (GHC libraries)
--
-- This module provides support for unsafe dynamic typing.
--
--  Use with care. Normally, "Data.Dynamic" is preferred since it provides a
-- safe casting operation thanks to the use of "Data.Typeable". 
--
-- /This module is based on Lava2000/: <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------
module ForSyDe.OSharing.UDynamic 
 (UDynamic, 
  unsafeToUDyn,
  unsafeFromUDyn)
  
 where

import Unsafe.Coerce (unsafeCoerce)
import GHC.Base (Any)

-- | A value of type 'UDynamic' (from Unsafe Dynamic) can
--   implicitly any Haskell value. 
--
--   It is unsafe because, as opposed to 'Data.Dynamic', 
---  it does not encapsule type information and does
--   it makes unsafe to cast a 'UDynamic' back to its original Haskell type.
data UDynamic = UDynamic Any

-- | Converts an arbitrary value into an object of type 'UDynamic'.
--   It is unsafe because it does not provide type information during the 
--   transformation
unsafeToUDyn :: a -> UDynamic
unsafeToUDyn a = UDynamic (unsafeCoerce a)

-- | Converts a 'UDynamic' object back into an ordinary Haskell value.
--   It is unsafe because there is no way to unsure converting
--   back to the correct original Haskell type. Thus, this
--   function should be used with care since it can cuase disastrous errors.
-- 
--   e.g. disastrousCast :: a -> b
--        disastrousCast = unsafeToDyn . unsafeFromDyn
--
--        The following program would coredump
--          main = putStr (disastrousCast 1 :: String) 
unsafeFromUDyn :: UDynamic -> a
unsafeFromUDyn (UDynamic obj) = unsafeCoerce obj 
