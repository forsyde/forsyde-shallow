-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Model
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the data structure and access
-- functions for the memory model.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Memory (
        Memory (..), Access (..), 
        MemSize, Adr, newMem, memState, memOutput
      ) where

import ForSyDe.Shallow.Core.Vector
import ForSyDe.Shallow.Core.AbsentExt

type Adr    =  Int
type MemSize      =  Int

-- | The data type 'Memory' is modeled as a vector. 
data Memory a         =  Mem Adr (Vector (AbstExt a)) 
            deriving (Eq, Show)

-- | The data type 'Access' defines two access patterns.
data Access a         =  Read Adr     -- ^ 'Read adr' reads an address from the memory.
          |  Write Adr a  -- ^ 'Write Adr a' writes a value into an address.
            deriving (Eq, Show)

-- | The function 'newMem' creates a new memory, where the number of entries is given by a parameter.
newMem        :: MemSize -> Memory a

-- | The function 'memState' gives the new state of the memory, after an access to a memory. A 'Read' operation leaves the memory unchanged.
memState          :: Memory a -> Access a -> Memory a


-- | The function 'memOutput' gives the output of the memory after an access to the memory. A 'Write' operation gives an absent value as output.
memOutput         :: Memory a -> Access a -> AbstExt a

-- Implementation

newMem size       =  Mem size (copyV size Abst)

writeMem          :: Memory a -> (Int, a) -> Memory a
writeMem (Mem size vs) (i, x) 
   | i < size && i >= 0   =  Mem size (replaceV vs i (abstExt x))
   | otherwise        =  Mem size vs

readMem           :: Memory a -> Int -> (AbstExt a)
readMem (Mem size vs) i   
   | i < size && i >= 0   =  vs `atV` i
   | otherwise        =  Abst

memState mem (Read _)     =  mem
memState mem (Write i x)  =  writeMem mem (i, x)

memOutput mem (Read i)    =  readMem mem i
memOutput _   (Write _ _)     =  Abst


