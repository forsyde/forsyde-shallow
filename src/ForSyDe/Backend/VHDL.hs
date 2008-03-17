-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the VHDL backend of ForSyDe's embedded compiler
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL 
 (writeVHDL, 
  writeVHDLOps, 
  VHDLOps(..),
  defaultVHDLOps) where

import Control.Monad.State (evalStateT)

import ForSyDe.ForSyDeErr
import ForSyDe.OSharing (readURef)
import ForSyDe.System.SysDef
import ForSyDe.Backend.VHDL.Traverse

-- | Given a System Definition whose name is a valid VHDL _basic_ identifier 
--   (call it \"A\") generate @A.vhd@ in current working directory using 
--   default compilation options.
--   Imp: the input and output signal names of A must be valid VHDL identifiers
--        (basic or extended) and different to @clk@ and @reset@
--        which are reserved for the main clock and reset signals
writeVHDL :: SysDef a -> IO ()
writeVHDL = writeVHDLOps defaultVHDLOps 

-- | 'writeVHDL'-alternative which allows setting VHDL compilation options.
writeVHDLOps :: VHDLOps -> SysDef a -> IO ()
writeVHDLOps ops sysDef = do
  -- initiate the compilation State
  s <- initVHDLTravST $ (readURef.unPrimSysDef.unSysDef) sysDef
  -- Compile te code
  res <- runErrorT $ evalStateT  (setVHDLOps ops >> writeVHDLM) s
  -- Check if the  compilation went well and print an error in case it didn't
  either printVHDLError return res