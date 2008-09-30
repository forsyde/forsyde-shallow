-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides the VHDL backend of ForSyDe's embedded compiler
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL 
 (writeVHDL, 
  writeVHDLOps,
  writeAndModelsimVHDL,
  writeAndModelsimVHDLOps, 
  VHDLOps(..),
  QuartusOps(..),
  QuartusAction(..),
  checkSynthesisQuartus,
  VHDLDebugLevel(..),
  VHDLRecursivity(..),
  defaultVHDLOps) where

import Control.Monad.State (evalStateT)
import qualified Language.Haskell.TH as TH

import ForSyDe.System.SysFun
import ForSyDe.ForSyDeErr
import ForSyDe.OSharing (readURef)
import ForSyDe.System.SysDef
import ForSyDe.Backend.VHDL.Traverse
import ForSyDe.Backend.VHDL.Modelsim

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
  let sinit = initVHDLTravST $ (readURef.unPrimSysDef.unSysDef) sysDef
  -- Compile the code
  res <- runErrorT $ evalStateT  (setVHDLOps ops >> writeVHDLM) sinit
  -- Check if the  compilation went well and print an error in case it didn't
  either printVHDLError return res



-- | Generate a function which, given a system definition and some simulation
--   stimuli:
--    
--     (1) Writes a VHDL model of the system 
--     
--     (2) Simulates the VHDL model with Modelsim getting the results back to Haskell
writeAndModelsimVHDL :: SysFunToIOSimFun sysF simF =>  
                        Maybe Int -- ^ Number of cycles to simulate
                                --   if 'Nothing' the number will be determined
                                --   by the length of the input stimulti.
                                --   Useful when the system to simulate doesn't
                                --   have inputs or the inputs provided are 
                                --   infinite
                     -> SysDef sysF -- ^ system definition to simulate
                     -> simF 
writeAndModelsimVHDL = writeAndModelsimVHDLOps defaultVHDLOps


-- | 'VHDLOps'-alternative of 'writeAndModelsimVHDL', note that
--   compileModelSim will implicitly be set to True
writeAndModelsimVHDLOps :: SysFunToIOSimFun sysF simF => 
                           VHDLOps -> Maybe Int -> SysDef sysF -> simF 
writeAndModelsimVHDLOps ops mCycles sysDef = fromTHStrSimFun simIO []
 where sinit = initVHDLTravST $ (readURef.unPrimSysDef.unSysDef) sysDef
       simVHDLM :: [[TH.Exp]] -> VHDLM [[String]] 
       simVHDLM stimuli = do 
         setVHDLOps ops{compileModelsim=True} 
         writeVHDLM
         executeTestBenchModelsim mCycles stimuli 
       simIO :: [[TH.Exp]] -> IO [[String]] 
       simIO stimuli = do
         res <- runErrorT $ evalStateT (simVHDLM stimuli) sinit
         either printVHDLError return res
