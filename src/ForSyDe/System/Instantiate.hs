{-# LANGUAGE TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.Instantiate
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides instantiation capabilities to System Definitions
-- ('SysDef's).through a  Template-Haskell-based function: 'instantiate'.
--
-- Thanks to 'instantiate' ForSyDe models have hierarchy capabilities.
--
-----------------------------------------------------------------------------
module ForSyDe.System.Instantiate (instantiate) where

import ForSyDe.Ids (ProcId)
import ForSyDe.OSharing (readURef)
import ForSyDe.Netlist
import ForSyDe.System.SysDef (SysDef)
import ForSyDe.System.SysFun (SysFun(fromListSysFun))




-- | Generates an instance of a 'SysDef' in the form of  
--   function out of the name of a 'SysDef' with the same type as its 
--   system function. The resulting function can then be used to plug the 
--   instance to the rest of the system.
instantiate :: SysFun f => ProcId -> SysDef f -> f
instantiate id sysDef = fromListSysFun (instantiateList id sysDef) []


-------------------
-- Helper Functions
-------------------

instantiateList :: ProcId -> SysDef a -> [NlSignal] -> [NlSignal]
instantiateList id sysDef inSigs = map (\t -> newNodeOutSig instanceSig t) tags 
   where instanceSig = newSysIns id sysDef inSigs 
         tags = outTags $ readURef instanceSig 
