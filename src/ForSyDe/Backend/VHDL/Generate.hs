-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Traverse.VHDLM
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used to generate VHDL AST elements without making any 
-- explicit translation.
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Generate where

import ForSyDe.Backend.VHDL.Constants
import ForSyDe.Backend.VHDL.AST
import ForSyDe.Backend.VHDL.Traverse.VHDLM

-- | Generate a list of asignments (in 'ConcSm' form) of intermediate signals
--   (first argument) to final output signals (second argument) 
genOutAssigns :: [IntSignalInfo] -> [VHDLId] -> [ConcSm]
genOutAssigns = zipWith assign
 where assign (IntSignalInfo{sId=orig}) dest = 
              CSSASm $ orig `genSimpleAssign` dest

-- | Generate a simple signal assignment, from a VHDL identifier to a
--   VHDL identifier
genSimpleAssign :: VHDLId -- ^ origin 
               ->  VHDLId -- ^ destination
               ->  ConSigAssignSm
genSimpleAssign orig dest = 
  dest :<==: (ConWforms [] (Wform [PrimName orig]) Nothing)  


-- | Generate a design file from the traversing results and the translated 
--   entity declaration
genDesignFile :: EntityDec -> TravResult -> DesignFile 
genDesignFile ent@(EntityDec id _) (TravResult decs stms) = 
   DesignFile contextClause [LUEntity ent, LUArch archBody]
 where archBody = ArchBody  (unsafeVHDLBasicId "synthesizable") id decs stms

-- | Generate a list of association from two lists of signal identifiers
--   The first one establishes the formal parameters
genAssocElems :: [VHDLName] -> [VHDLName] -> [AssocElem]
genAssocElems formalNames actualNames = zipWith genAssoc formalNames actualNames

-- | Generate a port map from two lists of signal identifiers
--   The first list establishes the formal parameters
genPMap :: [VHDLName] -> [VHDLName] -> PMapAspect
genPMap formalNames actualNames = 
  PMapAspect $ genAssocElems formalNames actualNames


-- | Generate a function call from two lists of signal identifiers
--   The first list establishes the formal parameters
genFCall :: VHDLName -> [VHDLName] -> [VHDLName] -> FCall 
genFCall fName formalNames actualNames = 
  FCall fName $ zipWith genAssoc formalNames actualNames
  

-- Generate an association of a formal and actual parameter
genAssoc :: VHDLName -> VHDLName -> AssocElem
genAssoc formal actual = Just formal :=>: ADName actual