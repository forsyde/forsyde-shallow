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
genOutAssigns :: [VHDLId] -> [VHDLId] -> [ConcSm]
genOutAssigns = zipWith assign
 where assign dest orig = CSSASm $ dest `genSignalAssign` orig

-- | Generate a simple signal assignment, from a VHDL identifier to a
--   VHDL identifier
genSignalAssign :: VHDLId -- ^ destination
               ->  VHDLId -- ^ origin
               ->  ConSigAssignSm
genSignalAssign dest orig = genExprAssign dest (PrimName $ NSimple orig) 

-- | Generate a function call asignment
genFCallAssign ::  VHDLId -- ^ destination signal
              ->  VHDLId -- ^ Function Name
              ->  [VHDLId] -- ^ Function formal parameters
              ->  [VHDLId] -- ^ Function actual parameters
              -> ConSigAssignSm
genFCallAssign dest fName formal actual =  
 genExprAssign dest (PrimFCall $ genFCall fName formal actual)  

-- | Generate a simple assignment from an expression to a name
genExprAssign :: VHDLId -> Expr -> ConSigAssignSm
genExprAssign dest origExpr =
   NSimple dest :<==: (ConWforms [] (Wform [origExpr]) Nothing)  

-- | Generate a system design file for a system from the global system identifier,
--   local traversing results and the translated entity declaration
genSysDesignFile :: String -> EntityDec -> LocalTravResult -> DesignFile 
genSysDesignFile globalSysId ent@(EntityDec id _) (LocalTravResult decs stms) = 
   DesignFile contextClause [LUEntity ent, LUArch archBody]
 where archBody = ArchBody archId  (NSimple id) decs stms      
       archId = unsafeVHDLBasicId "synthesizable"
       libName = globalSysId ++ "_lib"
       libId = unsafeVHDLId libName
       contextClause = commonContextClause ++ 
                   [Library libId,
                    Use $ NSelected (NSimple libId :.: SSimple typesId) :.: All]

-- | Generate a library design file from the global results
genLibDesignFile :: GlobalTravResult -> DesignFile
genLibDesignFile  (GlobalTravResult typeDecs) = 
   DesignFile commonContextClause [LUPackage packageDec]
 where packageDec = PackageDec (unsafeVHDLBasicId "types") typeDecs
       

  
-- | Generate a list of association from two lists of signal identifiers
--   The first one establishes the formal parameters
genAssocElems :: [VHDLId] -> [VHDLId] -> [AssocElem]
genAssocElems formalNames actualNames = zipWith genAssoc formalNames actualNames

-- | Generate a port map from two lists of signal identifiers
--   The first list establishes the formal parameters
genPMap :: [VHDLId] -> [VHDLId] -> PMapAspect
genPMap formalIds actualIds = 
  PMapAspect $ genAssocElems formalIds actualIds


-- | Generate a function call from two lists of signal identifiers
--   The first list establishes the formal parameters
genFCall :: VHDLId -> [VHDLId] -> [VHDLId] -> FCall 
genFCall fName formalIds actualIds = 
  FCall (NSimple fName) $ zipWith genAssoc formalIds actualIds
  

-- Generate an association of a formal and actual parameter
genAssoc :: VHDLId -> VHDLId -> AssocElem
genAssoc formal actual = Just formal :=>: ADName (NSimple actual)