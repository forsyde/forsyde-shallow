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

-- | Generate a system design file for a system from the global system 
--   identifier,
--   local traversing results and the translated entity declaration
genSysDesignFile :: String -> EntityDec -> LocalTravResult -> DesignFile 
genSysDesignFile globalSysId ent@(EntityDec id _) (LocalTravResult decs stms) = 
   DesignFile contextClause [LUEntity ent, LUArch archBody]
 where archBody = ArchBody archId  (NSimple id) decs stms      
       archId = unsafeVHDLBasicId "synthesizable"
       libName = globalSysId ++ "_lib"
       libId = unsafeVHDLExtId libName
       contextClause = commonContextClause ++ 
                   [Library libId,
                    Use $ NSelected (NSimple libId :.: SSimple typesId) :.: All]

-- | Generate a library design file from the global results
genLibDesignFile :: GlobalTravResult -> DesignFile
genLibDesignFile  (GlobalTravResult typeDecs subtypeDecs subProgBodies) = 
   DesignFile commonContextClause [LUPackageDec packageDec, 
                                   LUPackageBody packageBody]
 where packageDec = PackageDec typesId (packageTypeDecs ++ 
                                        packageSubtypeDecs ++
                                        subProgSpecs)
       packageTypeDecs = map PDITD typeDecs
       packageSubtypeDecs = map PDISD subtypeDecs
       subProgSpecs = map (\(SubProgBody spec _ _) -> PDISS spec) subProgBodies
       packageBody = PackageBody typesId subProgBodies

  
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
  

-- | Generate a function call from the Function Name and a list of expressions
--   (its arguments)
genExprFCall :: VHDLId -> [Expr] -> Expr
genExprFCall fName args = 
   PrimFCall $ FCall (NSimple fName)  $
             map (\exp -> Nothing :=>: ADExpr exp) args


-- | Generate a function call from the Function Name (constant function)
genExprFCall0 :: VHDLId -> Expr
genExprFCall0 fName = genExprFCall fName []


-- | Generate a function call from the Function Name and an expression argument
genExprFCall1 :: VHDLId -> Expr -> Expr
genExprFCall1 fName arg = genExprFCall fName [arg]


-- | Generate a function call from the Function Name and four expression arguments
genExprFCall2 :: VHDLId -> Expr -> Expr -> Expr
genExprFCall2 fName arg1 arg2 = genExprFCall fName [arg1,arg2]


-- | Generate a function call from the Function Name and two expression arguments
genExprFCall4 :: VHDLId -> Expr -> Expr -> Expr -> Expr -> Expr
genExprFCall4 fName arg1 arg2 arg3 arg4 = 
 genExprFCall fName [arg1,arg2,arg2,arg3,arg4]


-- Generate an association of a formal and actual parameter
genAssoc :: VHDLId -> VHDLId -> AssocElem
genAssoc formal actual = Just formal :=>: ADName (NSimple actual)


-- | Generate the default functions for an unconstrained custom vector type
genVectorFuns :: TypeMark -- ^ type of the vector elements
             -> TypeMark -- ^ type of the vector
             -> TypeMark -- ^ type of the null vector
             -> [SubProgBody]
genVectorFuns elemTM vectorTM nullVectorTM = 
  [SubProgBody defaultSpec []              [defaultExpr]  ,
   SubProgBody exSpec      []              [exExpr]       ,
   SubProgBody selSpec     [SPVD selVar]   [selFor,selRet],
   SubProgBody emptySpec   [SPVD emptyVar] [emptyExpr]    ]
 where ixPar  = unsafeVHDLBasicId "ix"
       vecPar = unsafeVHDLBasicId "vec"
       fPar = unsafeVHDLBasicId "f"
       nPar = unsafeVHDLBasicId "n"
       sPar = unsafeVHDLBasicId "s"
       iId = unsafeVHDLBasicId "i"
       resId = unsafeVHDLBasicId "res"
       defaultSpec = Function defaultId [] vectorTM
       defaultExpr = 
          ReturnSm (Just $ Aggregate [ElemAssoc (Just Others) 
                                                (PrimName defaultSN)])
       exSpec = Function exId [IfaceVarDec vecPar vectorTM,
                               IfaceVarDec ixPar  naturalTM] elemTM
       exExpr = 
          ReturnSm (Just $ PrimName $ NIndexed 
                        (IndexedName (NSimple vecPar) [PrimName $ NSimple ixPar]))
       selSpec = Function selId [IfaceVarDec fPar   naturalTM,
                                 IfaceVarDec nPar   naturalTM,
                                 IfaceVarDec sPar   naturalTM,
                                 IfaceVarDec vecPar vectorTM ] vectorTM
       -- variable res : fsvec_x (0 to n-1);
       selVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit (show (0::Int)))
                               ((PrimName (NSimple nPar)) :-:
                                (PrimLit (show (1::Int))))   ]))
                Nothing
       -- for i in 0 to (n-1) loop
       --   res(i) := vec(f+i*s);
       -- end loop;
       selFor = ForSM iId (AttribRange $ applyRangeAttrib resId) [selAssign]
       -- res(i) := vec(f+i*s);
       selAssign = let origExp = PrimName (NSimple fPar) :+: 
                                   (PrimName (NSimple iId) :*: 
                                    PrimName (NSimple sPar)) in
         NIndexed (IndexedName (NSimple resId) [PrimName (NSimple iId)]) :=
         (PrimName $ NIndexed (IndexedName (NSimple vecPar) [origExp]))
       -- return res;
       selRet =  ReturnSm (Just $ PrimName (NSimple resId))
       emptySpec = Function emptyId [] nullVectorTM
       emptyVar = VarDec resId (SubtypeIn nullVectorTM Nothing) Nothing
       emptyExpr = ReturnSm (Just $ PrimName (NSimple resId))


-- | Generate the default functions for a custom tuple type
genTupleFuns :: [TypeMark] -- ^ type of each tuple element
             -> TypeMark -- ^ type of the tuple
             -> [SubProgBody]
genTupleFuns elemTMs tupleTM = 
  [SubProgBody defaultSpec [] [defaultExpr]]
 where defaultSpec = Function defaultId [] tupleTM
       defaultExpr = 
          ReturnSm (Just $ Aggregate (replicate tupSize 
                                       (ElemAssoc Nothing $ PrimName defaultSN)))
       tupSize = length elemTMs

-- | Generate the default functions for a custom abst_ext_ type
genAbstExtFuns :: TypeMark -- ^ type of the values nested in AbstExt
             -> TypeMark -- ^ type of the extended values
             -> [SubProgBody]
genAbstExtFuns elemTM absExtTM = 
  [SubProgBody defaultSpec           [] [defaultExpr],
   SubProgBody absentSpec            [] [absentExpr] ,
   SubProgBody presentSpec           [] [presentExpr],
   SubProgBody fromAbstExtSpec       [] [fromAbstExtExpr],
   SubProgBody unsafeFromAbstExtSpec [] [unsafeFromAbstExtExpr],
   SubProgBody isPresentSpec         [] [isPresentExpr],
   SubProgBody isAbsentSpec          [] [isAbsentExpr]]

 where defaultPar = unsafeVHDLBasicId "default"
       extPar = unsafeVHDLBasicId "extabst"
       defaultSpec = Function defaultId [] absExtTM
       defaultExpr = 
          ReturnSm (Just $ PrimName $ NSimple absentId)
       absentSpec = Function absentId [] absExtTM
       absentExpr = 
          ReturnSm (Just $ Aggregate 
                             [ElemAssoc Nothing falseExpr, 
                              ElemAssoc Nothing $ PrimName $ defaultSN ])
       presentSpec = 
          Function absentId [IfaceVarDec extPar elemTM] absExtTM
       presentExpr = 
          ReturnSm (Just $ Aggregate [ElemAssoc Nothing trueExpr, 
                                      ElemAssoc Nothing $ PrimName $ NSimple extPar ])
       fromAbstExtSpec = Function absentId [IfaceVarDec defaultPar elemTM,
                                            IfaceVarDec extPar     absExtTM] 
                                           elemTM
       fromAbstExtExpr = 
          IfSm (PrimName $ NSelected (NSimple extPar :.: SSimple isPresentId))
               [ReturnSm (Just $ PrimName $ 
                 (NSelected (NSimple extPar :.: SSimple valueId)))]
               []
               (Just $ Else
                 [ReturnSm (Just $ PrimName $ NSimple defaultPar)])
       unsafeFromAbstExtSpec = 
          Function absentId [IfaceVarDec extPar absExtTM] elemTM
       unsafeFromAbstExtExpr =
          ReturnSm (Just $ 
                    PrimName (NSelected (NSimple extPar :.: SSimple valueId)))
       isPresentSpec = 
          Function absentId [IfaceVarDec extPar absExtTM] booleanTM
       isPresentExpr =
           ReturnSm (Just $
                   PrimName (NSelected (NSimple extPar :.: SSimple isPresentId)))
       isAbsentSpec = 
          Function absentId [IfaceVarDec extPar absExtTM] booleanTM
       isAbsentExpr =
           ReturnSm (Just $
             Not $ PrimName (NSelected (NSimple extPar :.: SSimple isPresentId)))


       

-- | Generate the default functions for a custom enumeration type
genEnumAlgFuns :: TypeMark -- ^ enumeration type
             -> VHDLId -- ^ First enumeration literal of the type
             -> [SubProgBody]
genEnumAlgFuns enumTM firstLit = 
  [SubProgBody defaultSpec [] [defaultExpr]]
 where defaultSpec = Function defaultId [] enumTM
       defaultExpr = ReturnSm (Just $ PrimName (NSimple firstLit))
      


-- | Apply the range attribute  out of a simple name
applyRangeAttrib :: SimpleName -> AttribName
applyRangeAttrib sName = AttribName (NSimple sName) rangeId Nothing

