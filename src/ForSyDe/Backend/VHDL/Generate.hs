-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Generate
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
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
   NSimple dest :<==: (ConWforms [] (Wform [WformElem origExpr Nothing]) 
                       Nothing)  

-- | Generate a system design file for a system from the global system 
--   identifier,
--   local traversing results and the translated entity declaration
genSysDesignFile :: String -> EntityDec -> LocalTravResult -> DesignFile 
genSysDesignFile globalSysId ent@(EntityDec id _) (LocalTravResult decs stms) = 
   DesignFile contextClause [LUEntity ent, LUArch archBody]
 where archBody = ArchBody archId  (NSimple id) decs stms      
       archId = unsafeVHDLBasicId "synthesizable"
       libName = globalSysId ++ "_lib"
       libId = unsafeVHDLBasicId libName
       contextClause = commonContextClause ++ 
                   [Library libId,
                    Use $ NSelected (NSimple libId :.: SSimple typesId) :.: All]

-- | Generate a library design file from the global results
genLibDesignFile :: GlobalTravResult -> DesignFile
genLibDesignFile  (GlobalTravResult typeDecs subtypeDecs subProgBodies) = 
   DesignFile commonContextClause [LUPackageDec packageDec, 
                                   LUPackageBody packageBody]
 where packageDec = PackageDec typesId (packageSubtypeDecs ++
                                        packageTypeDecs ++ 
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

-- | version of genExprFCall which requires exactly n arguments
genExprFCallN :: VHDLId -> Int -> [Expr] -> Expr
genExprFCallN fName n args = genExprFCall fName (takeExactly n args)
 where takeExactly 0 [] = []
       takeExactly n (x:xs) | n > 0 = x : takeExactly (n-1) xs
       takeExactly _ _ = error "takeExactly: non exact length of input list"

-- | Generate a function call from the Function Name (constant function)
genExprFCall0 :: VHDLId -> Expr
genExprFCall0 fName = genExprFCall fName []

-- | List version of genExprFCall0
genExprFCall0L :: VHDLId -> [Expr] -> Expr
genExprFCall0L fName [] = genExprFCall fName []
genExprFCall0L _ _ = error "ForSyDe.Backend.VHDL.Generate.genExprFCall0L incorrect length"

-- | Generate a function call from the Function Name and an expression argument
genExprFCall1 :: VHDLId -> Expr -> Expr
genExprFCall1 fName arg = genExprFCall fName [arg]

-- | List version of genExprFCall1
genExprFCall1L :: VHDLId -> [Expr] -> Expr
genExprFCall1L fName [arg] = genExprFCall fName [arg]
genExprFCall1L _ _ = error "ForSyDe.Backend.VHDL.Generate.genExprFCall1L incorrect length"

-- | Generate a function call from the Function Name and two expression arguments
genExprFCall2 :: VHDLId -> Expr -> Expr -> Expr
genExprFCall2 fName arg1 arg2 = genExprFCall fName [arg1,arg2]

-- | List version of genExprFCall2
genExprFCall2L :: VHDLId -> [Expr] -> Expr
genExprFCall2L fName [arg1, arg2] = genExprFCall fName [arg1,arg2]
genExprFCall2L _ _ = error "ForSyDe.Backend.VHDL.Generate.genExprFCall2L incorrect length"

-- | Generate a function call from the Function Name and two expression arguments
genExprFCall4 :: VHDLId -> Expr -> Expr -> Expr -> Expr -> Expr
genExprFCall4 fName arg1 arg2 arg3 arg4 = 
 genExprFCall fName [arg1,arg2,arg2,arg3,arg4]


-- | List version of genExprFCall4
genExprFCall4L :: VHDLId -> [Expr] -> Expr
genExprFCall4L fName [arg1, arg2, arg3, arg4] = 
 genExprFCall fName [arg1,arg2,arg2,arg3,arg4]
genExprFCall4L _ _ = error "ForSyDe.Backend.VHDL.Generate.genExprFCall4L incorrect length"

-- | Generate a procedure call from the Function Name and a list of expressions
--   (its arguments)
genExprProcCall :: VHDLId -> [Expr] -> SeqSm
genExprProcCall pName args = ProcCall (NSimple pName)  $
             map (\exp -> Nothing :=>: ADExpr exp) args


-- | Generate a procedure call from the Function Name (constant procedure)
genExprProcCall0 :: VHDLId -> SeqSm
genExprProcCall0 fName = genExprProcCall fName []


-- | Generate a procedure call from the Function Name and an expression argument
genExprProcCall1 :: VHDLId -> Expr -> SeqSm
genExprProcCall1 pName arg = genExprProcCall pName [arg]


-- | Generate a procedure call from the Function Name and four expression 
--   arguments
genExprProcCall2 :: VHDLId -> Expr -> Expr -> SeqSm
genExprProcCall2 pName arg1 arg2 = genExprProcCall pName [arg1,arg2]


-- | Generate a procedure call from the Function Name and two expression 
--   arguments
genExprProcCall4 :: VHDLId -> Expr -> Expr -> Expr -> Expr -> SeqSm
genExprProcCall4 pName arg1 arg2 arg3 arg4 = 
 genExprProcCall pName [arg1,arg2,arg2,arg3,arg4]


-- Generate an association of a formal and actual parameter
genAssoc :: VHDLId -> VHDLId -> AssocElem
genAssoc formal actual = Just formal :=>: ADName (NSimple actual)


-- | Generate the default functions for an unconstrained custom vector type
genUnconsVectorFuns :: TypeMark -- ^ type of the vector elements
                    -> TypeMark -- ^ type of the vector
                    -> [SubProgBody]
genUnconsVectorFuns elemTM vectorTM  = 
  [SubProgBody exSpec        []                  [exExpr]                    ,
   SubProgBody selSpec       [SPVD selVar]       [selFor, selRet]            ,
   SubProgBody emptySpec     [SPVD emptyVar]     [emptyExpr]                 ,
   SubProgBody lengthSpec    []                  [lengthExpr]                ,
   SubProgBody isnullSpec    []                  [isnullExpr]                ,
   SubProgBody replaceSpec   [SPVD replaceVar]   [replaceExpr, replaceRet]   ,
   SubProgBody headSpec      []                  [headExpr]                  ,
   SubProgBody lastSpec      []                  [lastExpr]                  ,
   SubProgBody initSpec      [SPVD initVar]      [initExpr, initRet]         ,
   SubProgBody tailSpec      [SPVD tailVar]      [tailExpr, tailRet]         ,
   SubProgBody takeSpec      [SPVD takeVar]      [takeExpr, takeRet]         ,
   SubProgBody dropSpec      [SPVD dropVar]      [dropExpr, dropRet]         ,
   SubProgBody shiftlSpec    [SPVD shiftlVar]    [shiftlExpr, shiftlRet]     ,
   SubProgBody shiftrSpec    [SPVD shiftrVar]    [shiftrExpr, shiftrRet]     ,
   SubProgBody rotlSpec      [SPVD rotlVar]      [rotlExpr, rotlRet]         ,
   SubProgBody rotrSpec      [SPVD rotrVar]      [rotrExpr, rotrRet]         ,
   SubProgBody reverseSpec   [SPVD reverseVar]   [reverseFor, reverseRet]    ,
   SubProgBody copySpec      [SPVD copyVar]      [copyExpr]                  ,
   SubProgBody plusgtSpec    [SPVD plusgtVar]    [plusgtExpr, plusgtRet]     ,
   SubProgBody ltplusSpec    [SPVD ltplusVar]    [ltplusExpr, ltplusRet]     ,
   SubProgBody plusplusSpec  [SPVD plusplusVar]  [plusplusExpr, plusplusRet] ,
   SubProgBody singletonSpec [SPVD singletonVar] [singletonRet]              ,
   SubProgBody showSpec      [SPSB doShowDef]    [showRet]                   ,
   SubProgBody defaultSpec   []                  [defaultExpr]               ]
 where ixPar = unsafeVHDLBasicId "ix"
       vecPar = unsafeVHDLBasicId "vec"
       vec1Par = unsafeVHDLBasicId "vec1"
       vec2Par = unsafeVHDLBasicId "vec2"
       fPar = unsafeVHDLBasicId "f"
       nPar = unsafeVHDLBasicId "n"
       sPar = unsafeVHDLBasicId "s"
       iId = unsafeVHDLBasicId "i"
       iPar = iId
       aPar = unsafeVHDLBasicId "a"
       resId = unsafeVHDLBasicId "res"
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
                   [ToRange (PrimLit "0")
                               ((PrimName (NSimple nPar)) :-:
                                (PrimLit "1"))   ]))
                Nothing
       -- for i res'range loop
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
       emptySpec = Function emptyId [] vectorTM
       emptyVar = 
            VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimLit "-1")]))
                Nothing
       emptyExpr = ReturnSm (Just $ PrimName (NSimple resId))
       lengthSpec = Function lengthId [IfaceVarDec vecPar vectorTM] naturalTM
       lengthExpr = ReturnSm (Just $ PrimName (NAttribute $ 
                                AttribName (NSimple vecPar) lengthId Nothing))
       isnullSpec = Function isnullId [IfaceVarDec vecPar vectorTM] booleanTM
       -- return vec'length = 0
       isnullExpr = ReturnSm (Just $ 
                        PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :=:
                        PrimLit "0")
       replaceSpec = Function replaceId [IfaceVarDec vecPar vectorTM,
                                         IfaceVarDec iPar   naturalTM,
                                         IfaceVarDec aPar   elemTM   ] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-1);
       replaceVar =
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                                (PrimLit "1"))   ]))
                Nothing
       --  res := vec(0 to i-1) & a & vec(i+1 to length'vec-1)
       replaceExpr = NSimple resId :=
           (vecSlice (PrimLit "0") (PrimName (NSimple iPar) :-: PrimLit "1") :&:
            PrimName (NSimple aPar) :&: 
             vecSlice (PrimName (NSimple iPar) :+: PrimLit "1")
                      ((PrimName (NAttribute $ 
                                AttribName (NSimple vecPar) lengthId Nothing)) 
                                                              :-: PrimLit "1"))
       replaceRet =  ReturnSm (Just $ PrimName $ NSimple resId)
       vecSlice init last =  PrimName (NSlice 
                                        (SliceName 
                                              (NSimple vecPar) 
                                              (ToRange init last)))
       headSpec = Function headId [IfaceVarDec vecPar vectorTM] elemTM
       -- return vec(0);
       headExpr = ReturnSm (Just $ (PrimName $ NIndexed (IndexedName 
                    (NSimple vecPar) [PrimLit "0"])))
       lastSpec = Function lastId [IfaceVarDec vecPar vectorTM] elemTM
       -- return vec(vec'length-1);
       lastExpr = ReturnSm (Just $ (PrimName $ NIndexed (IndexedName 
                    (NSimple vecPar) 
                    [PrimName (NAttribute $ 
                                AttribName (NSimple vecPar) lengthId Nothing) 
                                                             :-: PrimLit "1"])))
       initSpec = Function initId [IfaceVarDec vecPar vectorTM] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-2);
       initVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                                (PrimLit "2"))   ]))
                Nothing
       -- res:= vec(0 to vec'length-2)
       initExpr = NSimple resId := (vecSlice 
                               (PrimLit "0") 
                               (PrimName (NAttribute $ 
                                  AttribName (NSimple vecPar) lengthId Nothing) 
                                                             :-: PrimLit "2"))
       initRet =  ReturnSm (Just $ PrimName $ NSimple resId)
       tailSpec = Function tailId [IfaceVarDec vecPar vectorTM] vectorTM
       -- variable res : fsvec_x (0 to vec'length-2); 
       tailVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                                (PrimLit "2"))   ]))
                Nothing       
       -- res := vec(1 to vec'length-1)
       tailExpr = NSimple resId := (vecSlice 
                               (PrimLit "1") 
                               (PrimName (NAttribute $ 
                                  AttribName (NSimple vecPar) lengthId Nothing) 
                                                             :-: PrimLit "1"))
       tailRet = ReturnSm (Just $ PrimName $ NSimple resId)
       takeSpec = Function takeId [IfaceVarDec nPar   naturalTM,
                                   IfaceVarDec vecPar vectorTM ] vectorTM
       -- variable res : fsvec_x (0 to n-1);
       takeVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                               ((PrimName (NSimple nPar)) :-:
                                (PrimLit "1"))   ]))
                Nothing
       -- res := vec(0 to n-1)
       takeExpr = NSimple resId := 
                    (vecSlice (PrimLit "1") 
                              (PrimName (NSimple $ nPar) :-: PrimLit "1"))
       takeRet =  ReturnSm (Just $ PrimName $ NSimple resId)
       dropSpec = Function dropId [IfaceVarDec nPar   naturalTM,
                                   IfaceVarDec vecPar vectorTM ] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-n-1);
       dropVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimName $ NSimple nPar):-: (PrimLit "1")) ]))
               Nothing
       -- res := vec(n to vec'length-1)
       dropExpr = NSimple resId := (vecSlice 
                               (PrimName $ NSimple nPar) 
                               (PrimName (NAttribute $ 
                                  AttribName (NSimple vecPar) lengthId Nothing) 
                                                             :-: PrimLit "1"))
       dropRet =  ReturnSm (Just $ PrimName $ NSimple resId)
       shiftlSpec = Function shiftlId [IfaceVarDec vecPar vectorTM,
                                       IfaceVarDec aPar   elemTM  ] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-1);
       shiftlVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimLit "1")) ]))
                Nothing
       -- res := a & init(vec)
       shiftlExpr = NSimple resId :=
                      (PrimName (NSimple aPar) :&:
                       genExprFCall1 initId (PrimName $ NSimple vecPar))
       shiftlRet =  ReturnSm (Just $ PrimName $ NSimple resId)       
       shiftrSpec = Function shiftrId [IfaceVarDec vecPar vectorTM,
                                       IfaceVarDec aPar   elemTM  ] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-1);
       shiftrVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimLit "1")) ]))
                Nothing
       -- res := tail(vec) & a
       shiftrExpr = NSimple resId :=
                      (genExprFCall1 tailId (PrimName $ NSimple vecPar) :&:
                       PrimName (NSimple aPar))
       shiftrRet =  ReturnSm (Just $ PrimName $ NSimple resId)       
       rotlSpec = Function rotlId [IfaceVarDec vecPar vectorTM] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-1);
       rotlVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimLit "1")) ]))
                Nothing
       -- if null(vec) then res := vec else res := last(vec) & init(vec)
       rotlExpr = IfSm (genExprFCall1 isnullId (PrimName $ NSimple vecPar)) 
                       [NSimple resId := (PrimName $ NSimple vecPar)]
                       []
                       (Just $ Else [rotlExprRet])
        where rotlExprRet = 
                  NSimple resId := 
                          (genExprFCall1 lastId (PrimName $ NSimple vecPar) :&:
                           genExprFCall1 initId (PrimName $ NSimple vecPar))
       rotlRet =  ReturnSm (Just $ PrimName $ NSimple resId)       
       rotrSpec = Function rotrId [IfaceVarDec vecPar vectorTM] vectorTM 
       -- variable res : fsvec_x (0 to vec'length-1);
       rotrVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimLit "1")) ]))
                Nothing
       -- if null(vec) then res := vec else res := tail(vec) & head(vec)
       rotrExpr = IfSm (genExprFCall1 isnullId (PrimName $ NSimple vecPar)) 
                       [NSimple resId := (PrimName $ NSimple vecPar)]
                       []
                       (Just $ Else [rotrExprRet])  
        where rotrExprRet = 
                  NSimple resId := 
                      (genExprFCall1 lastId (PrimName $ NSimple vecPar) :&:
                       genExprFCall1 initId (PrimName $ NSimple vecPar))
       rotrRet =  ReturnSm (Just $ PrimName $ NSimple resId)       
       reverseSpec = Function reverseId [IfaceVarDec vecPar vectorTM] vectorTM
       reverseVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing) :-:
                               (PrimLit "1")) ]))
                Nothing
       -- for i in 0 to res'range loop
       --   res(vec'length-i-1) := vec(i);
       -- end loop;
       reverseFor = 
          ForSM iId (AttribRange $ applyRangeAttrib resId) [reverseAssign]
       -- res(vec'length-i-1) := vec(i);
       reverseAssign = NIndexed (IndexedName (NSimple resId) [destExp]) :=
         (PrimName $ NIndexed (IndexedName (NSimple vecPar) 
                              [PrimName $ NSimple iId]))
           where destExp = PrimName (NAttribute $ AttribName (NSimple vecPar) 
                                      lengthId Nothing) :-: 
                           PrimName (NSimple iId) :-: 
                           (PrimLit "1") 
       -- return res;
       reverseRet =  ReturnSm (Just $ PrimName (NSimple resId))
       copySpec = Function copyId [IfaceVarDec nPar   naturalTM,
                                      IfaceVarDec aPar   elemTM   ] vectorTM 
       -- variable res : fsvec_x (0 to n-1) := (others => a);
       copyVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                               ((PrimName (NSimple nPar)) :-:
                                (PrimLit "1"))   ]))
                (Just $ Aggregate [ElemAssoc (Just Others) 
                                             (PrimName $ NSimple aPar)])
       -- return res
       copyExpr = ReturnSm (Just $ PrimName $ NSimple resId)
       plusgtSpec = Function plusgtId [IfaceVarDec aPar   elemTM,
                                       IfaceVarDec vecPar vectorTM] vectorTM 
       -- variable res : fsvec_x (0 to vec'length);
       plusgtVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing))]))
                Nothing
       plusgtExpr = NSimple resId := 
                       ((PrimName $ NSimple aPar) :&: 
                        (PrimName $ NSimple vecPar))
       plusgtRet = ReturnSm (Just $ PrimName $ NSimple resId)
       ltplusSpec = Function ltplusId [IfaceVarDec vecPar vectorTM,
                                       IfaceVarDec aPar   elemTM] vectorTM 
       -- variable res : fsvec_x (0 to vec'length);
       ltplusVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing))]))
                Nothing
       ltplusExpr = NSimple resId := 
                       ((PrimName $ NSimple vecPar) :&: 
                        (PrimName $ NSimple aPar))
       ltplusRet = ReturnSm (Just $ PrimName $ NSimple resId)
       plusplusSpec = Function plusplusId [IfaceVarDec vec1Par vectorTM,
                                           IfaceVarDec vec2Par vectorTM  ] 
                               vectorTM 
       -- variable res : fsvec_x (0 to vec1'length + vec2'length -1);
       plusplusVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0")
                            (PrimName (NAttribute $ 
                              AttribName (NSimple vec1Par) lengthId Nothing) :+:
                             PrimName (NAttribute $ 
                              AttribName (NSimple vec2Par) lengthId Nothing) :-:
                             PrimLit "1")]))
                Nothing
       plusplusExpr = NSimple resId := 
                       ((PrimName $ NSimple vec1Par) :&: 
                        (PrimName $ NSimple vec2Par))
       plusplusRet = ReturnSm (Just $ PrimName $ NSimple resId)
       singletonSpec = Function singletonId [IfaceVarDec aPar elemTM ] 
                                            vectorTM
       -- variable res : fsvec_x (0 to 0) := (others => a);
       singletonVar = 
         VarDec resId 
                (SubtypeIn vectorTM
                  (Just $ IndexConstraint 
                   [ToRange (PrimLit "0") (PrimLit "0")]))
                (Just $ Aggregate [ElemAssoc (Just Others) 
                                             (PrimName $ NSimple aPar)])
       singletonRet = ReturnSm (Just $ PrimName $ NSimple resId)
       showSpec  = Function showId [IfaceVarDec vecPar vectorTM] stringTM
       doShowId  = unsafeVHDLBasicId "doshow"
       doShowDef = SubProgBody doShowSpec [] [doShowRet]
          where doShowSpec = Function doShowId [IfaceVarDec vecPar vectorTM] 
                                               stringTM
                -- case vec'len is
                --  when  0 => return "";
                --  when  1 => return head(vec);
                --  when others => return show(head(vec)) & ',' &
                --                        doshow (tail(vec));
                -- end case;
                doShowRet = 
                  CaseSm (PrimName (NAttribute $ 
                              AttribName (NSimple vecPar) lengthId Nothing))
                  [CaseSmAlt [ChoiceE $ PrimLit "0"] 
                             [ReturnSm (Just $ PrimLit "\"\"")],
                   CaseSmAlt [ChoiceE $ PrimLit "1"] 
                             [ReturnSm (Just $ 
                              genExprFCall1 showId 
                                   (genExprFCall1 headId (PrimName $ NSimple vecPar)) )],
                   CaseSmAlt [Others] 
                             [ReturnSm (Just $ 
                               genExprFCall1 showId 
                                 (genExprFCall1 headId (PrimName $ NSimple vecPar)) :&:
                               PrimLit "','" :&:
                               genExprFCall1 doShowId 
                                 (genExprFCall1 tailId (PrimName $ NSimple vecPar)) ) ]]
       -- return '<' & doshow(vec) & '>';
       showRet =  ReturnSm (Just $ PrimLit "'<'" :&:
                                   genExprFCall1 doShowId (PrimName $ NSimple vecPar) :&:
                                   PrimLit "'>'" )
       
       defaultSpec = Function defaultId [] vectorTM
       defaultExpr = 
          ReturnSm (Just $ genExprFCall0 emptyId)
    
                
-- | Generate the default functions for a custom tuple type
genTupleFuns :: [TypeMark] -- ^ type of each tuple element
             -> TypeMark -- ^ type of the tuple
             -> [SubProgBody]
genTupleFuns elemTMs tupleTM = 
  [SubProgBody defaultSpec [] [defaultExpr],
   SubProgBody showSpec    [] [showExpr]]
 where tupPar = unsafeVHDLBasicId "tup" 
       defaultSpec = Function defaultId [] tupleTM
       defaultExpr = 
          ReturnSm (Just $ Aggregate (replicate tupSize 
                                       (ElemAssoc Nothing $ PrimName defaultSN)))
       showSpec = Function showId [IfaceVarDec tupPar tupleTM ] stringTM
       -- return '(' & show(tup.
       showExpr = ReturnSm (Just $
                      PrimLit "'('" :&: showMiddle :&: PrimLit "')'")
         where showMiddle = foldr1 (\e1 e2 -> e1 :&: PrimLit "','" :&: e2) $ 
                  map ((genExprFCall1 showId).
                       PrimName . 
                       NSelected.
                       (NSimple tupPar:.:).
                       tupVHDLSuffix) 
                      [1..tupSize]
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
   SubProgBody isAbsentSpec          [] [isAbsentExpr],
   SubProgBody showSpec              [] [showExpr]             ]
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
       showSpec = Function showId [IfaceVarDec extPar absExtTM ] stringTM
       -- if extabs.isPresent 
       --    return "Prst " & show(extabs.value);
       -- else
       --    return "Abst";
       -- end if;  
       showExpr = 
          IfSm (PrimName $ NSelected (NSimple extPar :.: SSimple isPresentId))
               [ReturnSm (Just $ PrimLit "\"Prst \"" :&:
                  genExprFCall1 showId (PrimName $ 
                 (NSelected (NSimple extPar :.: SSimple valueId))))]
               []
               (Just $ Else
                 [ReturnSm (Just $ PrimLit "\"Abst\"")])

       

-- | Generate the default functions for a custom enumeration type
genEnumAlgFuns :: TypeMark -- ^ enumeration type
             -> VHDLId -- ^ First enumeration literal of the type
             -> [SubProgBody]
genEnumAlgFuns enumTM firstLit = 
  [SubProgBody defaultSpec [] [defaultExpr],
   SubProgBody showSpec [] [showExpr]]
 where enumPar = unsafeVHDLBasicId "enum"
       defaultSpec = Function defaultId [] enumTM
       defaultExpr = ReturnSm (Just $ PrimName (NSimple firstLit))
       showSpec = Function showId [IfaceVarDec enumPar enumTM] stringTM
       -- we slice the resulting image in order to eliminate the
       -- leading and trailing slashes
       -- 
       -- return enumTM'image(enum)(2 to enumTM'image(enum)'length-1);
       showExpr = ReturnSm (Just $ PrimName $ NSlice $ SliceName image
                  (ToRange (PrimLit "2")
                           ((PrimName $ NAttribute $
                             AttribName image lengthId Nothing) :-:
                             PrimLit "1")))
        where image = NAttribute$ AttribName (NSimple enumTM) imageId 
                                  (Just $ PrimName $ NSimple enumPar)

-- | Apply the range attribute  out of a simple name
applyRangeAttrib :: SimpleName -> AttribName
applyRangeAttrib sName = AttribName (NSimple sName) rangeId Nothing

