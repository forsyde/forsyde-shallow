{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Traverse
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to translate elements of the intermediate system 
-- representation to elements of the VHDL AST.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Translate where


import ForSyDe.Backend.VHDL.AST
import qualified ForSyDe.Backend.VHDL.AST as VHDL
import ForSyDe.Backend.VHDL.Constants
import ForSyDe.Backend.VHDL.Generate
import ForSyDe.Backend.VHDL.Traverse.VHDLM

import ForSyDe.Ids
import ForSyDe.AbsentExt
import ForSyDe.Signal
import ForSyDe.Bit hiding (not)
import qualified ForSyDe.Bit as B
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import ForSyDe.Process.ProcFun
import ForSyDe.Process.ProcVal
import ForSyDe.Process.ProcType

import Data.Typeable.TypeRepLib (unArrowT)
import Language.Haskell.TH.TypeLib (type2TypeRep)

import Data.Generics (tyconUQname)
import Data.Int
import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.Maybe (isJust, fromJust)
import Control.Monad.State
import qualified Data.Set as S
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (global)
import qualified Data.Traversable as DT
import Data.Typeable
import qualified Data.Param.FSVec as V
import Text.Regex.Posix ((=~))

import Data.TypeLevel.Num.Reps

-- | Translate a System Definition to an Entity, explicitly returning
--   the VHDL identifiers of its output signals.
transSysDef2Ent :: SysLogic -- ^ logic of the system 
                -> SysDefVal -- ^ system to translate 
                -> VHDLM EntityDec
transSysDef2Ent logic sysDefVal = do
 entId <- transSysId2VHDL (sid sysDefVal)
 inDecs  <- mapM (uncurry $ transPort2IfaceSigDec In)  (iIface sysDefVal) 
 outDecs <- mapM (uncurry $ transPort2IfaceSigDec Out) (oIface sysDefVal)
 -- clock and reset implicit declarations
 let implicitDecs = if logic == Sequential then 
                     [IfaceSigDec resetId In std_logicTM, 
                      IfaceSigDec clockId In std_logicTM]
                     else []
 return $ EntityDec entId (implicitDecs ++ inDecs ++ outDecs) 
 
-- | Translate a 'ZipwithNSY' process to a block returning a declaration of
--   the resulting signal.
transZipWithN2Block :: Label -- ^ process identifier
                    -> [VHDLId] -- ^ input signals
                    -> Loc -- ^ location of the inner function
                    -> TypedProcFunAST -- ^ AST of the inner function
                    -> VHDLId -- ^ output signal
                    -> VHDLM (BlockSm, SigDec)
transZipWithN2Block vPid ins loc ast out = do
 -- Translate the process function
 (f,fName , inFPars, inFTypes, retFType) <- 
        withProcFunC ((name.tpast) ast) loc $ transProcFun2VHDL ast 
 -- Generate the formal parameters of the block
 let inPars = map (\n -> unsafeIdAppend vPid ("_in" ++ show n)) [1..length ins]
     outPar = unsafeIdAppend vPid "_out"
 -- Generate the port interface of the block     
     inDecs = zipWith (\par typ -> IfaceSigDec par In typ) inPars inFTypes
     outDec = IfaceSigDec outPar Out retFType
     iface = inDecs ++ [outDec]
 -- Generate the port map
     pMap = genPMap  (inPars ++ [outPar]) (ins ++ [out])
 -- Generate the function call and signal assignment
     outAssign = genFCallAssign out fName inFPars ins
 return  (BlockSm vPid iface pMap [BDISPB f] [CSSASm outAssign],
          SigDec out retFType Nothing)

 
-- | Translate a 'ZipwithxSY' process to a block returning a declaration of
--   the resulting signal.
transZipWithx2Block :: Label -- ^ process identifier
                    -> [VHDLId] -- ^ input signals
                    -> Loc -- ^ location of the inner function
                    -> TypedProcFunAST -- ^ AST of the inner function
                    -> VHDLId -- ^ output signal
                    -> VHDLM (BlockSm, SigDec)
transZipWithx2Block vPid ins loc ast out = do
 -- Translate the process function
 (f, fName, [inFPar], [inFType], retFType) <- 
        withProcFunC ((name.tpast) ast) loc $ transProcFun2VHDL ast
 -- Figure out the type of the inputs from the 
 -- function's input vector type (horrible hack, but it works)
 let [[_,suffix]] = (fromVHDLId inFType) =~ "^fsvec_[0-9]*_(.*)$" 
     inType = unsafeVHDLBasicId $ suffix
 -- Generate the formal parameters of the block
     inPars = map (\n -> unsafeIdAppend vPid ("_in" ++ show n)) [1..length ins]
     outPar = unsafeIdAppend vPid "_out"
 -- Generate the port interface of the block     
     inDecs = map (\par -> IfaceSigDec par In inType) inPars
     outDec = IfaceSigDec outPar Out retFType
     iface = inDecs ++ [outDec]
 -- Generate the port map
     pMap = genPMap  (inPars ++ [outPar]) (ins ++ [out])
 -- Generate the function call and signal assignment
     aggregate = Aggregate $
                  map (\e -> ElemAssoc Nothing (PrimName(NSimple e))) inPars 
     fCall = PrimFCall $ FCall (NSimple fName) 
                               [Just inFPar :=>: ADExpr aggregate]
     outAssign = genExprAssign outPar fCall
 return  (BlockSm vPid iface pMap [BDISPB f] [CSSASm outAssign],
          SigDec out retFType Nothing)
  
-- | Translate a 'UnzipNSY' process to a block returning a declaration of
--   the resulting signal.
transUnzipNSY2Block :: Label -- ^ process identifier
                    -> VHDLId -- ^ input signal
                    -> [VHDLId] -- ^ output signals
                    -> [TypeRep] -- ^ output signal types
                    -> VHDLM (BlockSm, [SigDec])
transUnzipNSY2Block vPid inSig outSigs outTRTypes = do
 -- Generate the formal parameters of the block
 let inPar = unsafeIdAppend vPid "_in"
     outPars = map (\n -> unsafeIdAppend vPid ("_out" ++ show n)) 
                   [1..length outSigs]
 -- Generate the port interface of the block
     nOuts = length outSigs
     tupTyCon = mkTyCon $ replicate (nOuts-1) ','
     inTRType = tupTyCon `mkTyConApp` outTRTypes    
 outTMTypes <- mapM transTR2TM outTRTypes
 inTMType <- transTR2TM inTRType
 let inDec = IfaceSigDec inPar In inTMType
     outDecs = zipWith (\par typ -> IfaceSigDec par Out typ) outPars outTMTypes
     iface = inDec : outDecs
 -- Generate the port map
     pMap = genPMap  (inPar : outPars) (inSig : outSigs)
 -- Generate the signal assignments
     genOrigExp n = (PrimName $ NSelected 
                              (NSimple inPar :.: tupVHDLSuffix n))
     genOutAssign outSig n = CSSASm $ genExprAssign outSig (genOrigExp n)
     outAssigns = zipWith genOutAssign outPars [(1::Int)..]              
 return  (BlockSm vPid iface pMap [] outAssigns,
          zipWith (\sig typ -> SigDec sig typ Nothing) outSigs outTMTypes)



-- | Translate a 'UnzipxSY' process to a block returning a declaration of
--   the resulting signal.
transUnzipxSY2Block :: Label -- ^ process identifier
                    -> VHDLId -- ^ input signal
                    -> [VHDLId] -- ^ output signals
                    -> TypeRep -- ^ type of vector elements
                    -> Int -- ^ vector Size
                    -> VHDLM (BlockSm, [SigDec])
transUnzipxSY2Block vPid inSig outSigs elemTR vSize = do 
 -- Generate the formal parameters of the block
 let inPar = unsafeIdAppend vPid "_in"
     outPars = map (\n -> unsafeIdAppend vPid ("_out" ++ show n)) 
                   [1..length outSigs]
 -- Generate the port interface of the block
     inTRType = fSVecTyCon `mkTyConApp` [transInt2TLNat vSize, elemTR]    
 inTMType <- transTR2TM inTRType
 elemTM <- transTR2TM elemTR
 let inDec = IfaceSigDec inPar In inTMType
     outDecs = map (\par -> IfaceSigDec par Out elemTM) outPars 
     iface = inDec : outDecs
 -- Generate the port map
     pMap = genPMap  (inPar : outPars) (inSig : outSigs)
 -- Generate the signal assignments
     genOrigExp n = 
        PrimName $ NIndexed (NSimple inPar `IndexedName` [PrimLit  $ show n])
     genOutAssign outSig n = CSSASm $ genExprAssign outSig (genOrigExp n)
     outAssigns = zipWith genOutAssign outPars [(0::Int)..]              
 return  (BlockSm vPid iface pMap [] outAssigns,
          map (\sig -> SigDec sig elemTM Nothing) outSigs)




-- | Translate a 'DelaySY' process to a block returning a declaration of
--   the resulting signal.
transDelay2Block ::  Label -- ^ process identifier
                  -> VHDLId -- ^ input signal
                  -> ProcValAST -- ^ AST of the initial value 
                                -- of the delay process
                  -> VHDLId   -- ^ output signal
                  -> VHDLM (BlockSm, SigDec)
transDelay2Block vPid inS (ProcValAST exp tr enums) outS = do
 -- Add the enumerated types associated with the value to the global results
 addEnumTypes enums
 -- Get the type of the initial value
 initTR <- transTR2TM tr
 -- Translate the initial value
 initExp <- withProcValC exp $ withInitFunTransST $ (transExp2VHDL exp)
 -- Build the block
 let formalIn  = unsafeIdAppend vPid "_in"
     formalOut = unsafeIdAppend vPid "_out"
     iface = [IfaceSigDec resetId   In  std_logicTM,
              IfaceSigDec clockId   In  std_logicTM, 
              IfaceSigDec formalIn  In  initTR,
              IfaceSigDec formalOut Out initTR] 
     assocs = [Just resetId   :=>: ADName (NSimple resetId),
               Just clockId   :=>: ADName (NSimple clockId),
               Just formalIn  :=>: ADName (NSimple inS),
               Just formalOut :=>: ADName (NSimple outS)]
     sigAssign = CSSASm (NSimple formalOut :<==: 
                           (ConWforms [whenElseReset] inWform (Just whenRE)))
     whenElseReset = WhenElse (Wform [WformElem initExp Nothing])
                               (PrimName (NSimple resetId) :=: PrimLit "'0'")
     inWform = Wform [WformElem (PrimName $ NSimple formalIn) Nothing]
     whenRE = When (PrimFCall $ FCall (NSimple $ unsafeVHDLBasicId "rising_edge") 
                                      [Nothing :=>: ADName (NSimple clockId) ])
 return  (BlockSm vPid iface (PMapAspect assocs) [] [sigAssign],
          SigDec outS initTR Nothing)

-- | Translate a System instance into a VHDL component instantion
--   returning the declartion of the output signals
transSysIns2CompIns :: SysLogic -- ^ parent system logic
                    -> Label -- ^ instance identifier
                    -> [VHDLId] -- ^ input signals
                    -> [(VHDLId, TypeRep)] -- ^ output signals
                    -> SysId -- ^ parent system identifier
                    -> [PortId] -- ^ parent input identifiers
                    -> [PortId] -- ^ parent output identifiers
                    -> VHDLM (Maybe CompInsSm, [SigDec])
transSysIns2CompIns logic vPid ins typedOuts parentId parentInIds parentOutIds = do
 if length ins == 0 && length typedOuts == 0 
  then return (Nothing, []) 
  else do
   -- Create the declarations for the signals
   decs <- mapM (\(name,typ) -> transVHDLName2SigDec name typ Nothing) typedOuts
   -- Create the portmap 
   vParentId <- transSysId2VHDL parentId
   vParentInIds <- liftEProne $ mapM mkVHDLExtId parentInIds
   vParentOutIds <- liftEProne $ mapM mkVHDLExtId parentOutIds
   let implicitAssocIds = if logic == Sequential then [resetId, clockId] else []
       assocs =  genAssocElems 
                   (implicitAssocIds ++ vParentInIds ++ vParentOutIds)
                   (implicitAssocIds ++ ins          ++ map fst typedOuts)
       entityName = NSelected (NSimple workId :.: SSimple vParentId)
       instantiation = CompInsSm vPid (IUEntity entityName) (PMapAspect assocs)
   return (Just instantiation, decs)


-- | Translate a VHDL Signal to a VHDL Signal declaration
transVHDLName2SigDec ::  SimpleName -- ^ Signal name 
             -> TypeRep -- ^ Type of the intermediate signal 
             -> Maybe TH.Exp -- ^ Maybe an initializer expression for the signal
             -> VHDLM SigDec
transVHDLName2SigDec vId tr mExp = do
 tm <- transTR2TM tr
 mVExp <- DT.mapM (\e -> withInitFunTransST (transExp2VHDL e)) mExp
 return $ SigDec vId tm mVExp



-------------------------
-- Identifier translation
-------------------------


-- | Translate a VHDL identifier and a type to an interface signal declaration
transVHDLId2IfaceSigDec :: Mode -> VHDLId -> TypeRep -> VHDLM IfaceSigDec
transVHDLId2IfaceSigDec m vid trep = do           
 tm  <- transTR2TM trep
 return $ IfaceSigDec vid m tm
 
 
-- | Translate a Port to a VHDL Interface signal declaration
transPort2IfaceSigDec :: Mode -> PortId -> TypeRep -> VHDLM IfaceSigDec
transPort2IfaceSigDec m pid trep = do           
 sid <- transPortId2VHDL pid
 transVHDLId2IfaceSigDec m sid trep

-- | Translate a local TH name to a VHDL Identifier
transTHName2VHDL :: TH.Name -> VHDLM VHDLId 
-- we use pprint becase it shows unique names for local names
-- e.g. let x = 1 in let x =2 in x is printed as
--      let x_0 = 1 in let x_1 = 2 in x_1
-- we want unique names because it saves us from dealing wiht
-- name scopes and having a global name table.
transTHName2VHDL = transPortId2VHDL . tyconUQname  . pprint

-- | Translate a system identifier to a VHDL identifier
transSysId2VHDL :: SysId -> VHDLM VHDLId
transSysId2VHDL = transPortId2VHDL

-- | Translate a process identifier to a VHDL identifier
transProcId2VHDL :: ProcId -> VHDLM VHDLId
transProcId2VHDL = transPortId2VHDL

-- | translate a port identifier to a VHDL Identifier
transPortId2VHDL :: PortId -> VHDLM VHDLId
transPortId2VHDL str = liftEProne $ mkVHDLExtId str


-------------------
-- Type translation 
-------------------

-- | translate a 'TypeRep' to a VHDL 'TypeMark'
-- We don't distinguish between a type and its version nested in 'Signal'
-- since it makes no difference in VHDL
transTR2TM :: TypeRep -> VHDLM TypeMark
transTR2TM rep 
 -- Is it a Signal?
 | isSignal = transTR2TM  nestedTR
 -- Is it a primitive type?
 | isJust mPrimitiveTM = return $ fromJust mPrimitiveTM
 -- Non-Primitive type, try to translate it
 | otherwise = customTR2TM rep
 where (isSignal, nestedTR) = let (tc,~[tr]) = splitTyConApp rep
                              in  (tc == signalTyCon, tr)
       signalTyCon = (typeRepTyCon.typeOf) (undefined :: Signal ())
       mPrimitiveTM = lookup rep primTypeTable 


-- | Translate a custom 'TypeRep' to a VHDL 'TypeMark'
customTR2TM :: TypeRep -> VHDLM TypeMark
customTR2TM rep = do
 -- Check if it was previously translated
 mTranslated <- lookupCustomType rep
 case mTranslated of
   -- Not translated previously
   Nothing -> do
      -- translate it
      e <- doCustomTR2TM rep
      -- update the translation table and the accumulated type declarations
      addCustomType rep e
      -- return the translation
      case e of
        Left (TypeDec id _) -> return id
        Right (SubtypeDec id _) -> return id
   -- Translated previously
   Just tm -> return tm
     
-- | Really do the translation (customTR2TM deals with caching)
doCustomTR2TM :: TypeRep -> VHDLM (Either TypeDec SubtypeDec)

-- | FSVec?
--   FSVecs are translated to subtypes of unconstrained vectors.
--   All FSVec operations are translated as operations for the
--   unconstrained type.
doCustomTR2TM rep | isFSVec = do
 -- Translate the type of the elements contained in the vector
 valTM <- transTR2TM valueType
 -- Build the unconstrained vector identifier
 let vectorId = unsafeVHDLBasicId ("fsvec_"++ fromVHDLId valTM)
 -- Obtain the unconstrained vector together with its functions and add them
 -- to the global traversing-results (if this wasn't previously done):
 --  * Check if the unconstrained array was previously translated
 vecs <- gets (transUnconsFSVecs.global)
 --  * if it wasn't ...
 when (not $ elem valueType vecs) $ do
      -- create the unconstrained vector type and add it to the global
      -- results. _Only_ if we are not working with "FSVec _ Bit" becuase
      -- "type fsvec_std_logic" is already included in forsyde.vhd.
      when (valueType /= typeOf (undefined :: Bit))
           (addTypeDec $ TypeDec vectorId (TDA (UnconsArrayDef [fsvec_indexTM] valTM)))
      -- Add the default functions for the unconstrained
      -- vector type to the global results
      let funs =  genUnconsVectorFuns valTM vectorId
      mapM_ addSubProgBody funs
      -- Mark the unconstrained array as translated
      addUnconsFSVec valueType

 -- Create the vector subtype identifier
 let subvectorId = unsafeVHDLBasicId ("fsvec_" ++ show size ++ "_" ++
                                     fromVHDLId valTM)
 -- Create the vector subtype declaration
 return $ Right $ 
     SubtypeDec subvectorId (SubtypeIn vectorId 
              (Just $ IndexConstraint [ToRange (PrimLit "0")
                                               (PrimLit (show $ size-1))]))
   where (cons, ~[sizeType,valueType]) = splitTyConApp rep
         isFSVec = cons == fSVecTyCon
         size = transTLNat2Int sizeType


-- | Tuple?
doCustomTR2TM rep | isTuple = do
  -- Create the elements of the record
  fieldTMs <- mapM transTR2TM args
  let elems = zipWith (\fieldId fieldTM -> ElementDec fieldId fieldTM )
                      [tupVHDLIdSuffix n | n <- [1..]] fieldTMs              
  -- Create the Type Declaration identifier
      recordId = unsafeVHDLBasicId $ 
              (tupStrSuffix $ length fieldTMs) ++ "_" ++ 
              (concatMap fromVHDLId.intersperse (unsafeVHDLBasicId "_")) fieldTMs
  -- Add the default functions for the tuple type to the global results
      funs = genTupleFuns fieldTMs recordId
  mapM_ addSubProgBody funs
  -- Create the record
  return $ Left $ (TypeDec recordId (TDR $ RecordTypeDef elems))
 where (cons, args) = splitTyConApp rep
       conStr = tyConString cons
       isTuple = all (==',') conStr
       

-- | AbstExt?
doCustomTR2TM rep | isAbsExt = do
  -- Create the elements of the record
  valueTM <- transTR2TM valueTR
  let elems = [ElementDec isPresentId booleanTM,
               ElementDec valueId     valueTM  ]
              
  -- Create the Type Declaration identifier
      recordId = unsafeVHDLBasicId $ 
                    "abs_ext_" ++ fromVHDLId valueTM
  -- Add the default functions for the vector type to the global results
      funs =  genAbstExtFuns valueTM recordId
  mapM_ addSubProgBody funs
  -- Return the resulting the record
  return $ Left $ (TypeDec recordId (TDR $ RecordTypeDef elems))
 where (cons, ~[valueTR]) = splitTyConApp rep
       absExtTyCon = (typeRepTyCon.typeOf) (undefined :: AbstExt ())
       isAbsExt = cons == absExtTyCon 

-- | Finally, it is an Enumerated algebraic type 
--   or an unkown custom type (in that case we throw an error)
--
--   NOTE: It would be cleaner to have a different clauses for each case but
--   since we need to access the state to check if it's an enumerated 
--   algebraic type, we cannot do it.
doCustomTR2TM rep = do
 -- Get the accumulated Enumerated Algebraic Types
 eTys <- gets (enumTypes.global)
 -- Check if current Type representation can be found in eTys
 let equalsRep (EnumAlgTy name _) = name == (tyConString.typeRepTyCon) rep 
 case (S.toList.(S.filter equalsRep)) eTys of
   -- Found!
   [enumDef] -> liftM Left $ enumAlg2TypeDec enumDef 
   -- Not found, unkown custom type
   _ ->  throwFError $ UnsupportedType rep

-- | Transform an enumerated Algebraic type to a VHDL
--   TypeMark adding its default function to the global results
enumAlg2TypeDec :: EnumAlgTy -- ^ Enumerated type definition
                -> VHDLM TypeDec
enumAlg2TypeDec (EnumAlgTy tn cons) = do
 -- Get the TypeMark
 tMark <- liftEProne $ mkVHDLExtId tn
 -- Get the enumeration literals
 enumLits@(firstLit:_) <- liftEProne $ mapM mkVHDLExtId cons
 -- Add the default functions for the enumeration type
 let funs = genEnumAlgFuns tMark firstLit
 mapM_ addSubProgBody funs
 -- Create the enumeration type
 return (TypeDec tMark (TDE $ EnumTypeDef enumLits))
   
-- | Translation table for primitive types
primTypeTable :: [(TypeRep, TypeMark)]
primTypeTable = [-- Commented out due to representation overflow
                 -- (typeOf (undefined :: Int64), int64TM)   ,
                 (typeOf (undefined :: Int32), int32TM)   ,
                 (typeOf (undefined :: Int16), int16TM)   ,
                 (typeOf (undefined :: Int8) , int8TM)    ,
                 (typeOf (undefined :: Bool) , booleanTM) ,
                 (typeOf (undefined :: Bit)  , std_logicTM)]

---------------------------------------
-- Translating functions and expresions
---------------------------------------

------------------------
-- Translating functions
------------------------


-- | Throw a function error
funErr :: VHDLFunErr -> VHDLM a
funErr err = throwFError $ UntranslatableVHDLFun err

-- | Translate a typed function AST to VHDL
transProcFun2VHDL :: TypedProcFunAST  -- ^ input ast
    -> VHDLM (SubProgBody, VHDLId, [VHDLId], [TypeMark], TypeMark)
    -- ^ Function, Function name, name of inputs, type of inputs, return type   
transProcFun2VHDL (TypedProcFunAST fType fEnums fAST) = do
 -- Add the enumerated types associated with the function to the global results
 addEnumTypes fEnums
 -- Check if the procFunAST fullfils the restrictions of the VHDL Backend
 -- FIXME: translate the default arguments
 (fName, fInputPats, fBodyExp, whereDecs) <- checkProcFunAST fAST
 -- Get the function spec and initialize the translation namespace
 (fSpec, fVHDLName, fVHDLPars, argsTM, retTM) <- 
  transProcFunSpec fName fType fInputPats
 -- Translate the where declarations and them to the
 -- auxiliary declarations of the function
 transDecs whereDecs
 -- Translate the function's body
 bodySm <- transFunBodyExp2VHDL fBodyExp
 decs <- gets (auxDecs.funTransST.local)
 let  fBody = SubProgBody fSpec decs [bodySm]
 return (fBody, fVHDLName, fVHDLPars, argsTM, retTM)

-- | Translate a typed function AST to VHDL (only returning the functions body
transProcFun2VHDLBody :: TypedProcFunAST -> VHDLM SubProgBody
transProcFun2VHDLBody tpf = do
 (body, _, _, _, _) <- transProcFun2VHDL tpf
 return body

-- | Translate a list of declarations to a list of process function 
--   ASTs
decs2ProcFuns :: [Dec] -> VHDLM [TypedProcFunAST]
decs2ProcFuns [] = return []
decs2ProcFuns decs = do
 (dec, t, name, clauses, restDecs) <- case decs of
   -- A  type signature followed by its function declaration
   SigD n1 t : f@(FunD n2 cls) : xs | n1 == n2 -> 
      return (f, t, n1, cls, xs)
   -- A type signature followed by its value declaration
   -- which will be translated to a function
   SigD n1 t : v@(ValD (VarP n2) bdy ds) : xs | n1 == n2 -> do
      return (v, t, n1, [Clause [] bdy ds] , xs)
   -- Otherwise the provided declaration block is not supported
   _ -> funErr $ UnsupportedDecBlock decs    
 t' <- maybe (funErr $ PolyDec dec) return (type2TypeRep t) 
 let tpf = TypedProcFunAST t' S.empty (ProcFunAST name clauses [])
 restTPFs <- decs2ProcFuns restDecs
 return $ tpf:restTPFs

-- | Tranlate a list of declarations and add them to the auxiliary
--   declarations in the function translation state
transDecs :: [Dec] -> VHDLM ()
transDecs decs = do
  -- first we tranlsate the declarations to process functions
  tpfs <- decs2ProcFuns decs
  -- Before translating the process functions we add their names to the
  -- name translation table. It is important to note that, since
  -- Template Haskell makes local names unique (e.g. [| let x = 1 in
  -- let x = 2 in x |] is tranlsated to let x_0 = 1 in let x_1 = 2 in x_2),
  -- we don't have to take care of name scopes i.e. we can have a global name
  -- scope.
  mapM_ addDecName tpfs
  -- Translate the declarations to VHDL and add them
  -- to the auxiliary declarations of the function translation
  bodyDecs <- mapM (liftM SPSB . transProcFun2VHDLBody) tpfs
  addDecsToFunTransST bodyDecs
 where addDecName :: TypedProcFunAST -> VHDLM ()
       addDecName (TypedProcFunAST t _ (ProcFunAST n _ _)) = do
          let arity = (length.fst.unArrowT) t
          vhdlId <- transTHName2VHDL n
          addTransNamePair n arity (genExprFCallN vhdlId arity) 

-- | Check if a process function AST fulfils the VHDL backend restrictions.
--   It returs the function TH-name its input paterns, its body expression,
--   and the list of theclarations in the where construct. 
checkProcFunAST :: ProcFunAST
                -> VHDLM (Name, [Pat], Exp, [Dec])
-- FIXME: translate the default arguments!
checkProcFunAST (ProcFunAST thName [Clause pats (NormalB exp) decs] []) =
 return (thName, pats, exp, decs)
checkProcFunAST (ProcFunAST _ _ (_:_)) =
 intError "ForSyDe.Backend.VHDL.Translate.checkProcFunSpec" 
          (UntranslatableVHDLFun $ GeneralErr (Other "default parameters are not yet supported"))
checkProcFunAST (ProcFunAST _ [Clause _ bdy@(GuardedB _) _] _) =  
  funErr (FunGuardedBody bdy)
checkProcFunAST (ProcFunAST _ clauses@(_:_) _) =  
  funErr (MultipleClauses clauses)
-- cannot happen
checkProcFunAST (ProcFunAST _ [] _) =  
 -- FIXME, use a custom error
 intError "ForSyDe.Backend.VHDL.Translate.checkProcFunSpec" 
          (UntranslatableVHDLFun $ GeneralErr (Other "inconsistentency"))



-- |  Get the spec of a VHDL function from the Haskell function name, its type 
--    and its input patterns. This function also takes care of initalizing the 
--    translation namespace.
transProcFunSpec :: TH.Name -- ^ Function name
                 -> TypeRep -- ^ Function type
                 -> [Pat]   -- ^ input patterns 
                 -> VHDLM (SubProgSpec, VHDLId, [VHDLId], [TypeMark], TypeMark)
-- ^ translated function spec, function name, inpt parameters, input types
--   and return types
transProcFunSpec fName fType fPats = do
 -- FIXME: translate the default arguments!
 -- Get the input and output types
 let (argsTR, retTR) = unArrowT fType
 -- Check that the number of patterns equal the function parameter number
     expectedN = length argsTR
     actualN = length fPats
 when (expectedN /= actualN) (funErr $ InsParamNum fName actualN)
 -- Get a VHDL identifier for each input pattern and
 -- initialize the translation namespace
 fVHDLParIds <- mapM transInputPat2VHDLId fPats 
 -- Translate the function name
 fVHDLName <- transTHName2VHDL fName 
 -- Translate the types
 argsTM <- mapM transTR2TM argsTR
 retTM <- transTR2TM retTR
 -- Create the spec
 let iface = zipWith (\name typ -> IfaceVarDec  name typ) fVHDLParIds argsTM
     fSpec = Function fVHDLName iface retTM
 -- Finally, return the results
 return (fSpec, fVHDLName, fVHDLParIds, argsTM, retTM)
 
-- | Translate an input pattern to a VHDLID, 
--   making the necessary changes in the translation namespace
transInputPat2VHDLId :: TH.Pat -> VHDLM VHDLId
transInputPat2VHDLId  pat = do
 -- Get the parameter identifier
 id <- case pat of
         -- if we get a variable or and @ patterm, we just translate it to VHDL
         VarP name -> transTHName2VHDL name
         AsP name _ -> transTHName2VHDL name
         -- otherwise, generate a fresh identifier
         _ -> genFreshVHDLId

 -- Prepare the namespace for the pattern
 preparePatNameSpace (NSimple id) pat
 -- Finally return the generated id
 return id 


-- | prepare the translation namespace for an input pattern
preparePatNameSpace :: Prefix -- ^ name prefix obtained so far 
                    -> Pat    -- ^ pattern 
                    -> VHDLM ()
-- NOTE: a good alternative to adding selected names to the
--       translation table would be declaring a variable
--       assignment. It would probably make the generated code more
--       readable but at the same time, it requires knowing the
--       pattern type, and TH's AST is unfortunately not
--       type-annotated which would make things more difficult.

-- variable pattern
preparePatNameSpace prefix (VarP name) = 
 addTransNamePair name 0 (\[] -> PrimName prefix)

-- '@' pattern 
preparePatNameSpace prefix (AsP name pat) = do
  addTransNamePair name 0 (\[]  -> PrimName prefix)
  preparePatNameSpace prefix pat

-- wildcard pattern
preparePatNameSpace _ WildP = return ()  

-- tuple pattern 
preparePatNameSpace prefix (TupP pats) = do
  let prepTup n pat = preparePatNameSpace 
                          (NSelected (prefix :.: tupVHDLSuffix n)) pat
  zipWithM_ prepTup [1..] pats

-- AbstExt patterns

-- Since we only support one clause per function
-- they are not really useful, but we accept them anyways 
-- FIXME: true, they are not useful, but again, since we only support one
--        clause per function they denote a programming error. Should they
--        really be supported?
preparePatNameSpace prefix (ConP name ~[pat]) | isAbstExt name =  
  when isPrst (preparePatNameSpace (NSelected (prefix :.: valueSuffix)) pat) 
 where isAbstExt name = isPrst || name == 'Abst
       isPrst =  name == 'Prst

-- Unary Constructor patterns
-- We try an enumerated type patterns 
-- otherwise we throw an unknown constructor pattern error
preparePatNameSpace _ pat@(ConP name []) = do
 mId <- getEnumConsId name
 case mId of 
   -- it is an enumerated data constructor, however, since we only admit
   -- one clause per function there is nothing to do about it
  Just _ -> return ()
  -- it is an unknown data constructor
  Nothing -> funErr $ UnsupportedFunPat pat

-- otherwise the pattern is not supported
preparePatNameSpace _ pat = funErr $ UnsupportedFunPat pat



--------------------------
-- Translating expressions
--------------------------

-- | Throw an expression error
expErr :: Exp -> VHDLExpErr -> VHDLM a
expErr exp err = throwFError $ UntranslatableVHDLExp exp err


-- | Create the unique statement of a VHDL from a TH expression.
transFunBodyExp2VHDL :: TH.Exp -> VHDLM SeqSm
transFunBodyExp2VHDL  (CondE condE thenE  elseE)  = 
  do condVHDLE  <- transExp2VHDL condE   
     thenVHDLSm <- transFunBodyExp2VHDL thenE 
     elseVHDLSm <- transFunBodyExp2VHDL elseE 
     return (IfSm condVHDLE [thenVHDLSm] [] (Just $ Else [elseVHDLSm]))
transFunBodyExp2VHDL caseE@(CaseE exp matches)  = 
  do caseVHDLE  <- transExp2VHDL exp 
     caseSmAlts <- mapM (transMatch2VHDLCaseSmAlt caseE) matches
     return (CaseSm caseVHDLE caseSmAlts)
-- In other case it is an expression returned directly
transFunBodyExp2VHDL  e =            
  do vHDLe <- transExp2VHDL e 
     return (ReturnSm $ Just vHDLe)

-- | Translate a case alternative from Haskell to VHDL
transMatch2VHDLCaseSmAlt :: TH.Exp -> TH.Match -> VHDLM CaseSmAlt
-- FIXME: the exp passed (which contains the full case expression for
-- error reporting purposes) should be part of the context once VHDLM
-- is reworked
transMatch2VHDLCaseSmAlt contextExp (Match pat (NormalB matchExp) decs) =
 do transDecs decs
    sm <- transFunBodyExp2VHDL matchExp
    case pat of
     -- FIXME: support pattern matching with tuples, AbsExt, 
     -- and enumerated types
     WildP -> return $ CaseSmAlt [Others] [sm] 
     LitP lit -> do vHDLExp <- transExp2VHDL (LitE lit)
                    return $ CaseSmAlt [ChoiceE vHDLExp] [sm]
     -- FIXME: check! this case introduces new names into scope
     VarP name -> do vHDLExp <- transExp2VHDL (VarE name)
                     return $ CaseSmAlt [ChoiceE vHDLExp] [sm]
     _ -> expErr contextExp $ UnsupportedCasePat pat
transMatch2VHDLCaseSmAlt contextExp (Match _ bdy@(GuardedB _) _) =
 expErr contextExp $ CaseGuardedBody bdy


-- | Translate a Haskell expression to a VHDL expression
transExp2VHDL :: TH.Exp -> VHDLM VHDL.Expr


-- TypeLevel-package numerical constant aliases
transExp2VHDL (VarE name) | isTypeLevelAlias = do
 let constant = nameBase name
     ([baseSym], val) = splitAt 1 constant
     basePrefix = case baseSym of
       'b' -> "2#" 
       'o' -> "8#" 
       'h' -> "16#"
       'd' -> ""
       _   -> error "unexpected base symbol"
 return (PrimLit $ basePrefix ++ val) 
 where isTypeLevelAlias = (show name =~ aliasPat)
       aliasPat = "^Data\\.TypeLevel\\.Num\\.Aliases\\.(b[0-1]+|o[0-7]+|d[0-9]+|h[0-9A-F]+)$"



-- A FSVec generated with Template Haskell 
transExp2VHDL (VarE unsafeFSVecCoerce `AppE` _ `AppE` (ConE con `AppE` ListE exps)) 
 | show unsafeFSVecCoerce == "Data.Param.FSVec.unsafeFSVecCoerce" &&
   show con == "Data.Param.FSVec.FSVec" = do
    vhdlExps <- mapM transExp2VHDL exps
    return $ Aggregate (map (\e -> ElemAssoc Nothing e) vhdlExps)


-- Is it function/constructor application, a constant
-- or an unkown name.
transExp2VHDL e | isConsOrFun   =
  do -- get the symbol table (name translation table)
     nameTable <- gets (nameTable.funTransST.local)
     case lookup name nameTable of
       -- found name
       Just (arity, transF) -> 
            if arity /= numArgs 
              then expErr e $ CurryUnsupported arity numArgs
              else do exps <- mapM transExp2VHDL args
                      return $ transF exps
       -- Didn't find the name in the global table
       Nothing -> do 
         -- Check if it is a user-defined enumerated data constructor 
         mId <- getEnumConsId name
         case mId of
            Just id -> return $ PrimName (NSimple id)
            Nothing -> expErr e $ UnkownIdentifier name
 where (f,args,numArgs) = unApp e 
       mName = getName f
       name = fromJust mName
       isConsOrFun = isJust mName 
       getName (VarE n) = Just n
       getName (ConE n) = Just n
       getName _        = Nothing



-- Literals
transExp2VHDL  (LitE (IntegerL integer))  = (return.transInteger2VHDL) integer
transExp2VHDL  (LitE (IntPrimL integer))  = (return.transInteger2VHDL) integer

-- Unsupported literal 
transExp2VHDL lit@(LitE _) = expErr lit $ UnsupportedLiteral

-- Infix expressions
transExp2VHDL (InfixE (Just argl) f@(VarE _) (Just argr)) = 
 transExp2VHDL $ f `AppE` argl `AppE` argr

-- Sections (unsupported)
transExp2VHDL infixExp@(InfixE _ (VarE _) _) = expErr infixExp Section

-- Tuples: e.g. (1,2)
transExp2VHDL (TupE exps) = do
 vExps <- mapM transExp2VHDL exps
 return $ Aggregate $ map (\expr -> ElemAssoc Nothing expr) vExps

-- Let expressions
transExp2VHDL (LetE decs e) = do
 transDecs decs
 transExp2VHDL e

-- Unsupported expressions
transExp2VHDL lamE@(LamE _ _) = expErr lamE  LambdaAbstraction
transExp2VHDL condE@(CondE _ _ _) = expErr condE Conditional
transExp2VHDL caseE@(CaseE _ _) = expErr caseE Case
transExp2VHDL doE@(DoE _) = expErr doE Do	
transExp2VHDL compE@(CompE _) = expErr compE ListComprehension
transExp2VHDL arithSeqE@(ArithSeqE _) = expErr arithSeqE ArithSeq	
transExp2VHDL listE@(ListE _) = expErr listE List	
transExp2VHDL sigE@(SigE _ _) = expErr sigE Signature	
transExp2VHDL reConE@(RecConE _ _) = expErr reConE Record	
transExp2VHDL recUpE@(RecUpdE _ _) = expErr recUpE Record

-- The rest of expressions are not valid in practice and thus, not supported 
-- (e.g. InfixE Nothing (RecConE _ _) _
transExp2VHDL exp = expErr exp Unsupported
  

-- | Translate an integer to VHDL
transInteger2VHDL :: Integer -> Expr
transInteger2VHDL = PrimLit . show 


--------------------
-- Helper Functions
--------------------

-- Translate the TypeRep of a type-level natural (e.g: D1 :* D2) to a number
-- Make sure you don't supply an incorrect TypeRep or the function will break
transTLNat2Int :: TypeRep -> Int
transTLNat2Int tr
  -- Digit
  | isDigit = (digitToInt.last.tyConString) cons
  -- Connective
  | otherwise = 10 * (transTLNat2Int prefix) + (transTLNat2Int lastDigit) 
 where (cons, args@(~[prefix, lastDigit])) = splitTyConApp tr
       isDigit = null args


-- Tranlate an Into to the TypeRep of a type-level natural (e.g: D1 :* D2)
transInt2TLNat :: Int -> TypeRep
transInt2TLNat n
 | n < 0 = intError fName (Other "negative index")
 | n < 10 = digit n
 | otherwise = mkTyConApp conTyCon [transInt2TLNat suffix, digit last]
 where fName = "ForSyDe.Backend.VHDL.Translate.transInt2TLNat"
       (suffix, last) = n `divMod` 10 
       digit 0 = typeOf (undefined :: D0)
       digit 1 = typeOf (undefined :: D1)
       digit 2 = typeOf (undefined :: D2)
       digit 3 = typeOf (undefined :: D3)
       digit 4 = typeOf (undefined :: D4)
       digit 5 = typeOf (undefined :: D5)
       digit 6 = typeOf (undefined :: D6)
       digit 7 = typeOf (undefined :: D7)
       digit 8 = typeOf (undefined :: D8)
       digit 9 = typeOf (undefined :: D9)
       -- Just to hush the compiler warnings
       digit _ = undefined
       conTyCon = (typeRepTyCon.typeOf) (undefined :: () :* ())

-- Type constructor of FSVec
fSVecTyCon :: TyCon
fSVecTyCon =(typeRepTyCon.typeOf) (undefined :: V.FSVec () ())

-- unApply an expression and obtain the number of arguments found
unApp :: Exp -> (Exp, [Exp], Int)
unApp e = (first, rest, n)
 where (first:rest, n) = unAppAc ([],0) e
       unAppAc (xs,n) (f `AppE` arg) = unAppAc (arg:xs, n+1) f
       unAppAc (xs,n) f = (f:xs,n)
