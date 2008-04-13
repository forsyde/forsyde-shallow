{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Traverse
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
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
import ForSyDe.Signal
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import ForSyDe.Process.ProcFun
import ForSyDe.Process.ProcVal

import Data.Typeable.TypeRepLib (unArrowT)


import Data.Char (digitToInt)
import Data.List (intersperse, find)
import Data.Maybe (isJust, fromJust)
import Data.Bits ((.&.), (.|.), xor)
import Control.Monad.State
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (global)
import qualified Data.Traversable as DT
import Data.Typeable
import Data.Char (toLower)
import qualified Data.Param.FSVec as V


import Data.TypeLevel.Num.Reps

-- | Translate a System Definition to an Entity, explicitly returning
--   the VHDL identifiers of its output signals.
transSysDef2Ent :: SysDefVal -> VHDLM EntityDec
transSysDef2Ent sysDefVal = do
 -- FIXME: check if the entity name and its signals are in the same namespace
 -- then we could use mkVHDLId instead.
 entId <- transSysId2VHDL (sid sysDefVal)
 inDecs  <- mapM (uncurry $ transPort2IfaceSigDec In)  (iIface sysDefVal) 
 outDecs <- mapM (uncurry $ transPort2IfaceSigDec Out) (oIface sysDefVal)
 -- clock and reset implicit declarations
 let implicitDecs = [IfaceSigDec resetId In std_logicTM, 
                     IfaceSigDec clockId In std_logicTM]
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
 -- function's input vector type (ugly hack, but it works)
 tdecs <- gets (typeDecs.globalRes.global)
 let TypeDec _ (TDA (ArrayTypeDef _ _ inType)) 
      = fromJust $ find (\(TypeDec id _) -> id == inFType)  tdecs
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
     aggregate = Aggregate (map (PrimName . NSimple) inPars) 
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
                              (NSimple inPar :.: 
                              (SSimple $ unsafeVHDLBasicId $ "tup_" ++ show n)))
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
     genOrigExp n = PrimName (NSimple inPar) `IndexedExp` (PrimLit  $ show n)
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
transDelay2Block vPid inS ast outS = do
 -- Get the type of the initial value
 initTR <- transTR2TM (expTyp ast)
 -- Translate the initial value
 let val = expVal ast
 initExp <- withProcValC val $ withNameTable [] $ (transExp2VHDL val)
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
     whenElseReset = WhenElse (Wform [initExp])
                               (PrimName (NSimple resetId) :=: PrimLit "'0'")
     inWform = Wform [PrimName $ NSimple formalIn]
     whenRE = When (PrimFCall $ FCall (NSimple $ unsafeVHDLBasicId "rising_edge") 
                                      [Nothing :=>: ADName (NSimple clockId) ])
 return  (BlockSm vPid iface (PMapAspect assocs) [] [sigAssign],
          SigDec outS initTR Nothing)

-- | Translate a System instance into a VHDL component instantion
--   returning the declartion of the output signals
transSysIns2CompIns :: Label -- ^ instance identifier
                    -> [VHDLId] -- ^ input signals
                    -> [(VHDLId, TypeRep)] -- ^ output signals
                    -> SysId -- ^ parent system identifier
                    -> [PortId] -- ^ parent input identifiers
                    -> [PortId] -- ^ parent output identifiers
                    -> VHDLM (CompInsSm, [SigDec])
transSysIns2CompIns vPid ins typedOuts parentId parentInIds parentOutIds = do
  -- Create the declarations for the signals
  decs <- mapM (\(name,typ) -> transVHDLName2SigDec name typ Nothing) typedOuts
  -- Create the portmap 
  vParentId <- transSysId2VHDL parentId
  vParentInIds <- liftEProne $ mapM mkVHDLId parentInIds
  vParentOutIds <- liftEProne $ mapM mkVHDLId parentOutIds
  let assocs =  genAssocElems 
                  ([resetId, clockId] ++ vParentInIds ++ vParentOutIds)
                  ([resetId, clockId] ++ ins          ++ map fst typedOuts)
      entityName = NSelected (NSimple workId :.: SSimple vParentId)
      instantiation = CompInsSm vPid (IUEntity entityName) (PMapAspect assocs)
  return (instantiation, decs)


-- | Translate a VHDL Signal to a VHDL Signal declaration
transVHDLName2SigDec ::  SimpleName -- ^ Signal name 
             -> TypeRep -- ^ Type of the intermediate signal 
             -> Maybe TH.Exp -- ^ Maybe an initializer expression for the signal
             -> VHDLM SigDec
transVHDLName2SigDec vId tr mExp = do
 tm <- transTR2TM tr
 mVExp <- DT.mapM (\e -> withNameTable [] (transExp2VHDL e)) mExp
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
 
-- | Translate a system identifier to a VHDL identifier
transSysId2VHDL :: SysId -> VHDLM VHDLId
transSysId2VHDL = transPortId2VHDL

-- | Translate a process identifier to a VHDL identifier
transProcId2VHDL :: ProcId -> VHDLM VHDLId
transProcId2VHDL = transPortId2VHDL

-- | translate a port identifier to a VHDL Identifier
transPortId2VHDL :: PortId -> VHDLM VHDLId
transPortId2VHDL str = do let strL = map toLower str
                          when (elem strL reservedStrs)
                               (throwFError (ReservedId str)) 
                          liftEProne $ mkVHDLId str


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
      typeDec@(TypeDec id _) <- doCustomTR2TM rep
      -- update the translation table and the accumulated type declarations
      addTypeDec rep typeDec
      -- return the translation
      return id
   -- Translated previously
   Just tm -> return tm
     
-- | Really do the translation (customTR2TM deals with caching)
doCustomTR2TM :: TypeRep -> VHDLM TypeDec

-- | FSVec
doCustomTR2TM rep | isFSVec = do
 -- Translate the type of the elements contained in the vector
 valTM <- transTR2TM valueType
 -- Create the vector type identifier
 let vectorId = unsafeVHDLBasicId ("fsvec_" ++ show size ++ "_" ++
                                   fromVHDLId valTM)
 -- Create the vector declaration
     vectorDec = TypeDec vectorId (TDA (ArrayTypeDef 0 (size-1) valTM))
 return vectorDec
   where (cons, ~[sizeType,valueType]) = splitTyConApp rep
         isFSVec = cons == fSVecTyCon
         size = transTLNat2Int sizeType
 

-- | Tuples
doCustomTR2TM rep | isTuple = do
  -- Create the elements of the record
  fieldTMs <- mapM transTR2TM args
  let elems = zipWith (\fieldId fieldTM -> ElementDec (unsafeVHDLBasicId fieldId)
                                                      fieldTM )
                   ["tup_" ++ show n | n <- [(1::Int)..]] fieldTMs              
  -- Create the Type Declaration identifier
      recordId = unsafeVHDLBasicId $ 
              "tup" ++ 
              (show $ length fieldTMs) ++ "_" ++ 
              (concatMap fromVHDLId.intersperse (unsafeVHDLBasicId "_")) fieldTMs 
  -- Create the record
  return (TypeDec recordId (TDR $ RecordTypeDef elems))
 where (cons, args) = splitTyConApp rep
       conStr = tyConString cons
       isTuple = all (==',') conStr
       

-- | Abst

-- | Unkown custom type
doCustomTR2TM rep = throwFError $ UnsupportedType rep               
        
-- | Translation table for primitive types
primTypeTable :: [(TypeRep, TypeMark)]
primTypeTable = [(typeOf (undefined :: Int), int32TM),
                 (typeOf (undefined :: Bool), std_logicTM)]

---------------------------------------
-- Translating functions and expresions
---------------------------------------

------------------------
-- Translating functions
------------------------


-- | Throw an expression error
funErr :: VHDLFunErr -> VHDLM a
funErr err = throwFError $ UntranslatableVHDLFun err

-- | Translate a typed function ast to VHDL
transProcFun2VHDL :: TypedProcFunAST  -- ^ input ast
    -> VHDLM (SubProgBody, VHDLId, [VHDLId], [TypeMark], TypeMark)
    -- ^ Function, Function name, name of inputs, type of inputs, return type   
transProcFun2VHDL (TypedProcFunAST trep ast) = do
 let (argsTR, retTR) = unArrowT trep
 -- Get the interface
 argsTM <- mapM transTR2TM argsTR
 retTM <- transTR2TM retTR
 -- FIXME: translate the default arguments
 -- Check the and translate the function's spec
 (fName, pars, nameTable, bodyExp) <- checkProcFunSpec (length argsTM) ast
 let iface = zipWith (\name typ -> IfaceVarDec  name typ) pars argsTM
     fSpec = Function fName iface retTM
 -- Translate the function's body
 bodySm <- withNameTable nameTable (transFunBodyExp2VHDL bodyExp)
 let  fBody = SubProgBody fSpec [bodySm]
 return (fBody, fName, pars, argsTM, retTM)

-- |  Translate the name of a Process function and its formal parameters,
--    previously checking if the function fullfils the expected constraints of 
--    the VHDL backend.
checkProcFunSpec ::  Int -- ^ expected number of parameters
          -> ProcFunAST -- ^ the function AST
          -> VHDLM (VHDLId, [VHDLId], [(TH.Name, VHDLName)],  TH.Exp)
 -- ^ translated function name, function parameters, initial namespace,  and
 --   function body
checkProcFunSpec argN (ProcFunAST thName [Clause pats (NormalB exp) []] _)= do
 fName <-  thName2VHDL thName
 parTHNames <-  mapM getParName pats
 parVHDLIds <-  mapM thName2VHDL parTHNames
 let nameTable = zipWith (\thName vHDLId-> (thName,NSimple vHDLId)) parTHNames
                                                                    parVHDLIds
     parN = length nameTable
 when (parN /= argN) 
   (funErr $ InsParamNum parN)
 return (fName, parVHDLIds, nameTable, exp)
    where getParName (VarP name) = return name 
          getParName pat = funErr $ NonVarPar pat
          thName2VHDL name = (liftEProne.mkVHDLId.nameBase) name
checkProcFunSpec _ (ProcFunAST _ [Clause _ _ whereConstruct@(_:_)] _) =  
  funErr (FunWhereConstruct whereConstruct)
checkProcFunSpec _ (ProcFunAST _ [Clause _ bdy@(GuardedB _) _] _) =  
  funErr (FunGuardedBody bdy)
checkProcFunSpec _ (ProcFunAST _ clauses@(_:_:_) _) =  
  funErr (MultipleClauses clauses)
-- cannot happen
checkProcFunSpec _ (ProcFunAST _ [] _) =  
 -- FIMXE, use a custom error
 intError "ForSyDe.Backend.VHDL.Translate.checkProcFunSpec" 
          (UntranslatableVHDLFun $ GeneralErr (Other "inconsistentency"))


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
-- FIXME: the exp passed should be part of the context once VHDLM is reworked
transMatch2VHDLCaseSmAlt contextExp (Match pat (NormalB matchExp) []) =
 do sm <- transFunBodyExp2VHDL matchExp
    case pat of
     WildP -> return $ CaseSmAlt [Others] [sm] 
     LitP lit -> do vHDLExp <- transExp2VHDL (LitE lit)
                    return $ CaseSmAlt [ChoiceE vHDLExp] [sm]
     -- FIXME: check! this case introduces new names into scope
     VarP name -> do vHDLExp <- transExp2VHDL (VarE name)
                     return $ CaseSmAlt [ChoiceE vHDLExp] [sm]
     _ -> expErr contextExp $ UnsupportedCasePat pat
transMatch2VHDLCaseSmAlt contextExp (Match _ _ whereDecs@(_:_)) =
 expErr contextExp $ CaseWhereConstruct whereDecs
transMatch2VHDLCaseSmAlt contextExp (Match _ bdy@(GuardedB _) _) =
 expErr contextExp $ CaseGuardedBody bdy


-- | Translate a Haskell expression to a VHDL expression
transExp2VHDL :: TH.Exp -> VHDLM VHDL.Expr

-- Is it an binary function application?
transExp2VHDL (VarE fName `AppE` arg1 `AppE` arg2)
 | isJust maybeInfixOp =
  do vHDLarg1 <- transExp2VHDL arg1
     vHDLarg2 <- transExp2VHDL arg2
     return $ (fromJust maybeInfixOp) vHDLarg1 vHDLarg2
 where maybeInfixOp = lookup fName validBinaryFuns

-- Is it a unary function application?
transExp2VHDL (VarE fName `AppE` arg) | isJust maybeUnaryOp =
  do vHDLarg <- transExp2VHDL arg
     return $ (fromJust maybeUnaryOp) vHDLarg      
 where maybeUnaryOp = lookup fName validUnaryFuns


-- Unkown function
transExp2VHDL exp@(VarE fName `AppE` _) = expErr exp $ UnkownIdentifier fName

-- Local variable or unknown identifier
transExp2VHDL exp@(VarE name) = 
  do -- get the list of valid local names from the state monad
     validLocalNames <- gets (nameTable.local)
     let mVHDLName = lookup name validLocalNames
     maybe (expErr exp $ UnkownIdentifier name) 
           (\name -> return $ PrimName name) 
           mVHDLName       
        

-- Unary constructor
transExp2VHDL  con@(ConE cName) 
  | isJust consTranslation = return $ fromJust consTranslation
  | otherwise =  expErr con $ UnkownIdentifier cName
    where consTranslation = lookup cName validUnaryCons

-- Unkown constructor
transExp2VHDL  con@(ConE cName `AppE` _) = expErr con $ UnkownIdentifier cName

-- Literals
transExp2VHDL  (LitE (IntegerL integer))  = (return.transInteger2VHDL) integer
transExp2VHDL  (LitE (IntPrimL integer))  = (return.transInteger2VHDL) integer

-- Unsupported literal 
transExp2VHDL lit@(LitE _) = expErr lit $ UnsupportedLiteral

-- Infix expressions
transExp2VHDL infixExp@(InfixE (Just argl) (VarE fName) (Just argr)) = 
  case maybeInfixOp of
   Just op ->
     do vHDLargl <- transExp2VHDL argl
        vHDLargr <- transExp2VHDL argr
        return $ op vHDLargl vHDLargr
   Nothing -> expErr infixExp  $ UnkownIdentifier fName
 where maybeInfixOp = lookup fName validBinaryFuns
-- Sections (unsupported)
transExp2VHDL infixExp@(InfixE _ (VarE _) _) = expErr infixExp Section

-- Tuples: e.g. (1,2)
transExp2VHDL (TupE exps) = do
 vExps <- mapM transExp2VHDL exps
 return $ Aggregate vExps

-- Unsupported expressions
transExp2VHDL lamE@(LamE _ _) = expErr lamE  LambdaAbstraction
transExp2VHDL condE@(CondE _ _ _) = expErr condE Conditional
transExp2VHDL letE@(LetE _ _) = expErr letE Let
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

-- FIXME: remove when done
-- PrimFCall $ FCall (unsafeVHDLBasicId "TO_SIGNED") 
--  [Just (unsafeVHDLBasicId "ARG")  :=>: (ADExpr $ PrimLit (show i)),
--   Just (unsafeVHDLBasicId "SIZE") :=>: (ADExpr $ PrimLit "32")     ]


-- | Translation table of valid constructors
validUnaryCons :: [(TH.Name, Expr)]
validUnaryCons = [('True, PrimLit "'1'"),
                  ('False, PrimLit "'0'")]



-- | Translation table of valid binary functions
validBinaryFuns :: [(TH.Name, (Expr -> Expr -> Expr))]
validBinaryFuns = [('(&&) , And   ),
                   ('(||) , Or    ),
                   ('(.&.), And   ),
                   ('(.|.), Or    ),
                   ('xor  , Xor   ),
                   ('(==) , (:=:) ),
                   ('(/=) , (:/=:)),
                   ('(<)  , (:<:) ), 
                   ('(<=) , (:<=:)),
                   ('(>)  , (:>:) ),
                   ('(>=) , (:>=:)),  
                   ('(+)  , (:+:) ),
                   ('(-)  , (:-:) ),
                   ('(*)  , (:*:) ),
                   ('div  , (:/:) ),
                   ('mod  , (Mod) ),
                   ('rem  , (Rem) ),
                   ('(^)  , (:**:))] 
                 

-- | Translation table of valid unary functions
validUnaryFuns :: [(TH.Name, (Expr -> Expr))]
validUnaryFuns = [('not   , Not  ),
                  ('negate, Neg  ),
                  ('abs   , Abs  )]

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