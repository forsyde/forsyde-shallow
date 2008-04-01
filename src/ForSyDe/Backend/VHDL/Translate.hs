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
import ForSyDe.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Backend.VHDL.Generate

import ForSyDe.Ids
import ForSyDe.Signal
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import ForSyDe.Process.ProcFun

import Data.Typeable.TypeRepLib (unArrowT)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import qualified Data.Traversable as DT
import Data.Typeable
import Data.Char (toLower)

-- | Translate a System Definition to an Entity, explicitly returning
--   the VHDL identifiers of its output signals.
transSysDef2Ent :: SysDefVal -> EProne EntityDec
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

-- | Translate a 'ZipwithN' process to a block returning a declaration of
--   the resulting signal.
transZipWithN2Block :: Label -- ^ process identifier
                    -> [VHDLName] -- ^ input signals
                    -> TypedProcFunAST -- ^ AST of the inner function
                    -> VHDLName   -- ^ output signal
                    -> EProne (BlockSm, SigDec)
transZipWithN2Block vPid ins ast out = do
 -- Translate the process function
 (f,fName , inFPars, inFTypes, retFType) <- transProcFun2VHDL ast 
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
     fCall = genFCall fName inFPars ins
     outAssign = out :<==: (ConWforms [] (Wform [PrimFCall fCall]) Nothing) 
 return  (BlockSm vPid iface pMap [BDISPB f] [CSSASm outAssign],
          SigDec out retFType Nothing)

-- | Translate a System instance into a VHDL component instantion
--   returning the declartion of the output signals
transSysIns2CompIns :: Label -- ^ instance identifier
                    -> [VHDLName] -- ^ input signals
                    -> [(VHDLName, TypeRep)] -- ^ output signals
                    -> SysId -- ^ parent system identifier
                    -> [PortId] -- ^ parent input identifiers
                    -> [PortId] -- ^ parent output identifiers
                    -> EProne (CompInsSm, [SigDec])
transSysIns2CompIns vPid ins typedOuts parentId parentInIds parentOutIds = do
  -- Create the declarations for the signals
  decs <- mapM (\(name,typ) -> transVHDLName2SigDec name typ Nothing) typedOuts
  -- Create the portmap 
  vParentId <- transSysId2VHDL parentId
  vParentInIds <- mapM mkVHDLId parentInIds
  vParentOutIds <- mapM mkVHDLId parentOutIds
  let assocs =  genAssocElems 
                  ([resetId, clockId] ++ vParentInIds ++ vParentOutIds)
                  ([resetId, clockId] ++ ins          ++ map fst typedOuts)
      instantiation = CompInsSm vPid (IUEntity vParentId) assocs
  return (instantiation, decs)


-- | Translate a VHDL Signal to a VHDL Signal declaration
transVHDLName2SigDec ::  VHDLName -- ^ Signal name 
             -> TypeRep -- ^ Type of the intermediate signal 
             -> Maybe TH.Exp -- ^ Maybe an initializer expression for the signal
             -> EProne SigDec
transVHDLName2SigDec vId tr mExp = do
 tm <- transTR2TM tr
 mVExp <- DT.mapM transExp2VHDL mExp
 return $ SigDec vId tm mVExp




-- | Translate an intermediate Signal to a VHDL Signal declaration
transIntSignal2SigDec ::  IntSignalInfo -- ^ Intermediate signal information 
             -> TypeRep -- ^ Type of the intermediate signal 
             -> Maybe TH.Exp -- ^ Maybe an initializer expression for the signal
             -> EProne SigDec
transIntSignal2SigDec (IntSignalInfo vId) = transVHDLName2SigDec vId

-- | Translate a VHDL identifier and a type to an interface signal declaration
transVHDLId2IfaceSigDec :: Mode -> VHDLId -> TypeRep -> EProne IfaceSigDec
transVHDLId2IfaceSigDec m vid trep = do           
 tm  <- transTR2TM trep
 return $ IfaceSigDec vid m tm
 
 
-- | Translate a Port to a VHDL Interface signal declaration
transPort2IfaceSigDec :: Mode -> PortId -> TypeRep -> EProne IfaceSigDec
transPort2IfaceSigDec m pid trep = do           
 sid <- transPortId2VHDL pid
 transVHDLId2IfaceSigDec m sid trep
 
-- | Translate a system identifier to a VHDL identifier
transSysId2VHDL :: SysId -> EProne VHDLId
transSysId2VHDL = transPortId2VHDL

-- | Translate a process identifier to a VHDL identifier
transProcId2VHDL :: ProcId -> EProne VHDLId
transProcId2VHDL = transPortId2VHDL

-- | translate a port identifier to a VHDL Identifier
transPortId2VHDL :: PortId -> EProne VHDLId
transPortId2VHDL str = do let strL = map toLower str
                          when (elem strL reservedStrs)
                               (throwError (ReservedId str)) 
                          mkVHDLId str
                 

-- | translate a 'TypeRep' to a VHDL 'TypeMark'
-- We don't distinguish between a type and its version nested in 'Signal'
-- since it makes no difference in VHDL
transTR2TM :: TypeRep -> EProne TypeMark
transTR2TM rep
 | isSignal = transTR2TM $ nestedTR
 | rep == boolTR  = return std_logicTM
 | rep == intTR = return int32TM
 | otherwise = throwError (UnsupportedType rep)
 where intTR  = typeOf (undefined :: Int)
       boolTR = typeOf (undefined :: Bool) 
       (isSignal, nestedTR) = let (tc,~[tr]) = splitTyConApp rep
                              in  (tc == signalTyCon, tr)
       signalTyCon = (typeRepTyCon.typeOf) (undefined :: Signal ())

-- | Translate a Haskell expression to a VHDL expression
transExp2VHDL :: TH.Exp -> EProne VHDL.Expr
transExp2VHDL = undefined

-- | Translate a typed function ast to VHDL
transProcFun2VHDL :: TypedProcFunAST  -- ^ input ast
    -> EProne (SubProgBody, VHDLName, [VHDLName], [TypeMark], TypeMark)
    -- ^ Function, Function name, name of inputs, type of inputs, return type 
   
transProcFun2VHDL (TypedProcFunAST trep ast) = do
 let (argsTR, retTR) = unArrowT trep
 -- Get the interface
 argsTM <- mapM transTR2TM argsTR
 retTM <- transTR2TM retTR
 (fName, pars, body) <- checkProcFunSpec (length argsTM) ast
 let iface = zipWith IfaceVarDec pars argsTM
     fSpec = Function fName iface retTM
     -- FIXME, translate the body
     fBody = SubProgBody fSpec []
 return (fBody, fName, pars, argsTM, retTM)
-- | Check if the process function spec fullfils the translating constraints
--   Translate the name of a Process function and its formal parameters
checkProcFunSpec ::  Int -- ^ expected number of parameters
          -> ProcFunAST -- ^ the function AST
          -> EProne (VHDLName, [VHDLName], TH.Body)
-- FIXME: custom errors + handle default parameters
checkProcFunSpec expectArgN (ProcFunAST thName [Clause pats bdy []] _)= do
 fName <-  thName2VHDL thName
 parNames <- mapM getParName pats
 when (length parNames /= expectArgN) 
   (throwError $ Other "incorrect arg number")
 return (fName, parNames, bdy)
 where getParName (VarP name) = thName2VHDL name
       getParName _ = throwError $ Other "incorrect parameter"
       thName2VHDL = mkVHDLId.nameBase 

checkAST _ = throwError $ Other "incorrect function"
  
   