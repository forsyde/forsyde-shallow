{-# OPTIONS_GHC -fth #-}
-- -fth is necessary due to the use of template haskell syntax
--  in the translation tables

module HD.VHDL (writeVHDL) where

import HD.Misc
import HD.Types
import HD.HDSignal
import HD.HDFun
import HD.Port
import HD.VHDL.AST
import qualified HD.VHDL.Ppr as VHDLPpr
-- qualified to avoid clashes with Language.Haskell.TH.Ppr
import HD.Netlist

import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)
import Control.Monad.State
import System.IO
import Text.PrettyPrint.HughesPJ 

import qualified Language.Haskell.TH.Syntax as THAST 
-- qualified to avoid confusion with HD.VHDL.AST which has similar types
import qualified Language.Haskell.TH.Ppr as THPpr
-- qualified to avoid clashes with HD.VHDL.Ppr

-- TODO: create a BlockWalker class so that backends are easier to code
-- FIXME: Block Instances not supported yet
-- FIXME: The functions are too damn long, refactor them


writeVHDL :: Block -> IO ()
-- Given a block whose name is a valid VHDL _basic_ identifier (call it "A") 
-- generate A.vhd in current working directory
-- Imp: the input and output signal names of A must be valid VHDL identifiers
--      (basic or extended) and different to "clk" and "reset" 
--      which are reserved for the main clock and reset signal
writeVHDL  block@(Block name _ _) = 
 do let entityId = fromEProne ("blockname" ++ name) (mkVHDLBasicId name)
        fName    = name ++ ".vhd"
    putStr $ "Writing VHDL code to " ++ fName ++ " ... "
    handle     <- openFile fName WriteMode 
    designFile <- mkDesignFile entityId block
    hPutStr handle $ (render . VHDLPpr.ppr) designFile
    putStrLn "done!"


mkDesignFile :: VHDLId -> Block -> IO DesignFile 
-- Gets a  block and creates its design file
mkDesignFile eId (Block _ (InPort ip) (OutPort op)) =
 do let entityDec = mkEntityDec eId (portDesc ip) (portDesc op)
        signalL = [signal | (_,_,signal) <- op] 
    nlState <- execStateT (netlist new define (return signalL)) initNLState
    let archBody  = mkArchBody eId nlState
    return  $ DesignFile contextClause [LUEntity entityDec,LUArch archBody]


contextClause :: [ContextItem]
-- Create the import list
contextClause = [Library (unsafeVHDLBasicId "forsyde"), 
                 Library (unsafeVHDLBasicId "ieee"   ),
                 Use     "ieee.std_logic_1164.all"   ]

mkEntityDec :: VHDLId -> PortDesc -> PortDesc -> EntityDec
-- Create the declaration of the entity
mkEntityDec eId iPD oPD = EntityDec eId iface
-- We can use unsafeVHDLBasicId because the signal identifiers are already
-- checked while traversing the graph
 where iface = map (mkDec In ) iPD ++
               map (mkDec Out) oPD ++
               [IfaceSigDec clkId   In std_logicTM,
                IfaceSigDec resetId In std_logicTM]
       mkDec mode (id,hdType) = IfaceSigDec 
                                 (unsafeVHDLBasicId id) 
                                 mode 
                                 (hDST2TM.HDSignalType $ hdType)
  

mkArchBody :: VHDLId -> NLState -> ArchBody
-- Create the architecture body
mkArchBody eId NLState{archDecs=aDecs,archSms=aSms} =
  ArchBody (unsafeVHDLBasicId "Structural") eId aDecs aSms



-- NetList generation State,
-- We keep the declarations and statements of the architecture
-- which will describe the block in VHDL
-- We keep track as well of the number of components used
-- in order to label them in a more intuitive one for the potential code reader
data NLState = 
     NLState {
              archDecs :: [BlockDecItem], -- architecture declarations 
              archSms  :: [ConcSm]      , -- architecture statements
              nMap     :: Int           , -- number of map components
              nZipWith :: Int           , -- number of zip components
              nDelay   :: Int           , -- number of delay compoments
              nBI      :: [(VHDLId,Int)]} -- number of BockInstances
                                          --  given the name of the Block
                                          --  which they represent

initNLState = NLState [] [] 0 0 0 []

-- Netlist monad
-- FIXME: it should be defined in Netlist.hs
type NLMonad a = StateT NLState IO a 

-- Tag used for the nodes during the netlist generation
-- It represents the signal coming out from the node 
-- and the label of the component instantiation or block
-- to which that signal belongs, if it really belongs to any 
-- (i.e. it doesn't apply to InputS or OutPortS)
-- FIXME: The maybes are quite ugly
-- FIXME: Why putting the label when the block or instantiations can be put
--        directly instead
data NodeTag = NodeTag {sId    :: VHDLId,
                        sType  :: TypeMark,
                        label  :: Maybe Label}  




new :: NLMonad HDPrimSignal -> NLMonad NodeTag
new signalM = 
 do signal <- signalM
    state <- get 
    let node = unwrap signal
        t    = (hDST2TM.typeOfS) signal
    case node of
     InPortS str      -> 
       do let id = fromEProne "" (checkId str) 
          return  $ NodeTag id t Nothing
     -- UndefS should only be a graph root, and it doesn't request
     -- info about itself in define. Thus, no one should do it unless it 
     -- is an error
     -- Note: due to Haskell's laziness this error can only be trigered by
     --       define
     -- FIXME: this is ugly, change types to avoid these problems
     UndefS  -> 
       return $ error "Internal error: UndefS doesn't provide node info"
     OutPortS str _ -> 
       do let id = fromEProne "" (checkId str) 
          return  $ NodeTag id t Nothing
     BlockInsOS _ _   -> 
       error "Instances not supported yet"
     MapSY  _ _  -> 
       do let count = nMap state
              id  = unsafeVHDLBasicId $ "mapSY_" ++ show count
              dec = BDISD $ SigDec id t
          put state{nMap=count+1,archDecs=dec : archDecs state}
          return $ NodeTag id t (Just id)
     ZipWithSY  _ _ _ -> 
       do let count = nZipWith state
              id = unsafeVHDLBasicId $ "zipWithSY_" ++ show count
              dec = BDISD $ SigDec id t
          put state{nZipWith=count+1,archDecs=dec : archDecs state}
          return $ NodeTag id t (Just id)
     DelaySY _ _      -> 
       do let count = nDelay state
              id = unsafeVHDLBasicId $ "delaySY_" ++ show count
              dec = BDISD $ SigDec id t
          put state{nDelay=count+1,archDecs=dec : archDecs state}
          return $ NodeTag id t (Just id)

define :: NLMonad (HDSignalType,NodeTag) -> S NodeTag -> NLMonad ()
define rootInfo root =  
 do (t,~(NodeTag rId rT rLabel)) <- rootInfo
    state <- get  
    -- Note that the lazy pattern match is due to UndefS's error in new
    case root of
     InPortS _ -> do liftIO (putStr "llega una vez\n")
                     return ()
     UndefS    -> return ()
     -- FIXME: remove intermediate signals 
     OutPortS _ NodeTag{sId=cId} -> 
       do let assign = rId :<==: ConWforms [] (Wform [PrimName cId]) Nothing 
          put state{archSms = CSSASm assign : archSms state}
          return ()
     -- FIXME: implement instances
     BlockInsOS _ _   ->  error "Instances not supported yet"
     MapSY  f NodeTag{sId=cId,sType=cT}   -> 
       do let block = mkMapBlock (fromJust rLabel) cId cT rT rId f 
          put state{archSms = CSBSm block : archSms state}          
     ZipWithSY f NodeTag{sId=cId1,sType=cT1} NodeTag{sId=cId2,sType=cT2} -> 
       do let block = 
                mkZipWithBlock (fromJust rLabel) cId1 cT1 cId2 cT2 rT rId f 
          put state{archSms = CSBSm block : archSms state}          
     DelaySY c NodeTag{sId=cId,sType=cT} ->
       do let block = mkDelayBlock (fromJust rLabel) cId cT rT rId c 
          put state{archSms = CSBSm block : archSms state}


mkMapBlock :: Label-> 
              VHDLName -> TypeMark -> 
              VHDLName -> TypeMark -> 
              HDPrimFun ->
              BlockSm 
mkMapBlock label iId iT oT oId f = 
  BlockSm label iface (PMapAspect assocs) [BDISPB fVHDL] [sigAssign]
 where formalIn  = unsafeVHDLBasicId "mapSY_input"
       formalOut = unsafeVHDLBasicId "mapSY_output"
       iface = [IfaceSigDec formalIn  In  iT,
                IfaceSigDec formalOut Out oT] 
       assocs = [Just formalIn  :=>: ADName iId,
                 Just formalOut :=>: ADName oId]
       (fVHDL,fId,[parId]) = fromEProne "in mapSY" (hDPrimFun2VHDL f)
       sigAssign = CSSASm (formalOut :<==: (ConWforms [] wform Nothing))
       wform = Wform [PrimFCall $ FCall fId [Just parId :=>: ADName formalIn] ]

mkZipWithBlock :: Label-> 
                  VHDLName -> TypeMark -> 
                  VHDLName -> TypeMark -> 
                  VHDLName -> TypeMark -> 
                  HDPrimFun ->
                  BlockSm 
mkZipWithBlock label iId1 iT1 iId2 iT2 oT oId f = 
  BlockSm label iface (PMapAspect assocs) [BDISPB fVHDL] [sigAssign]
 where formalIn1 = unsafeVHDLBasicId "zipWithSY_input1"
       formalIn2 = unsafeVHDLBasicId "zipWithSY_input2"
       formalOut = unsafeVHDLBasicId "zipWithSY_output"
       iface = [IfaceSigDec formalIn1 In  iT1,
                IfaceSigDec formalIn2 In  iT2,
                IfaceSigDec formalOut Out oT ] 
       assocs = [Just formalIn1 :=>: ADName iId1,
                 Just formalIn2 :=>: ADName iId2, 
                 Just formalOut :=>: ADName oId ]
       (fVHDL,fId,[parId1,parId2]) = 
           fromEProne "in zipWithSY" (hDPrimFun2VHDL f)
       sigAssign = CSSASm (formalOut :<==: (ConWforms [] wform Nothing))
       wform = Wform [PrimFCall $ FCall fId [Just parId1 :=>: ADName formalIn1,
                                             Just parId2 :=>: ADName formalIn2]
                     ]

mkDelayBlock :: Label-> 
                VHDLName -> TypeMark -> 
                VHDLName -> TypeMark -> 
                HDPrimConst ->
                BlockSm 
mkDelayBlock label iId iT oT oId c = 
  BlockSm label iface (PMapAspect assocs) [] [sigAssign]
 where formalIn  = unsafeVHDLBasicId "delaySY_input"
       formalOut = unsafeVHDLBasicId "delaySY_output"
       iface = [IfaceSigDec resetId   In  std_logicTM,
                IfaceSigDec clkId     In  std_logicTM, 
                IfaceSigDec formalIn  In  iT       ,
                IfaceSigDec formalOut Out oT       ] 
       assocs = [Just resetId   :=>: ADName resetId,
                 Just clkId     :=>: ADName clkId  ,
                 Just formalIn  :=>: ADName iId    ,
                 Just formalOut :=>: ADName oId    ]
       sigAssign = CSSASm (formalOut :<==: 
                           (ConWforms [whenElseReset] inWform (Just whenRE)))
       whenElseReset = WhenElse (Wform [hDPC2Expr c]) 
                                (PrimName resetId :=: PrimLit "'0'")
       inWform = Wform [PrimName formalIn]
       whenRE = When (PrimFCall $ 
                      FCall (unsafeVHDLBasicId "rising_edge") 
                            [Nothing :=>: ADName clkId])
                                            

----------------------------------
-- Translating a HDPrimFun to VHDL
----------------------------------


hDPrimFun2VHDL :: HDPrimFun -> EProne (SubProgBody, VHDLName, [VHDLName])
-- It retuns the subprogram body the function name, and its parameter names 
-- or an error
hDPrimFun2VHDL (HDPrimFun fName fType 
                (THAST.Clause pats (THAST.NormalB exp) [])) = 
 do fVHDLName                        <- mkVHDLId fName +! fNameErr
    (fSpec, parVHDLNames, parTHNames)<- mkFunSpec fVHDLName fType pats 
                                         +! idErr
    fBodySm                          <- evalStateT (mkFunBodySm exp) parTHNames
    return (SubProgBody fSpec [fBodySm], fVHDLName, parVHDLNames)
  where fNameErr = "incorrect function name " ++ fName
        idErr = "incorrect parameter name in function " ++ fName



mkFunSpec :: VHDLName -> HDFunType -> [THAST.Pat] -> 
             EProne (SubProgSpec, [VHDLName], [THAST.Name])
-- Gets a function name, its type and its formal parameters (in TH
-- representation) and returns the function specification and the name
-- of its parameters (both in VHDL and TH representation) 
-- or an error if there was a syntactically incorrect parameter
mkFunSpec fName (HDFunType types) pars =
 do let (parTypes,retType) = initLast types
    (iface, vHDLNames, tHNames) <- mkIface pars parTypes 
    return (Function fName iface (getVHDLType retType), vHDLNames, tHNames)
  where mkIface :: [THAST.Pat] -> [HDType] -> 
                   EProne ([IfaceVarDec], [VHDLName], [THAST.Name])
        mkIface [] [] = return ([],[],[])
        mkIface (par:restPars) (parType:restTypes) =
          do (varDecs, vHDLNames, tHNames) <- mkIface restPars  restTypes
             (varDec,vHDLName,tHName) <-  mkIfaceVarDec par parType
             return (varDec:varDecs, vHDLName:vHDLNames, tHName:tHNames)
        mkIfaceVarDec :: THAST.Pat -> HDType -> 
                         EProne (IfaceVarDec, VHDLName, THAST.Name)
        mkIfaceVarDec (THAST.VarP varN) varT =
         do let varNStr = THAST.nameBase varN 
            varVHDLN <- checkId varNStr +! ("incorrect parameter " ++ varNStr)
            return (IfaceVarDec varVHDLN (getVHDLType varT), varVHDLN, varN)
        getVHDLType = hDST2TM.HDSignalType
        


-- Translation table of valid constructors
validCons :: [(THAST.Exp, Expr)]
validCons = [($(mkMetaAST True),  PrimLit "'1'"),
             ($(mkMetaAST False), PrimLit "'0'")]

-- Translation table of valid infix operators
validInfixOps :: [(THAST.Name,(Expr -> Expr -> Expr))]
validInfixOps = [('(&&) , And   ),
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
                 

-- Translation table of valid unary operators
validUnaryOps :: [(THAST.Name,(Expr -> Expr))]
validUnaryOps = [('not   , Not  ),
                 ('negate, Neg  ),
                 ('abs   , Abs  )]
                 

-- Function body translation state
-- List of local valid names
type FTState = [THAST.Name]

-- Error prone function translation state monad 
-- it is both a state monad (including FTState)
-- and an Error monad (like EProne from Misc.hs)
type EProneTS a = StateT FTState (Either String) a

mkFunBodySm :: THAST.Exp -> EProneTS SeqSm
-- Create the unique statement of a function in VHDL from a TH expression.
mkFunBodySm  inf@(THAST.CondE condE thenE  elseE)  = 
  do condVHDLE  <- mkVHDLExpr condE   
     thenVHDLSm <- mkFunBodySm thenE 
     elseVHDLSm <- mkFunBodySm elseE 
     return (IfSm condVHDLE [thenVHDLSm] [] (Just $ Else [elseVHDLSm]))
mkFunBodySm  inf@(THAST.CaseE caseE matches)  = 
  do caseVHDLE  <- mkVHDLExpr caseE 
     caseSmAlts <- mapM  mkVHDLCaseSmAlt matches
     return (CaseSm caseVHDLE caseSmAlts)
-- In other case it is an expression returned directly
mkFunBodySm  e = 
  do vHDLe <- mkVHDLExpr e 
     return (ReturnSm $ Just vHDLe)

mkVHDLCaseSmAlt :: THAST.Match -> EProneTS CaseSmAlt
-- Translate a case alternative from Haskell to VHDL
mkVHDLCaseSmAlt (THAST.Match pat (THAST.NormalB exp) []) =
 do sm <- mkFunBodySm exp
    case pat of
     THAST.WildP -> return $ CaseSmAlt [Others] [sm] 
     THAST.LitP  lit -> do vHDLExp <- mkVHDLExpr exp
                           return $ CaseSmAlt [ChoiceE vHDLExp] [sm]


mkVHDLExpr :: THAST.Exp -> EProneTS Expr
-- Get a VHDL expression from a TH expression  

-- Is it an Infix operator?
mkVHDLExpr (THAST.AppE (THAST.AppE (THAST.VarE fName) arg1) arg2)
 | isJust maybeInfixOp =
  do vHDLarg1 <- mkVHDLExpr arg1
     vHDLarg2 <- mkVHDLExpr arg2
     return $ (fromJust maybeInfixOp) vHDLarg1 vHDLarg2
 where maybeInfixOp = lookup fName validInfixOps

-- Is it a unary operator?
mkVHDLExpr  (THAST.AppE (THAST.VarE fName) arg) | isJust maybeUnaryOp =
  do vHDLarg <- mkVHDLExpr arg
     return $ (fromJust maybeUnaryOp) vHDLarg      
 where maybeUnaryOp = lookup fName validUnaryOps

-- Application expression which doesnt belong to the accepted subset
mkVHDLExpr exp@(THAST.AppE _ _) = 
 throwError $ "expression doesn't belong to the accepted Haskell subset" ++ 
              (THPpr.pprint exp)

-- Local variable or unkown function
mkVHDLExpr  (THAST.VarE name) = 
  do -- get the list of valid local names from the state monad
     validLocalNames <- get
     let nameStr = THAST.nameBase name 
     if elem name validLocalNames 
       -- the parameter was checked in the interface and can be 
       -- securely transformed to a VHDLId bypassing checks
       then let  id = unsafeVHDLBasicId nameStr
            in   return $ PrimName id
       else throwError $ "unkown function " ++ show nameStr 

-- Constructor, (only constant constructors belonging to validCons are valid)
mkVHDLExpr  con@(THAST.ConE cName) 
  | isJust consTranslation = return $ fromJust consTranslation
  | otherwise = throwError $ "unkown constant-constructor " ++ 
                             (THAST.nameBase cName) 
    where consTranslation = lookup con validCons


-- Literals
mkVHDLExpr  (THAST.LitE (THAST.IntegerL integer))  = 
 return $ (hDPC2Expr.toPConstant) $ (fromIntegral integer :: Int)
mkVHDLExpr  (THAST.LitE (THAST.IntPrimL integer))  = 
  return $ (hDPC2Expr.toPConstant) $ (fromIntegral integer :: Int)

-- Infix expressions
mkVHDLExpr  (THAST.InfixE (Just argl) (THAST.VarE fName) (Just argr))  = 
  if isJust maybeInfixOp then
     do vHDLargl <- mkVHDLExpr argl
        vHDLargr <- mkVHDLExpr argr
        return $ (fromJust maybeInfixOp) vHDLargl vHDLargr
  else throwError $ "unkown infix operator: " ++ ((show.THAST.nameBase) fName)
 where maybeInfixOp = lookup fName validInfixOps



-------------------
-- Helper Functions
-------------------


checkId :: String -> EProne VHDLId
-- check if a string is a syntactically correct VHDL identifier 
-- and is not reserved
checkId id
 | elem lowId reservedSignals = throwError reservedErr
 | otherwise = mkVHDLId id +! id
 where lowId   = map toLower id
       reservedErr = id ++ " is a reserved identifier"
       

hDST2TM :: HDSignalType -> TypeMark
-- Tranlsate a signal type to VHDL
hDST2TM (HDSignalType Bool) = std_logicTM
hDST2TM (HDSignalType Int)  = unsafeVHDLBasicId "forsyde.types.int32"

hDPC2Expr :: HDPrimConst -> Expr
hDPC2Expr (HDIntConst i) = 
 PrimFCall $ FCall (unsafeVHDLBasicId "TO_SIGNED") 
  [Just (unsafeVHDLBasicId "ARG")  :=>: (ADExpr $ PrimLit (show i)),
   Just (unsafeVHDLBasicId "SIZE") :=>: (ADExpr $ PrimLit "32")     ]
hDPC2Expr (HDBoolConst True)  = PrimLit "'1'"
hDPC2Expr (HDBoolConst False) = PrimLit "'0'"


reset, clk :: String
reset = "resetn"
clk = "clk"

reservedSignals :: [String]
reservedSignals = map (map toLower) [clk,reset]

resetId, clkId :: VHDLId
resetId = unsafeVHDLBasicId reset
clkId = unsafeVHDLBasicId clk


std_logicTM :: TypeMark
std_logicTM = unsafeVHDLBasicId "std_logic" 
