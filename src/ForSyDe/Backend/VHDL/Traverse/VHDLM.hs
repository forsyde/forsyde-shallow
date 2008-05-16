{-# LANGUAGE FlexibleContexts #-}
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
-- 'VHDM' (VHDL Monad), related types and functions
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Traverse.VHDLM where

import ForSyDe.Backend.VHDL.AST

import ForSyDe.Ids
import ForSyDe.ForSyDeErr
import ForSyDe.OSharing
import ForSyDe.System.SysDef (SysDefVal(..))
import ForSyDe.Netlist.Traverse (TravSEIO)
import ForSyDe.Process.ProcType (EnumAlgTy(..))

import Data.Generics (tyconUQname, tyconModule)
import Data.Maybe (fromJust)
import qualified Data.Set as S (filter)
import Data.Set (Set, union, empty, toList)
import Control.Monad.State
import Language.Haskell.TH (nameBase, nameModule, Name, Exp)
import Data.Typeable (TypeRep)

-------------------------------------
-- How does the VHDL Backend work? --
-------------------------------------

-- All the types used in the the System Defintion are translated to VHDL
-- put into Package, the into a Design File and written to disk.
--
-- The System Definition itself is translated to another VHDL Design File and 
-- written to disk.
--
-- This Design File will contain only two library units;
-- an Entity Declaration and an Architecture.
-- 1) The Entity Declaration can be obtained from the SysDef directly (without
--    traversing the netlist)
-- 2) The Architecture (or more specifically, its declarations  and
--    the statements) is obtained from the netlist by traversing it. 
--
-- The state of the traversal is composed by 
--  * list of type defintions translated during the traversal
--  * table of equivalence between Haskell types and the VHDL identifier
--    of its translated type (used to avoid translating the same type
--    multiple times)
--  * the list of declarations of the architecture
--  * the list of statements of the architecture 
--  * a table of System Definition references, used to keep track of the
--    system definitions (corresponding to one or more instances in the 
--    netlist) whose code was already generated.
--
-- For each process (netlist node) found during the traversal:
--    * A signal declaration is generated for each output and added to
--      the list of architecture declarations.
--    * A VHDL block including the translation of the process is generated
--      and added to the list of architecture statements.
--
-- In the special case of finding a System Instance
--      1) a port map statement is generated and added to the list of 
--         architecture statements.
--      2) the System Definition table is used to check if the Design File of
--         the System Definition associated with the instance was written to 
--         disk.
--      3) if the the associated System Definition wasn't in the table
--          1) generate and write to disk the corresponding Design File
--          2) add the System Definition to the table

-----------
-- SysLogic
-----------

-- | Indicates wether a system is combinational or sequential
--   In practice, a system is sequential if if contains a delay process or
--   a sequential system instance, otherwise its combinational.
data SysLogic = Combinational | Sequential
 deriving (Eq, Show)

-----------------
-- TransNameSpace
-----------------

-- | Translation namespace.
--
-- This type provides the number of fresh names already generated and a tranlsation 
-- table from Template Haskell Names to VHDL Names. It only makes sense
-- in a process-function context. 
data TransNameSpace = TransNameSpace 
    {freshNameCount :: Int,
     nameTable      :: [(Name, VHDLName)]}

emptyTransNameSpace :: TransNameSpace
emptyTransNameSpace = TransNameSpace 0 []

-----------
-- VHDLM --
-----------

-- | VHDL backend monad
type VHDLM a = TravSEIO VHDLTravST ContextErr a

----------------
-- VHDLTravST --
----------------

-- | VHDL traversing State. (see 'ForSyDe.Netlist.Traverse.traverseSIO')
data VHDLTravST = VHDLTravST
  {local  :: LocalVHDLST, -- Local State (related to the system currently 
                          -- compiled)
   global :: GlobalVHDLST}  -- Global state (related to all systems being 
                            -- recursively compiled)   

data LocalVHDLST = LocalVHDLST
   {currSysDef     :: SysDefVal, -- System definition which is currently 
                                 -- being compiled
   context         :: Context,  -- Error Context
   logic           :: SysLogic,  -- Current system is sequential or combinational?
   transNameSpace  :: TransNameSpace,  -- Translation namespace for functions
                                       -- It only makes sense
                                       -- in a process-function context 
   localRes        :: LocalTravResult} -- Result accumulated during the 
                                       -- traversal of current System Definition 
                                       -- netlist




-- | initialize the local state
initLocalST :: SysDefVal -> LocalVHDLST
initLocalST sysDefVal = 
 LocalVHDLST sysDefVal (SysDefC (sid sysDefVal) (loc sysDefVal)) Combinational 
             emptyTransNameSpace emptyLocalTravResult

-- | Execute certain operation with a concrete local state.
--   The initial local state is restored after the operation is executed
withLocalST :: LocalVHDLST -> VHDLM a -> VHDLM a
withLocalST l' action =  do
  -- get the initial local state
  st <- get
  let l = local st
  -- set the modified state
  put st{local=l'}
  -- execute the action
  res <- action
  -- restore the initial local state
  st' <- get 
  put st'{local=l}
  -- return the result
  return res

-- | Execute certain operation with an empty translation namespace
--   The initial namespace is restored after the operation is executed
withEmptyTransNameSpace :: VHDLM a -> VHDLM a
withEmptyTransNameSpace action = do
  -- get the initial name space
  st <- get
  let l = local st
      ns = transNameSpace l
  -- set the empty name space
  put st{local=l{transNameSpace=emptyTransNameSpace}}
  -- execute the action
  res <- action
  -- restore the initial name table
  st' <- get
  let l' = local st'
  put st'{local=l'{transNameSpace=ns}}
  -- return the result
  return res
      
            

data GlobalVHDLST = GlobalVHDLST
  {globalSysDef :: SysDefVal,
   ops          :: VHDLOps,   -- Compilation options
   globalRes    :: GlobalTravResult, -- Result accumulated during the 
                                    -- whole compilation
   compSysDefs  :: URefTableIO SysDefVal SysLogic, -- Table containing the
                                            -- System Definitions which
                                            -- where already compiled
                                            -- indicating what type of
                                            -- logic
   enumTypes    :: Set EnumAlgTy,          -- Set of the enumerated
                                           -- algebraic types accumulated
                                           -- by all ProcFuns and ProcVals
                                           -- in the system      
   typeTable    :: [(TypeRep, TypeMark)]} -- Type translation table


-- | Empty initial traversing state
initGlobalVHDLST :: SysDefVal -> IO GlobalVHDLST
initGlobalVHDLST  sysDefVal = do
 t <- newURefTableIO
 return $ GlobalVHDLST sysDefVal defaultVHDLOps emptyGlobalTravResult t empty []

-- | Empty initial traversing state 
initVHDLTravST :: SysDefVal -> IO VHDLTravST
initVHDLTravST sysDefVal = do 
 gst <- initGlobalVHDLST sysDefVal
 return $ VHDLTravST (initLocalST sysDefVal) gst

-------------
-- TravResult
-------------

-- | Local result accumulated during the traversal of a netlist
data LocalTravResult = LocalTravResult 
  {archDecs  :: [BlockDecItem], -- generated architecture declarations 
   archSms   :: [ConcSm]      } -- generated architecture statements



-- | empty local VHDL compilation result
emptyLocalTravResult :: LocalTravResult
emptyLocalTravResult = LocalTravResult [] []


-- | Global Results accumulated throughout the whole compilation
data GlobalTravResult = GlobalTravResult 
 {typeDecs      :: [TypeDec], -- Types translated during the traversal
  subProgBodies :: [SubProgBody] } -- Functions or procedures generated during
                                   -- the traversal



-- | empty global VHDL compilation result
emptyGlobalTravResult :: GlobalTravResult
emptyGlobalTravResult = GlobalTravResult [] []


----------
-- VHDLOps
----------

-- | VHDL Compilation options
data VHDLOps = VHDLOps {debugVHDL :: VHDLDebugLevel, recursivityVHDL :: VHDLRecursivity}
 deriving (Eq, Show)

-- | Debug level
data VHDLDebugLevel = VHDLNormal | VHDLVerbose
 deriving (Eq, Ord, Show)

-- | Print a message to stdout if in verbose mode
debugMsg :: String -> VHDLM ()
debugMsg str = do
 debugLevel <- gets (debugVHDL.ops.global)
 when (debugLevel > VHDLNormal) 
      (liftIO $ putStr ("DEBUG: " ++ str))

-- | Recursivity, should the parent systems of system instances be compiled as 
--   well?
data VHDLRecursivity = VHDLRecursive | VHDLNonRecursive
 deriving (Eq, Show)

-- | Check if we are in recursive mode
isRecursiveSet :: VHDLM Bool
isRecursiveSet = do 
  recOp <- gets (recursivityVHDL.ops.global)
  return $ recOp == VHDLRecursive


-- | Default traversing options
defaultVHDLOps :: VHDLOps
defaultVHDLOps =  VHDLOps VHDLNormal VHDLRecursive


-- | Set VHDL options inside the VHDL monad
setVHDLOps :: VHDLOps -> VHDLM ()
setVHDLOps options =  modify (\st -> st{global=(global st){ops=options}})


-------------------------------------
-- Useful functions in the VHDL Monad
-------------------------------------

-- | Set local System as sequential
setSeqCurrSys :: VHDLM ()
setSeqCurrSys = modify setSeq
 where setSeq st = st{local=(local st){logic=Sequential}}


-- | Add a signal declaration to the 'LocalTravResult' in the State
addSigDec :: SigDec -> VHDLM ()
addSigDec dec = modify addFun 
 -- FIXME: use a queue for the declarations
  where addFun st = st{local=l{localRes=r{archDecs=ads ++ [BDISD dec]}}}
         where l  = local st
               r  = localRes l
               ads = archDecs r 


-- | Add a statement to the 'LocalTravResult' in the State
addStm :: ConcSm -> VHDLM ()
addStm sm = modify addFun
 -- FIXME: use a queue for the statements
  where addFun st = st{local=l{localRes=r{archSms=aSms ++ [sm]}}}
         where l  = local st
               r  = localRes l
               aSms = archSms r 

 
-- | Add an element to the 'SysDef' table in the global state
addSysDef :: URef SysDefVal -> SysLogic -> VHDLM ()
addSysDef ref logic = do table <- gets (compSysDefs.global)
                         liftIO $ addEntryIO table ref logic 

-- | Find a previously translated custom type
lookupCustomType :: TypeRep -> VHDLM (Maybe SimpleName)
lookupCustomType rep = do
 transTable <- gets (typeTable.global)
 return $ lookup rep transTable


-- | Check if a SysDef was previously traversed returning its
--   type lof logic
traversedSysDef :: URef SysDefVal -> VHDLM (Maybe SysLogic)
traversedSysDef ref =  do table <- gets (compSysDefs.global)
                          liftIO $ queryIO table ref

-- | Add enumerated types to the global state
addEnumTypes :: Set EnumAlgTy -> VHDLM ()
addEnumTypes newETs = do
 globalST <- gets global 
 let oldETs = enumTypes globalST
 modify (\st -> st{global = globalST {enumTypes = oldETs `union` newETs}})
 
-- | Check if a Template haskell 'Name' corresponding to
--   a Enumerated-type data constructor is present in the enumerated
--   types accumulated in the global state and return the corresponding
--   VHDL identifier.
getEnumConsId :: Name -> VHDLM (Maybe VHDLId)
getEnumConsId consName = do
 let consModule = (fromJust.nameModule) consName
     consBase = nameBase consName
 enums <- gets (enumTypes.global)
 let matchesName (EnumAlgTy dataName enums) = 
                 tyconModule dataName == consModule && elem consBase enums
 case (toList.(S.filter matchesName)) enums of
   []  -> return Nothing
   [_] -> liftM Just (liftEProne $ mkVHDLExtId consBase) 
 
-- | Add a type declaration to the global results and type translation table
addTypeDec :: TypeRep -> TypeDec -> VHDLM ()
addTypeDec rep typeDec@(TypeDec id _) = do
 globalST <- gets global 
 let transTable = typeTable globalST
     gRes = globalRes globalST 
     tDecs =  typeDecs gRes
 -- FIXME: use queues
 modify (\st -> st{global = globalST
                             {typeTable = transTable ++ [(rep, id)],
                              globalRes = gRes{typeDecs = tDecs ++ [typeDec]}}})
  

-- | Add a subprogram to the global results
addSubProgBody :: SubProgBody -> VHDLM ()
addSubProgBody newBody = do
 globalST <- gets global 
 let gRes = globalRes globalST 
     bodies = subProgBodies gRes
 -- FIXME: use queues
 modify (\st -> st{global = globalST
                       {globalRes = gRes{subProgBodies = bodies ++ [newBody]}}})


-- | Add a TH-name VHDL-name pair to the translation namespace table
addTransNamePair :: Name -> VHDLName -> VHDLM ()
addTransNamePair thName vHDLName = do
 lState <- gets local
 let ns = transNameSpace lState
     table = nameTable ns
 modify (\st -> st{local=lState{transNameSpace=ns{
                                       nameTable=(thName,vHDLName):table}}})

-- | Get a fresh VHDL Identifier and increment the
--   tranlation-namespace-count of freshly generated identifiers.
--
--   Note that all user-supplied identifiers (process ids, port ids,
--   and function parameters) are translated to extended VHDL
--   identifiers. That, together with the fact that basic and extended
--   VHDL identifers live in different namespaces, guarantees that
--   freshly generated basic VHDL identifiers cannot clash with the
--   ones supplied by the frontend.
genFreshVHDLId :: VHDLM VHDLId
genFreshVHDLId = do
 lState <- gets local
 let ns = transNameSpace lState
     count = freshNameCount ns
 modify (\st -> st{local=lState{transNameSpace=ns{freshNameCount=count+1}}})
 return $ unsafeVHDLBasicId ("fresh_" ++ show count) 

-- | Lift an 'EProne' value to the VHDL monad setting current error context
--   for the error
-- liftEProne :: EProne a -> VHDLM a
liftEProne :: EProne a -> VHDLM a
liftEProne ep = do
 cxt <- gets (context.local)
 either (throwError.(ContextErr cxt)) return ep

-- | Throw a ForSyDe error, setting current error context
throwFError :: ForSyDeErr -> VHDLM a
throwFError = liftEProne.Left



-- | Execute certain operation with a concrete process context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a system context or 'InconsistenContexts'
--         will be raised.
withProcC :: ProcId -> VHDLM a -> VHDLM a
withProcC pid action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified name context
  put st{local=l{context=setProcC pid c}}
  -- execute the action
  res <- action
  -- restore the initial name context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res



-- | Execute certain operation with a concrete process function context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a process context or 
--         'InconsistenContexts' will be raised.
withProcFunC :: Name -> Loc -> VHDLM a -> VHDLM a
withProcFunC name loc action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified context
  put st{local=l{context=setProcFunC name loc c}}
  -- execute the action
  res <- action
  -- restore the initial context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res



-- | Execute certain operation with a concrete process function context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a process context or 
--         'InconsistenContexts' will be raised.
withProcValC :: Exp -> VHDLM a -> VHDLM a
withProcValC exp action = do
  -- get the initial context
  st <- get
  let l = local st
      c = context l
  -- set the modified context
  put st{local=l{context=setProcValC exp c}}
  -- execute the action
  res <- action
  -- restore the initial context
  st' <- get
  let l' = local st'
  put st'{local=l'{context=c}}
  -- return the result
  return res


----------------
-- IntSignalInfo
----------------

-- | Intermediate signal information. Tag generated for each output of each
--   node found during the traversal. 
-- (see ForSyDe.Netlist.Traverse.traverseSIO).
--   It contains the VHDL intemediate signal name associated with the process 
--   output.
type IntSignalInfo = SimpleName

