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

import Control.Monad.State

-------------------------------------
-- How does the VHDL Backend work? --
-------------------------------------

-- The System Definition is translated to a VHDL Design File and written to 
-- disk.
--
-- The Design File will contain only two library units;
-- an Entity Declaration and an Architecture.
-- 1) The Entity Declaration can be obtained from the SysDef directly (without
--    traversing the netlist)
-- 2) The Architecture (or more specifically, its declarations  and
--    the statements) is obtained from the netlist by traversing it. 
--
-- The state of the traversal is composed by 
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
-- VHDLM --
-----------

-- | VHDL backend monad
type VHDLM a = TravSEIO VHDLTravST ContextErr a


----------------
-- VHDLTravST --
----------------

-- | VHDL traversing State. (see 'ForSyDe.Netlist.Traverse.traverseSIO')
data VHDLTravST = VHDLTravST
  {currSysDef  :: SysDefVal, -- System definition which is currently 
                             -- being compiled
   res         :: TravResult, -- Result accumulated during the 
                              -- traversal of the System Definition netlist
   constNum    :: Int,       -- Number of contants found
   ops         :: VHDLOps,   -- Compilation options
   compSysDefs :: URefTableIO SysDefVal ()} -- Table indication which 
                                            -- System Definitions
                                            -- where already compiled

-- | Empty initial traversing state 
initVHDLTravST :: SysDefVal -> IO VHDLTravST
initVHDLTravST sysDefVal = do 
 t <- newURefTableIO
 return $ VHDLTravST sysDefVal emptyTravResult 0  defaultVHDLOps t

-------------
-- TravResult
-------------

-- | Result accumulated during the traversal of a netlist
data TravResult = TravResult 
  {archDecs  :: [BlockDecItem], -- generated architecture declarations 
   archSms   :: [ConcSm]      } -- generated architecture statements

-- | Set the Design File information inside the VHDL monad
setTravResult :: TravResult -> VHDLM ()
setTravResult travRes = modify (\st -> st{res=travRes})

-- | empty VHDL compilation Result
emptyTravResult :: TravResult
emptyTravResult = TravResult [] []

----------
-- VHDLOps
----------

-- | VHDL Compilation options
data VHDLOps = VHDLOps VHDLDebugLevel
 deriving (Eq, Show)

-- | Debug level
data VHDLDebugLevel = Normal | Verbose
 deriving (Eq, Ord, Show)

-- | Set VHDL options inside the VHDL monad
setVHDLOps :: VHDLOps -> VHDLM ()
setVHDLOps options =  modify (\st -> st{ops=options})


-- | Default traversing options
defaultVHDLOps :: VHDLOps
defaultVHDLOps =  VHDLOps Normal


-------------------------------------
-- Useful functions in the VHDL Monad
-------------------------------------

-- | Add a signal declaration to the 'TravResult' in the State
addSigDec :: SigDec -> VHDLM ()
addSigDec dec = modify (\st -> st{res= addDec' (res st)})
 -- FIXME: use a queue for the declarations
 where addDec' res =  res{archDecs=archDecs res ++ [BDISD dec]}


-- | Add a statement to the 'TravResult' in the State
addStm :: ConcSm -> VHDLM ()
addStm sm = modify (\st -> st{res= addStm' (res st)})
 -- FIXME: use a queue for the declarations
 where addStm' res =  res{archSms=archSms res ++ [sm]}

 
-- | Add an element to the 'SysDef' table in the state
addSysDef :: URef SysDefVal -> VHDLM ()
addSysDef ref = do table <- gets compSysDefs
                   lift.liftIO $ addEntryIO table ref () 

-- | Increment the number of constants found
incConstNum :: VHDLM ()
incConstNum = modify (\st -> st{constNum=constNum st + 1})


-- | Lift an 'EProne' value to the VHDL monad setting an empty context
--   for the error
-- FIXME: this function should maybe be moved to ForSyDeErr with a more general
--        type: MonadError m e => EProne a -> m a 
liftEProne :: EProne a -> VHDLM a
liftEProne ep = either (throwError.(ContextErr Empty)) return ep


-- | Lift an 'EProne' value to the VHDL monad setting the context of the
--   error as the system currently compiled
liftEProneSys :: EProne a -> VHDLM a
liftEProneSys ep = do 
 sys <- gets currSysDef
 either ( throwError . (ContextErr (SysDefC (sid sys) (loc sys))) )
        return ep
              
-- | Lift an 'EProne' value to the VHDL monad setting the context of the
--   error as a process in the system currently compiled
liftEProneProc :: ProcId -> EProne a -> VHDLM a
liftEProneProc pid ep = do
  sys <- gets currSysDef
  either ( throwError . (ContextErr (ProcC (sid sys) (loc sys) pid)) )
         return ep

-- | Throw an ForSyDe error, setting an empty context
-- FIXME: this function should maybe moved to ForSyDeErr
throwFError :: ForSyDeErr -> VHDLM a
throwFError = liftEProne.Left

-- | Throw an ForSyDe error, setting the context of the
--   error as the system currently compiled
throwFErrorSys :: ForSyDeErr -> VHDLM a
throwFErrorSys = liftEProneSys.Left

-- | Throw an ForSyDe error, setting the context of the
--   error as a process the system currently compiled
throwFErrorProc :: ProcId -> ForSyDeErr -> VHDLM a
throwFErrorProc pid = (liftEProneProc pid).Left


----------------
-- IntSignalInfo
----------------

-- | Intermediate signal information. Tag generated for each output of each
--   node found during the traversal. 
-- (see ForSyDe.Netlist.Traverse.traverseSIO).
--   It contains the VHDL intemediate signal name associated with the process 
--   output.
data IntSignalInfo = IntSignalInfo {sId    :: VHDLId}  

