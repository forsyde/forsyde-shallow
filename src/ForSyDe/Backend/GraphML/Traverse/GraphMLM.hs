{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.Traverse.GraphMLM
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- 'GraphMLM' (GraphML Monad), related types and functions
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.Traverse.GraphMLM where

import ForSyDe.Backend.GraphML.AST

import ForSyDe.Ids
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef (SysDefVal(..))
import ForSyDe.Netlist.Traverse (TravSEIO)

import Control.Monad.State


--------------
-- GraphMLM --
--------------

-- | GraphMLM backend monad
type GraphMLM a = TravSEIO GraphMLTravST ContextErr a


-------------------
-- GraphMLTravST --
-------------------

-- | GraphML traversing State. (see 'ForSyDe.Netlist.Traverse.traverseSIO')
data GraphMLTravST = GraphMLTravST
  {local  :: LocalGraphMLST,   -- Local State (related to the system currently 
                               -- compiled)
   global :: GlobalGraphMLST}  -- Global state (related to all systems being 
                               -- recursively compiled)   

data LocalGraphMLST = LocalGraphMLST
   {currSysDef :: SysDefVal, -- System definition which is currently 
                              -- being compiled
   context     :: Context,    -- Error Context
   localRes    :: LocalTravResult} -- Result accumulated during the 
                                   -- traversal of current System Definition 
                                   -- netlist




-- | initialize the local state
initLocalST :: SysDefVal -> LocalGraphMLST
initLocalST sysDefVal = 
 LocalGraphMLST sysDefVal (SysDefC (sid sysDefVal) (loc sysDefVal)) 
             emptyLocalTravResult

-- | Execute certain operation with a concrete local state.
--   The initial local state is restored after the operation is executed
withLocalST :: LocalGraphMLST -> GraphMLM a -> GraphMLM a
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

data GlobalGraphMLST = GlobalGraphMLST
  {globalSysDef :: SysDefVal,
   ops          :: GraphMLOps,   -- Compilation options
   globalRes    :: GlobalTravResult} -- Result accumulated during the 
                                     -- whole compilation
    



-- | Empty initial traversing state
initGlobalGraphMLST :: SysDefVal -> GlobalGraphMLST
initGlobalGraphMLST  sysDefVal = 
  GlobalGraphMLST sysDefVal defaultGraphMLOps emptyGlobalTravResult

-- | Empty initial traversing state 
initGraphMLTravST :: SysDefVal -> GraphMLTravST
initGraphMLTravST sysDefVal = 
  GraphMLTravST (initLocalST sysDefVal) (initGlobalGraphMLST sysDefVal)

-------------
-- TravResult
-------------

-- | Local result accumulated during the traversal of a netlist
data LocalTravResult = LocalTravResult 
  {nodes  :: [GraphMLNode], -- generated nodes
   edges  :: [GraphMLEdge]} -- generated edges



-- | empty local GraphML compilation result
emptyLocalTravResult :: LocalTravResult
emptyLocalTravResult = LocalTravResult [] []


-- | Global Results accumulated throughout the whole compilation
--   (empty right now)
type GlobalTravResult = ()


-- | empty global GraphML compilation result
emptyGlobalTravResult :: GlobalTravResult
emptyGlobalTravResult = ()


-------------
-- GraphMLOps
-------------

-- | GraphML Compilation options
data GraphMLOps = GraphMLOps 
   {debugGraphML :: GraphMLDebugLevel, 
    recursivityGraphML :: GraphMLRecursivity,
    yFilesMarkup :: Bool -- ^ Generate yFiles markup? 
                        }
 deriving (Eq, Show)

-- | Debug level
data GraphMLDebugLevel = GraphMLNormal | GraphMLVerbose
 deriving (Eq, Ord, Show)

-- | Print a message to stdout if in verbose mode
debugMsg :: String -> GraphMLM ()
debugMsg str = do
 debugLevel <- gets (debugGraphML.ops.global)
 when (debugLevel > GraphMLNormal) 
      (liftIO $ putStr ("DEBUG: " ++ str))

-- | Recursivity, should the parent systems of system instances be compiled as 
--   well?
data GraphMLRecursivity = GraphMLRecursive | GraphMLNonRecursive
 deriving (Eq, Show)

-- | Check if we are in recursive mode
isRecursiveSet :: GraphMLM Bool
isRecursiveSet = do 
  recOp <- gets (recursivityGraphML.ops.global)
  return $ recOp == GraphMLRecursive

-- | Check if we want to generate yFiles markup
genyFilesMarkup :: GraphMLM Bool
genyFilesMarkup = gets (yFilesMarkup.ops.global)


-- | Default traversing options
defaultGraphMLOps :: GraphMLOps
defaultGraphMLOps =  GraphMLOps GraphMLNormal GraphMLRecursive False


-- | Set GraphML options inside the GraphML monad
setGraphMLOps :: GraphMLOps -> GraphMLM ()
setGraphMLOps options =  modify (\st -> st{global=(global st){ops=options}})


----------------------------------------
-- Useful functions in the GraphML Monad
----------------------------------------

-- | Add a signal declaration to the 'LocalTravResult' in the State
addEdge :: GraphMLEdge -> GraphMLM ()
addEdge e = modify addFun 
 -- FIXME: use a queue for the declarations
  where addFun st = st{local=l{localRes=r{edges=edg ++ [e]}}}
         where l   = local st
               r   = localRes l
               edg = edges r 


-- | Add a statement to the 'LocalTravResult' in the State
addNode :: GraphMLNode -> GraphMLM ()
addNode node = modify addFun
 -- FIXME: use a queue for the statements
  where addFun st = st{local=l{localRes=r{nodes= nds ++ [node]}}}
         where l  = local st
               r  = localRes l
               nds = nodes r 



-- | Lift an 'EProne' value to the GraphML monad setting current error context
--   for the error
liftEProne :: EProne a -> GraphMLM a
liftEProne ep = do
 cxt <- gets (context.local)
 either (throwError.(ContextErr cxt)) return ep

-- | Throw a ForSyDe error, setting current error context
throwFError :: ForSyDeErr -> GraphMLM a
throwFError = liftEProne.Left



-- | Execute certain operation with a concrete process context.
--   The initial context is restored after the operation is executed
--   Note: the initial context must be a system context or 'InconsistenContexts'
--         will be raised.
withProcC :: ProcId -> GraphMLM a -> GraphMLM a
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




----------------
-- IntSignalInfo
----------------

-- | Intermediate edge information. Tag generated for each output of each
--   node found during the traversal. 
-- (see ForSyDe.Netlist.Traverse.traverseSIO).
--   It contains the GraphML node identifier 
--   and port identifier associated with the process output.

data IntSignalInfo = IntSignalInfo GraphMLNodeId GraphMLPortId 

