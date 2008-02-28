-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Netlist
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a definition for the netlist of a system description 
-- 
-- The netlist is modelled as a directed cyclic graph whose nodes 
-- are shared using Observable Sharing ("ForSyDe.OSharing").
--
-- /This module is based on Lava2000/: <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------

-- FIXME: WHy are we reexporting PortId?
module ForSyDe.Netlist  where


import ForSyDe.OSharing (URef, newURef, readURef)
import ForSyDe.System.SysDef 
 (SysDef(..), PrimSysDef(..), IFace, oIface)
import ForSyDe.Process.ProcFun (ProcFun(..))
import ForSyDe.ForSyDeErr
import ForSyDe.Process.ProcVal (ProcVal(..))


import Language.Haskell.TH (Type)
import Data.Dynamic
import Data.Maybe

-----------
-- Netlist
-----------


-- | The netlist of a system is modelled as a directed cyclic graph but is
-- really equivalently implemented as a set of (possibly overlapping) trees
-- capable of sharing nodes between them.
--
-- TODO, put sample image
--
-- There is no specific data structure for the netlist itself. Instead, a
-- netlist is simply represented by its outputs. Each output is the root node
-- of a tree ('NlTree'). The NlTrees can (and probably will) have common nodes.
--
-- It is important to note that, due to the use of trees, the only way to
-- traverse a 'Netlist' is from its outputs to its inputs.

newtype Netlist container = Netlist (container NlTree)



----------
-- NlNode
----------

-- FIXME: remove this or decide where to put it
type ProcId = String
type PortId = String

-- | A node of the netlist can be either a process, component instances or
-- special nodes to help traversing the graph.  
--
-- Since the netlist graph is implemented with trees, they only store
-- information about their inputs (which are their child nodes in the tree).

data  NlNode inputi =
             -- Ports
             InPort  PortId               | -- ^ Input Ports of the system 
             -- Constant signal 
             Const ProcVal                | -- ^ A signal with constant value
                                            --   During all its periods        
             -- Processes
             Proc ProcId (NlProc inputi)


-- | A process node
--   Note that vectors and transformed to lists puls an Int parameter 
--   indicating its size
data NlProc inputi = 
 ZipWithNSY (ProcFun Dynamic) -- Process function in dynamic form
            [inputi]                                | -- ^ mapSY and zipWithSY*
                                                      --   processes
 ZipWithxSY Int -- Size of input vectors (number of inputs)
            (ProcFun ([Dynamic] -> Dynamic)) -- Process function 
                                                  -- with dynamic arguments  
            ([inputi])                         | -- ^ Vector version 
                                                 --   of zipWithNSY
 UnzipNSY Int -- Number of outputs
          (Dynamic -> [Dynamic]) -- Dynamic version of the zipping function
                                 -- for the concrete, monomorphic types
                                 -- of the process
          inputi                                    | -- ^ Inverse of
                                                      -- ZipWithNSY
 UnzipxSY Int -- Size of output vectors (Number of outputs)
          (Dynamic -> [Dynamic])             
          inputi                                    | -- ^ Vector version
                                                      --   of UnzipSY 

 DelaySY    ProcVal    inputi                       | -- ^ delaySY process
 -- A System Instance is considered a special process
 -- FIXME: aren't the PortId elements redundant?
 SysIns PrimSysDef [(PortId, inputi)]                 -- ^ System Instance
 
         

---------
-- NlEdge
---------

-- | The node connection is carried out by directed edges modelled as 
--   Unsafe Unmutable References (allowing to share nodes) connected 
--   in the output->input direction (remember we are using trees). 
--   Since the node to which the edge is directed can have various outputs 
--   (e.g a system instance) the edge is tagged to indicate to what 
--   output it refers to.
data NlEdge node = NlEdge (ForSyDe.OSharing.URef node) 
                           NlNodeOut
                           Type


-- | The different outputs which the different nodes can have
data NlNodeOut = InPortOut        |
                 ConstOut         |
                 ZipWithNSYOut    |
                 ZipWithxSYOut    |
                 UnzipNSYOut Int  |
                 UnzipxSYOut Int  |
                 DelaySYOut       |
                 SysInsOut PortId 
 deriving (Show, Eq)                

-- | Get the output tags of a node
outTags :: NlNode a -> [NlNodeOut]
outTags (InPort  _) = [InPortOut]
outTags (Const _) = [ConstOut]
outTags (Proc _ proc) =
 case proc of
   ZipWithNSY _ _ -> [ZipWithNSYOut]
   ZipWithxSY _ _ _ -> [ZipWithxSYOut] 
   UnzipNSY nout _ _  -> map UnzipNSYOut [1..nout]
   UnzipxSY nout _ _  -> map UnzipxSYOut [1..nout]
   DelaySY    _ _  -> [DelaySYOut]
   SysIns primSysDefRef _ -> 
       map (SysInsOut . fst) ((oIface . readURef . unPrimSysDef) primSysDefRef)

 

------------
-- NlTree
------------

-- | We tie the knot to connect nodes through 'NlEdge's, building a Tree which
--   can have shared nodes.  
--
-- That is done by storing 'NlTree's as the the input information of each node
-- of the tree. Child nodes of the tree are closer to the inputs of the system
-- and parents are closer to the outputs.

newtype NlTree = NlTree  {rootEdge :: (NlEdge (NlNode NlTree))}

-----------
-- NlSignal
-----------

-- | A 'NlTree', or more preciselly its root 'NlEdge', is after all, how
-- signals are implemented in the netlist.
type NlSignal = NlTree

-- | Get the type of a signal
nlSignalType :: NlSignal -> Type
nlSignalType (NlTree (NlEdge _ _ t)) = t

-- | Get the tag of a signal
nlSignalNlOut :: NlSignal -> NlNodeOut
nlSignalNlOut (NlTree (NlEdge _ tag _)) = tag

-------------------
-- Helper functions
-------------------

-- FIXME: All these newWhatever functions probably shouldn't be here

-- | Generate a signal pointing to an 'InPort' node
newInPort :: PortId -> Type -> NlSignal
newInPort id t = NlTree (NlEdge (newURef (InPort id)) InPortOut t)

-- | Transform an interface into a list of input signals
iface2InPorts :: IFace -> [NlSignal]
iface2InPorts = map (uncurry newInPort)          


-- | Generate a reference to a new 'SysIns' node
newSysIns :: ProcId -> SysDef a -> [(PortId, NlSignal)] 
             -> URef (NlNode NlSignal)
newSysIns id (SysDef primSysDef) inputInfo = 
    newURef (Proc id (SysIns primSysDef inputInfo))   

-- | Generate the output signal of a node
newNodeOutSig :: URef (NlNode NlSignal)  -- ^ Reference to the node 
              -> NlNodeOut               -- ^ Output tag
              -> Type                    -- ^ Type of the output
              -> NlSignal
newNodeOutSig ref mTag t = NlTree (NlEdge ref mTag t)

-- | Create one of the output signals of a node
node2NlSignal :: URef (NlNode NlSignal) -> NlNodeOut -> Type -> NlSignal
node2NlSignal ref tag t = NlTree (NlEdge ref tag t)


-- | Evaluate the output of a process within a synchronous period
--   The inputs and outputs are given in dynamic form
eval :: NlNode Dynamic -> [(NlNodeOut, Dynamic)]
eval (InPort _)    = intError evalStr (EvalErr "InPort")
eval (Const pv)   = [(ConstOut, dyn pv)]
eval (Proc _ proc) = 
  case proc of
    ZipWithNSY fun dynArgs
      -> [(ZipWithNSYOut, foldl1 dynApp ((val fun) : dynArgs))]  
    ZipWithxSY _ fun dynListArg
      -> [(ZipWithxSYOut, (val fun) dynListArg)]
    UnzipNSY _ fun dynArg
      -> zipWith (\n dyn -> (UnzipNSYOut n, dyn))
                 [1..] 
                 (fun dynArg) 
    UnzipxSY _ fun dynArg
      -> zipWith (\n dyn -> (UnzipxSYOut n, dyn))
                 [1..] 
                 (fun dynArg) 
    DelaySY _ _                   
      -> intError evalStr (EvalErr "DelaySY")
    SysIns  _ _                   
      -> intError evalStr (EvalErr "SysIns")
     

-- FIXME: ugly string
evalStr = "ForSyDe.Netlist.eval"