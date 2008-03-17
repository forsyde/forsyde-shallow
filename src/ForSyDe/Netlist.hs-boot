-- Netlist.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it allows recursive imports in Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details
module ForSyDe.Netlist where

import ForSyDe.Ids

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

-- | A node of the netlist can be either a process, component instances or
-- special nodes to help traversing the graph.  
--
-- Since the netlist graph is implemented with trees, they only store
-- information about their inputs (which are their child nodes in the tree).

data  NlNode inputi


-- | A process node
data NlProc inputi 

---------
-- NlEdge
---------

-- | The node connection is carried out by directed edges modelled as 
--   Unsafe Unmutable References (allowing to share nodes) connected 
--   in the output->input direction (remember we are using trees). 
--   Since the node to which the edge is directed can have various outputs 
--   (e.g a system instance) the edge is tagged to indicate to what 
--   output it refers to.
data NlEdge node 

-- | The different outputs which the different nodes can have
data NlNodeOut             

instance Show NlNodeOut

 

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

-------------------
-- Helper functions
-------------------

-- | Generate a signal pointing to an 'InPort' node
newInPort :: PortId -> NlSignal

