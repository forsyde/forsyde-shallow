-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.AST
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- AST covering the GraphML subset we are interested in for this backend
-----------------------------------------------------------------------------

--FIXME: the design of this module is ugly

module ForSyDe.Backend.GraphML.AST where

import ForSyDe.Netlist

type GraphMLGraphId = String
type GraphMLPortId = String
type GraphMLNodeId = String

-- | Main AST type, a graph
data GraphMLGraph = GraphMLGraph 
        GraphMLGraphId -- Graph id
        [GraphMLNode] -- Nodes 
        [GraphMLEdge] -- Edges


-- | Edge
data GraphMLEdge = GraphMLEdge 
  GraphMLNode    -- Origin node
  GraphMLPortId  -- Origin port id
  GraphMLNode    -- Target node
  GraphMLPortId  -- Target port id

-- | Node
data GraphMLNode = 
 ProcNode (NlNode GraphMLPortId) -- Netlist node indicating the input ports
                 [GraphMLPortId]  -- Output ports 
 -- FIXME: This representation is ugly
                                  |
 OutNode GraphMLNodeId GraphMLPortId  -- the special output node      
