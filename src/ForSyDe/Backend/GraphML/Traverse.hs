-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.GraphML.Traverse
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides specialized Netlist traversing functions aimed at
-- GraphML compilation.
-----------------------------------------------------------------------------
module ForSyDe.Backend.GraphML.Traverse 
 (writeGraphMLM,
  module ForSyDe.Backend.GraphML.Traverse.GraphMLM) where

import ForSyDe.Backend.GraphML.Traverse.GraphMLM
import ForSyDe.Backend.GraphML.FileIO
import ForSyDe.Backend.GraphML.AST


import ForSyDe.System.SysDef
import ForSyDe.Netlist.Traverse
import ForSyDe.Netlist
import ForSyDe.OSharing

import Data.Traversable.GenericZipWith

import System.Directory
import System.FilePath
import Control.Monad.State


-- | Internal GraphML-Monad version of 'ForSyDe.Backend.writeGraphML
writeGraphMLM :: GraphMLM ()
writeGraphMLM = do
    -- create and change to systemName/graphml
   rootDir <- gets (sid.globalSysDef.global)
   let graphmlDir = rootDir </> "graphml"
   liftIO $ createDirectoryIfMissing True graphmlDir
   liftIO $ setCurrentDirectory graphmlDir
   -- write the local results for the first-level entity
   writeLocalGraphMLM
   -- if we are in recursive mode, also write the local results
   -- for the rest of the subsystems
   rec <- isRecursiveSet
   when rec $ do subs <- gets (subSys.globalSysDef.global)
                 let writeSub s = 
                        withLocalST (initLocalST ((readURef.unPrimSysDef) s))
                                    writeLocalGraphMLM 
                 mapM_ writeSub subs
   -- go back to the original directory
   liftIO $ setCurrentDirectory (".." </> "..")


-- | Traverse the netlist and write the local results (i.e. system graphs)
writeLocalGraphMLM :: GraphMLM ()
writeLocalGraphMLM = do
  lSysDefVal <- gets (currSysDef.local)
  let lSysDefId =  sid lSysDefVal
  debugMsg $ "Compiling system definition `" ++ lSysDefId ++ "' ...\n"
  -- Obtain the output Nodes of the system
  -- Obtain the netlist of the system definition 
  let nl = netlist lSysDefVal
  -- Traverse the netlist, and get the traversing results
  intOutsInfo <- traverseGraphML nl 
  LocalTravResult nodes edges <- gets (localRes.local)
  -- For each output signal, we need a node and an edge between its 
  -- intermediate signal and the final output signal declared in system 
  -- interface.
  let outIds = map fst (oIface lSysDefVal)
      outNodes = map (\id -> OutNode id (id ++ "_in")) outIds
      outEdges = 
         zipWith (\(IntSignalInfo nId pId) id -> 
                   GraphMLEdge nId pId id (id ++ "_in")) intOutsInfo outIds
 -- Generate the final Graph 
      finalGraph = GraphMLGraph lSysDefId (nodes ++ outNodes) 
                                          (edges ++ outEdges)  
  -- and write it to disk
  liftIO $ writeGraph finalGraph (lSysDefId ++ ".graphml") 

-- | Traverse the netlist of a System Definition, 
--   returning the (implicit) final traversing state and a list
--   containing the 'IntSignalInfo' of each output of the system
traverseGraphML :: Netlist [] -> GraphMLM [IntSignalInfo]
traverseGraphML = traverseSEIO newGraphML defineGraphML

-- | \'new\' traversing function for the GraphML backend
newGraphML :: NlNode NlSignal -> GraphMLM [(NlNodeOut, IntSignalInfo)]
newGraphML node = do
   let id = case node of
             InPort id  -> id 
             Proc pid _ -> pid
     
   return $ zipWith (\tag n -> (tag, IntSignalInfo id (id ++ "_out" ++ show n)))
                    (outTags node) [(1::Int)..]
       

       
-- | \'define\' traversing function for the GraphML backend
defineGraphML :: [(NlNodeOut, IntSignalInfo)] 
             -> NlNode IntSignalInfo 
             -> GraphMLM ()
defineGraphML outs ins = do 
 let id = case ins of
           InPort id  -> id 
           Proc pid _ -> pid
     -- Formal input signals of the proces
     formalInL = [id ++ "_in" ++ show n | n <- [(1::Int)..]]
     -- Actual input signals of the process
     actualInL = arguments ins
     -- Generate the input edges of the node 
     inEdges = zipWith (\(IntSignalInfo aNid aPid) fPid  ->
                         GraphMLEdge aNid aPid id fPid ) actualInL formalInL
     -- Generate the graph node
     -- Formal output ports of the process
     outPids = map (\(_, IntSignalInfo _ pid) -> pid) outs
     -- Substitute actual inputs by formal inputs in "ins"
     insFormal = zipWithTF (\_ i -> i) ins formalInL
     node = ProcNode insFormal outPids
 mapM_ addEdge inEdges
 addNode node




