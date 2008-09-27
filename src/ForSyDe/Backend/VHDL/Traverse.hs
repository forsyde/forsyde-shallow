-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Traverse
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides specialized Netlist traversing functions aimed at
-- VHDL compilation.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Traverse 
 (writeVHDLM,
  module ForSyDe.Backend.VHDL.Traverse.VHDLM) where

import ForSyDe.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Backend.VHDL.Translate
import ForSyDe.Backend.VHDL.Generate
import ForSyDe.Backend.VHDL.FileIO
import ForSyDe.Backend.VHDL.AST
import ForSyDe.Backend.VHDL.Quartus (callQuartus)
import ForSyDe.Backend.VHDL.Modelsim

import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import ForSyDe.Process.ProcVal
import ForSyDe.Process.ProcFun
import ForSyDe.Netlist.Traverse
import ForSyDe.Netlist
import ForSyDe.OSharing

import Control.Monad.State
import System.Directory
import System.FilePath
import Data.Maybe (fromJust, isJust)

-- | Internal VHDL-Monad version of 'ForSyDe.Backend.writeVHDL'
--   (Note: the initial and final CWD will be / )
writeVHDLM :: VHDLM ()
writeVHDLM = do
   -- create and change to systemName/vhdl/work
   rootDir <- gets (sid.globalSysDef.global)
   let workDir = rootDir </> "vhdl" </> "work"
   liftIO $ createDirectoryIfMissing True workDir
   liftIO $ setCurrentDirectory workDir
   -- write the local results for the first-level entity
   writeLocalVHDLM
   -- if we are in recursive mode, also write the local results
   -- for the rest of the subsystems
   rec <- isRecursiveSet
   when rec $ do subs <- gets (subSys.globalSysDef.global)
                 let writeSub s = 
                        withLocalST (initLocalST ((readURef.unPrimSysDef) s))
                                    writeLocalVHDLM 
                 mapM_ writeSub subs
   -- create and change to systemName/vhdl/systemName_lib
   -- (remember we are in workDir)
   let libDir = ".." </> rootDir ++ "_lib"
   liftIO $ createDirectoryIfMissing True libDir
   liftIO $ setCurrentDirectory $ libDir
   -- write the global results
   writeGlobalVHDLM
   -- change to systemName/vhdl
   liftIO $ setCurrentDirectory ".."
   -- call quartus if necessary
   callQuartus
   -- analyze with modelsim if necessary
   compile <- isCompileModelsimSet
   when compile compileResultsModelsim
   -- go back to the original directory
   liftIO $ setCurrentDirectory (".." </> "..")
   
-- | Write the global traversing results (i.e. the library design file)
--   accumulated  in the state of the monad
writeGlobalVHDLM :: VHDLM ()
writeGlobalVHDLM = do
 gSysId <- gets (sid.globalSysDef.global)
 debugMsg $ "Generating global system library for `" ++ gSysId ++  "' ...\n"
 globalRes <- gets (globalRes.global)
 -- We can create the id unsafely because sysId was already checked in
 -- transSysDef2Ent
 let libName = gSysId ++ "_lib"
     libDesignFile = genLibDesignFile globalRes
 liftIO $ writeDesignFile libDesignFile (libName ++ ".vhd")


-- | Traverse the netlist and write the local results (i.e. system design files)
writeLocalVHDLM :: VHDLM ()
writeLocalVHDLM = do
  gSysDefVal <- gets (globalSysDef.global)
  lSysDefVal <- gets (currSysDef.local)
  let lSysDefId =  sid lSysDefVal
  debugMsg $ "Compiling system definition `" ++ lSysDefId ++ "' ...\n"
  -- Obtain the netlist of the system definition 
  let nl = netlist lSysDefVal
  -- Traverse the netlist, and get the traversing results
  intOutsInfo <- traverseVHDLM nl 
  LocalTravResult decs stms <- gets (localRes.local)
  let finalLogic = logic lSysDefVal
  -- Obtain the entity declaration of the system and the VHDL identifiers
  -- of the output signals.
  entity@(EntityDec _ eIface) <- transSysDef2Ent finalLogic lSysDefVal 
  -- For each output signal, we need an assigment between its intermediate
  -- signal and the final output signal declared in the entity interface.
  let outIds = mapFilter (\(IfaceSigDec id _ _) -> id) 
                         (\(IfaceSigDec _  m _) -> m == Out) eIface
      outAssigns = genOutAssigns outIds intOutsInfo
      finalRes = LocalTravResult decs (stms ++ outAssigns)
  -- Finally, generate the design file
      sysDesignFile = genSysDesignFile (sid gSysDefVal) entity finalRes
  -- and write it to disk
  liftIO $ writeDesignFile sysDesignFile (lSysDefId ++ ".vhd") 
 where mapFilter f p = foldr (\x ys -> if p x then (f x):ys else ys) []

-- | Traverse the netlist of a System Definition, 
--   returning the (implicit) final traversing state and a list
--   containing the 'IntSignalInfo' of each output of the system
traverseVHDLM :: Netlist [] -> VHDLM [IntSignalInfo]
traverseVHDLM = traverseSEIO newVHDL defineVHDL

-- | \'new\' traversing function for the VHDL backend
newVHDL :: NlNode NlSignal -> VHDLM [(NlNodeOut, IntSignalInfo)]
newVHDL node = case node of
  -- FIXME: Skip the case, basing the generation of tags on
  --        outTags 
  InPort id -> do vId <- transPortId2VHDL id
                  return [(InPortOut, vId)]
  Proc pid proc -> withProcC pid $ do
   -- Obtain the VHDL id of the process
   vpid <- transProcId2VHDL pid
   -- function to create an intermediate signal out of the process
   -- identifier and a string suffix
   let procSuffSignal sigSuffix = unsafeIdAppend vpid sigSuffix
   -- Multiple output tags, add a numeric suffix specifying the output
       multOutTags =  
            zipWith (\tag n -> (tag, procSuffSignal $ outSuffix ++ show n))
                    (outTags node) [(1::Int)..]
   case proc of
    Const _ -> return [(ConstOut, procSuffSignal outSuffix)]
    ZipWithNSY _ _ -> return [(ZipWithNSYOut, procSuffSignal outSuffix)]
    ZipWithxSY _ _ -> return [(ZipWithxSYOut, procSuffSignal outSuffix)]
    UnzipNSY _ _ _ -> return multOutTags
    UnzipxSY _ _ _ _ -> return multOutTags 
    DelaySY _ _ -> return [(DelaySYOut, procSuffSignal outSuffix)]
    SysIns  _ _ ->
      -- Note: Here we could use the name of the System outputs instead of
      --       instanceid_out_n but ... that could cause
      --       clashes with the oher signal names (we only check for the
      --       of the uniqueness of all process ids within a system when 
      --       creating it). We could check for those clashes but it would be
      --       ineffective and ilogical.
      return multOutTags
 where outSuffix = "_out"    
       
-- | \'define\' traversing function for the VHDL backend
defineVHDL :: [(NlNodeOut, IntSignalInfo)] 
             -> NlNode IntSignalInfo 
             -> VHDLM ()
defineVHDL outs ins = do 
 case (outs,ins) of
  (_, InPort _) -> return ()
  (outs, Proc pid proc) -> withProcC pid $ do
   -- We can unsafely transform the pid to a VHDL identifier because
   -- it was checked in newVHDL
   let vPid = unsafeVHDLExtId pid
   case (outs, proc) of
    ([(ConstOut, intSig)],  Const ProcVal{valAST=ast}) -> do
     -- Generate a Signal declaration for the constant
     let cons = expVal ast
     dec  <- withProcValC cons $ transVHDLName2SigDec 
                                   intSig (expTyp ast) (Just cons)
     addSigDec dec
    ([(ZipWithNSYOut, intOut)],  ZipWithNSY f intIns) -> do 
     -- Translate the zipWithN process to a block
     -- and get the declaration of its output signal
     (block, dec) <- transZipWithN2Block vPid intIns (tpfloc f) (tast f) intOut
     addStm $ CSBSm block
     -- Generate a signal declaration for the resulting signal
     addSigDec dec
    ([(ZipWithxSYOut, intOut)], ZipWithxSY f intIns) -> do
     -- Translate the zipWithx process to a block
     -- and get the declaration of its output signal
     (block, dec) <- transZipWithx2Block vPid intIns (tpfloc f) (tast f) intOut
     addStm $ CSBSm block
     -- Generate a signal declaration for the resulting signal
     addSigDec dec      
    (intOuts, UnzipNSY outTypes _ intIn) -> do
     -- Translate the zipWithNSY process to a block
     -- and get the declaration of its output signal
     (block, decs) <- transUnzipNSY2Block vPid intIn (map snd intOuts) outTypes 
     addStm $ CSBSm block
     -- Generate a signal declaration for the resulting signals
     mapM_ addSigDec decs
    (intOuts, UnzipxSY typ size _ intIn) -> do
     -- Translate the UnzipxSY process to a block
     -- and get the declaration of its output signal
     (block, decs) <- transUnzipxSY2Block vPid intIn (map snd intOuts) typ size 
     addStm $ CSBSm block
     -- Generate a signal declaration for the resulting signals
     mapM_ addSigDec decs
    ([(DelaySYOut, intOut)],  DelaySY initExp intIn) -> do
     -- Translate the delay process to a block
     -- and get the declaration of its output signal
     (block, dec) <- transDelay2Block vPid intIn (valAST initExp) intOut
     addStm $ CSBSm block
     -- Generate a signal declaration for the resulting delayed signal
     addSigDec dec 
  
    (intOuts, SysIns pSys intIns) -> do
      let parentSysRef = unPrimSysDef pSys
          parentSysVal = readURef parentSysRef
          parentLogic = logic parentSysVal
          parentInIface = iIface parentSysVal
          parentOutIface = oIface parentSysVal
          typedOuts = zipWith (\(_, t) (_, int) -> (int,t)) parentOutIface
                                                            intOuts 
          parentId = sid parentSysVal 
      -- Translate the instance to a component instantiation 
      -- and get the declaration of the output signals
      (mCompIns, decs) <- transSysIns2CompIns parentLogic 
                                             vPid 
                                             intIns 
                                             typedOuts 
                                             parentId 
                                             (map fst parentInIface) 
                                             (map fst parentOutIface)
      when (isJust mCompIns) (addStm $ CSISm $ fromJust mCompIns)
      -- Generate a signal declaration for each of the resulting signals
      mapM_ addSigDec decs

-- Othewise there is a problem of inconsisten tags
    _ -> intError "ForSyDe.Backend.VHDL.Traverse.defineVHDL" InconsOutTag





