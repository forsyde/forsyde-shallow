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

import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import ForSyDe.Process.ProcVal
import ForSyDe.Netlist.Traverse
import ForSyDe.Netlist

import Control.Monad.State

import Prelude hiding (exp)

-- | Internal VHDL-Monad version of 'ForSyDe.Backend.writeVHDL'
writeVHDLM :: VHDLM ()
writeVHDLM = do
  sysDefVal <- gets currSysDef
  -- Obtain the entity declaration of the system and the VHDL identifiers
  -- of the output signals.
  -- It is important to do it first, in order to validate the identifiers of the
  -- input and output signals.
  entity@(EntityDec _ eIface) <- liftEProneSys $  transSysDef2Ent sysDefVal 
  -- Obtain the netlist of the system definition 
  let nl = getSymNetlist sysDefVal
  -- Traverse the netlist, and get the traversing results
  intOutsInfo <- traverseVHDLM nl 
  TravResult decs stms <- gets res
  -- For each output signal, we need an assigment between its intermediate
  -- signal and the final output signal declared in the entity interface.
  let outIds = mapFilter (\(IfaceSigDec id _ _) -> id) 
                         (\(IfaceSigDec _  m _) -> m == Out) eIface
      outAssigns = genOutAssigns intOutsInfo outIds
      finalRes = TravResult decs (stms ++ outAssigns)
  -- Finally, generate the design file
      designFile = genDesignFile entity finalRes
  -- and write it to disk
  liftIO $ writeDesignFile designFile (((++ ".vhd").sid) sysDefVal)
 where mapFilter f p = foldr (\x ys -> if p x then (f x):ys else ys) []

-- | Traverse the netlist of a System Definition, 
--   returning the (implicit) final traversing state and a list
--   containing the 'IntSignalInfo' of each output of the system
traverseVHDLM :: Netlist [] -> VHDLM [IntSignalInfo]
traverseVHDLM = traverseSEIO newVHDL defineVHDL

-- | new traversing function for the VHDL backend
newVHDL :: NlNode NlSignal -> VHDLM [(NlNodeOut, IntSignalInfo)]
newVHDL node = case node of
  -- We can create the id unsafely because it was already checked in
  -- transSysDef2Ent
  InPort id -> return [(InPortOut, IntSignalInfo (unsafeVHDLId id))]
  -- FIXME: make it a reserved identifier
  Const _ -> do 
   n <- gets constNum
   incConstNum
   return [(ConstOut, IntSignalInfo (unsafeVHDLId $ "const" ++ show n))]
  Proc pid proc -> do
   let unsupportedError = throwFErrorProc pid $ UnsupportedProc
   -- Obtain the VHDL id of the process
   vpid <- liftEProneProc pid $  transProcId2VHDL pid
   -- function to create an intermediate signal out of the process
   -- identifier and a string suffix
   let procSuffSignal sigSuffix = IntSignalInfo $ unsafeIdAppend vpid sigSuffix 
   case proc of
    ZipWithNSY _ _ -> return [(ZipWithNSYOut, procSuffSignal outSuffix)]
    ZipWithxSY _ _ _ -> unsupportedError
    UnzipNSY _ _ _ -> unsupportedError
    UnzipxSY _ _ _ -> unsupportedError
    DelaySY _ _ -> return [(DelaySYOut, procSuffSignal outSuffix)]
    SysIns  _ _ -> do
      -- Note: Here we could use the name of the System outputs instead of
      --       instanceid_out_n but ... that could cause
      --       clashes with the oher signal names (we only check for the
      --       of the uniqueness of all process ids within a system when 
      --       creating it). We could check for those clashes but it would be
      --       ineffective and ilogical.
      return $ 
       zipWith (\tag n -> (tag, procSuffSignal $ outSuffix ++ show n))
               (outTags node) [(1::Int)..]
 where outSuffix = "_out"    
       
-- | define traversing function for the VHDL backend
defineVHDL :: [(NlNodeOut, IntSignalInfo)] 
             -> NlNode IntSignalInfo 
             -> VHDLM ()
defineVHDL outs ins = case (outs,ins) of
  (_, InPort _) -> return ()
  ([(ConstOut, IntSignalInfo cid)],  Const pv) -> do
   -- FIXME: give identifier to constants as well
   vTM  <- liftEProneSys $  transSignalTR2TM ((expTyp.valAST) pv)
   vExp <- liftEProneSys $  transExp2VHDL ((exp.valAST) pv)
   addSigDec (SigDec cid vTM (Just vExp))
  (outs, Proc pid proc) -> case (outs, proc) of
    ([(ZipWithNSYOut, IntSignalInfo cid)],  ZipWithNSY _ _) -> 
      return ()
    (_, ZipWithxSY _ _ _) -> return ()
    (_, UnzipNSY _ _ _) -> return ()
    (_, UnzipxSY _ _ _) -> return ()
    ([(DelaySYOut, IntSignalInfo cid)],  DelaySY _ _) -> 
      return ()
    (outs, SysIns  _ _) -> do
      
     
    