-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Quartus
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with Altera's Quartus II
-- software.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Quartus 
 (callQuartus) where

import ForSyDe.ForSyDeErr
import ForSyDe.Config (getDataDir)
import ForSyDe.OSharing (readURef)
import ForSyDe.System.SysDef (subSys,sid,unPrimSysDef)

import ForSyDe.Backend.VHDL.Traverse.VHDLM 

import System.IO
import System.Directory
import System.Process
import System.FilePath
import Control.Monad.State
import System.Exit (ExitCode(..))



-- | Analyze the results with Quartus
--   (Note: the initial and final CWD will be /systemName/vhdl )
callQuartus :: VHDLM ()
callQuartus = do
 mQuartus <- gets (execQuartus.ops.global)
 -- is it necessary to call quartus?
 case mQuartus of
  Nothing -> return ()
  -- Yes, we create the tcl script and call quartus_sh checking if it
  -- exists in the system
  Just ops -> do
    mPath <- liftIO $ findExecutable "quartus_sh"
    case mPath of
     Nothing -> do liftIO $ hPutStrLn stderr "Error: quartus_sh not found"
                   throwFError QuartusFailed
     Just _ -> do contents <- gen_quartus_tcl ops
                  liftIO $ writeFile "quartus.tcl" contents
                  liftIO $ putStrLn "Running quartus_sh -t quartus.tcl"
                  code <- liftIO $ waitForProcess =<< runCommand 
                                              "quartus_sh -t quartus.tcl" 
                  case code of
                      ExitFailure _ -> throwFError QuartusFailed
                      _ -> return ()
  
-- | Generate the content of quartus.tcl
--   Note that, even in windows, the tcl interpreter requires pathnames
--   to use \"/\" instead of \"\\\"
gen_quartus_tcl :: QuartusOps -> VHDLM String
gen_quartus_tcl (QuartusOps act mFMax mFamDev assigs) = do
 sysName <- gets (sid.globalSysDef.global)
 dataPath <- liftIO $ getDataDir
 recursive <- isRecursiveSet
 subs <- gets (subSys.globalSysDef.global)
 let libDir = (changeSlashes dataPath) ++  "/lib"
     sysLib = sysName ++ "_lib"
 return $ unlines (
   packages ++
   [projectNew    sysName] ++ 
   mDefault mFMax fmax ++
   mDefault mFamDev famDev ++
   map mkAssig assigs ++
   [topLevelEntity sysName,
   includeVHDLFile ("work/" ++ sysName ++ ".vhd") Nothing,
   includeVHDLFile ('"' : (libDir ++ "/forsyde.vhd") ++ "\"") (Just "forsyde"),
   includeVHDLFile (sysLib ++ "/" ++ sysLib ++ ".vhd") (Just sysLib)] ++
   (if recursive then
     map (\s -> includeVHDLFile 
               ((("work/"++).(++".vhd").sid.readURef.unPrimSysDef) s)
               Nothing) subs
     else []) ++
   [actionCmd  act]
  )
 where mDefault m f = maybe [] f m
       actionCmd act = case act of
                        AnalysisAndElaboration -> 
                           "execute_flow -analysis_and_elaboration"
                        AnalysisAndSynthesis ->
                           "execute_module -tool map"
                        FullCompilation ->
                           "execute_flow -compile"
       fmax max = ["set_global_assignment -name FMAX_REQUIREMENT \"" ++ 
                    show max ++ " MHz\""]
       famDev (fam, mDev) = 
         ["set_global_assignment -name FAMILY " ++ show fam] ++
         case mDev of
           Nothing -> []
           Just dev -> ["set_global_assignment -name DEVICE " ++ show dev]
       mkAssig (vhdlPin, fpgaPin) = "set_location_assignment " ++ 
                                      fpgaPin  ++ " -to " ++ vhdlPin
       packages = ["load_package project", "load_package flow"]
       includeVHDLFile :: FilePath      -- ^ system name
                       -> Maybe String  -- ^ what library to include the 
                                        -- file in
                       -> String
       includeVHDLFile file mLib = 
            "set_global_assignment -name VHDL_FILE " ++ file ++
            maybe "" (" -library "++)  mLib
       topLevelEntity name = 
         "set_global_assignment -name TOP_LEVEL_ENTITY " ++ name
       projectNew name = "project_new " ++ name ++ " -overwrite"   
       changeSlashes [] = []
       changeSlashes ('\\':xs) = '/' : changeSlashes xs
       changeSlashes (x:xs)    = x : changeSlashes xs         
 
