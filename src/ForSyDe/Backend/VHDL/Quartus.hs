-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Quartus
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with Altera's Quartus II
-- software.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Quartus (analyzeResultsQuartus) where

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
analyzeResultsQuartus :: VHDLM ()
analyzeResultsQuartus = do
 mPath <- liftIO $ findExecutable "quartus_sh"
 case mPath of
  Nothing -> do liftIO $ hPutStrLn stderr "Error: quartus_sh not found"
                throwFError QuartusFailed
  Just _ -> do contents <- gen_quartus_tcl
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
gen_quartus_tcl :: VHDLM String
gen_quartus_tcl = do
 sysName <- gets (sid.globalSysDef.global)
 dataPath <- liftIO $ getDataDir
 recursive <- isRecursiveSet
 subs <- gets (subSys.globalSysDef.global)
 let libDir = (changeSlashes dataPath) ++  "/lib"
     sysLib = sysName ++ "_lib"
 return $ unlines (
   packages ++
   [projectNew     sysName, 
   topLevelEntity sysName,
   includeVHDLFile ("work/" ++ sysName ++ ".vhd") Nothing,
   includeVHDLFile ('"' : (libDir ++ "/forsyde.vhd") ++ "\"") (Just "forsyde"),
   includeVHDLFile (sysLib ++ "/" ++ sysLib ++ ".vhd") (Just sysLib)] ++
   (if recursive then
     map (\s -> includeVHDLFile 
               ((("work/"++).(++".vhd").sid.readURef.unPrimSysDef) s)
               Nothing) subs
     else []) ++
   ["execute_module -tool map"]
  )
 where packages = ["load_package project", "load_package flow"]
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
 