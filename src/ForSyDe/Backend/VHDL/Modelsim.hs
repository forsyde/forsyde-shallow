-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Modelsim
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with Modelsim.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Modelsim (compileResultsModelsim) where

import ForSyDe.Backend.VHDL.Traverse.VHDLM

import ForSyDe.System.SysDef
import ForSyDe.OSharing
import ForSyDe.ForSyDeErr
import ForSyDe.Config (getDataDir)

import Data.List (intersperse)
import Control.Monad (liftM, when)
import Control.Monad.State (gets)
import System.Directory (findExecutable, setCurrentDirectory)
import System.Process (runProcess, waitForProcess)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

compileResultsModelsim :: VHDLM ()
compileResultsModelsim = do
 -- Check if modelsim is installed
 installed <- liftIO $ isModelsimInstalled
 when (not installed) (throwFError ModelsimFailed) 
 -- get the name of the vhdl files to compile
 sys <- gets (globalSysDef.global)
 let sysId = sid sys
     syslib = sysId ++ "_lib"
     libFile = syslib </> (syslib ++ ".vhd")
     workFiles = ("work" </> (sysId ++ ".vhd")) :
          map (("work"</>).(++".vhd").sid.readURef.unPrimSysDef)
              (subSys sys)
 -- get the modelsim directory  of ForSyDe's vhdl library
 -- (generated during installation)
 dataPath <- liftIO $ getDataDir

 -- map the directory to the logical name forsyde
 let modelsimForSyDe = dataPath </> "lib" </> "modelsim"
 run_vmap ["forsyde", modelsimForSyDe]
 
 -- compile the library of current model
 let modelsimLib = syslib </> "modelsim"
 liftIO $ setCurrentDirectory syslib
 run_vlib ["modelsim"]
 liftIO $ setCurrentDirectory ".."
 run_vcom [libFile, "-work", modelsimLib]
 -- map the directory of the library to its logical name
 run_vmap [syslib, modelsimLib]
 
 -- compile the work files
 let modelsimWork = "work" </> "modelsim"
 liftIO $ setCurrentDirectory "work"
 run_vlib ["modelsim"]
 liftIO $ setCurrentDirectory ".."
 run_vcom (workFiles ++ ["-work", modelsimWork])
 -- map the directory work library to its logical name
 run_vmap ["work", modelsimWork]
 



-- | Run vlib
run_vlib :: [String] -- ^ arguments
         -> VHDLM ()
run_vlib = runModelsimCommand "vlib"

-- | Run vmap
run_vmap :: [String] -- ^ arguments
         -> VHDLM ()
run_vmap = runModelsimCommand "vmap"


-- | Run vmap
run_vcom :: [String] -- ^ arguments
         -> VHDLM ()
run_vcom = runModelsimCommand "vcom"


-- | run a ModelSim command
runModelsimCommand :: String -- ^ Command to execute 
                   -> [String] -- ^ Command arguments
                   -> VHDLM ()
runModelsimCommand command args = do
  success <- liftIO $ runWait msg "vmap" args
  when (not success) (throwFError ModelsimFailed)
 where msg = "Running: " ++ command ++ " " ++ (concat $ intersperse " " args)


-- | Run a process, previously announcing a message and waiting for it
--   to finnish its execution.
runWait :: String -- ^ message to show
        -> FilePath -- ^ command to execute 
        -> [String] -- ^ command arguments
        -> IO Bool -- ^ Did the execution end succesfully?
runWait msg proc args = do
           putStrLn msg 
           h <- runProcess proc args Nothing Nothing Nothing Nothing Nothing
           code <- waitForProcess h
           return $ code == ExitSuccess 


-- Look for modelsim executables
isModelsimInstalled :: IO Bool
isModelsimInstalled =  executablePresent "vlib" <&&> 
                       executablePresent "vmap" <&&> 
                       executablePresent "vcom"
 where executablePresent = (liftM (maybe False (\_-> True))) .findExecutable

-- | short-circuit and for monads
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
x <&&> y = do p <- x
              if p then y else return False
