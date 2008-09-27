-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Modelsim
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to process the VHDL compilation results with Modelsim.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Modelsim (compileResultsModelsim,
                                      executeTestBenchModelsim) where

import ForSyDe.Backend.VHDL.Traverse.VHDLM
import ForSyDe.Backend.VHDL.TestBench

import ForSyDe.System.SysDef
import ForSyDe.OSharing
import ForSyDe.ForSyDeErr
import ForSyDe.Config (getDataDir)

import Data.List (intersperse)
import System.Directory (setCurrentDirectory)
import Control.Monad (liftM, when)
import Control.Monad.State (gets)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Process (runProcess, waitForProcess)
import System.Exit (ExitCode(..))
import System.IO 
import System.FilePath ((</>))
import qualified Language.Haskell.TH as TH (Exp)

-- | Generate a testbench and execute it with Modelsim
--   (Note: the initial and final CWD will be / )
executeTestBenchModelsim :: Maybe Int -- ^ Number of cycles to simulate          
                         -> [[TH.Exp]] -- ^ input stimuli, each signal value
                                       --   is expressed as a template haskell
                                       --   expression 
                         -> VHDLM [[String]] -- ^ results, each signal value
                                             --   is expressed as a string
executeTestBenchModelsim mCycles stimuli= do
  cycles <- writeVHDLTestBench mCycles stimuli
  sysid <- gets (sid.globalSysDef.global)
  -- change to sysid/vhdl/
  liftIO $ setCurrentDirectory (sysid </> "vhdl")
  -- compile the testbench with modelsim
  run_vcom ["-93", "-quiet", "-nologo", "-work", "work", 
            "test" </> (sysid ++ "_tb.vhd")]           
  -- execute the testbench and capture the results
  tmpdir <- liftIO getTemporaryDirectory 
  (file, handle) <- liftIO $ openTempFile tmpdir "tb_out.txt"
  -- we close the temporal file to avoid opening problems with vsim on windows
  liftIO $ hClose handle 
  run_vsim ["-c", "-std_output", file, "-quiet", 
            "-do", "run " ++ show (cycles*10) ++ " ns; exit",
            "work." ++ sysid ++ "_tb"]
  handle2 <- liftIO $ openFile file ReadMode
  flatOut <- liftIO $ hGetContents handle2
  -- go back to the original directory
  liftIO $ setCurrentDirectory (".." </> "..")
  parseTestBenchOut flatOut

-- | Compile the generated VHDL code with Modelsim
--   (Note: the initial and final CWD will be /systemName/vhdl )
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
 run_vlib [modelsimLib]
 run_vcom ["-93", "-quiet", "-nologo", "-work", modelsimLib, libFile]
 -- map the directory of the library to its logical name
 run_vmap [syslib, modelsimLib]
 
 -- compile the work files
 -- NOTE: Since vcom doesn't figure out
 --       the compilation order (according to compoment dependencies),
 --       we first compile the entities and then the architectures.
 --       Another option would be keeping a dependency tree in SysDef
 --       and execute vcom using a depth search (currently we keep
 --       all the subsystems in a flatenned list) 
 let modelsimWork = "work" </> "modelsim"
 run_vlib [modelsimWork]
 run_vcom ("-93" : "-quiet" : "-nologo" : "-work" : modelsimWork : 
           "-just" : "e" : workFiles)
 run_vcom ("-93" : "-quiet" : "-nologo" : "-work" : modelsimWork : 
           "-just" : "a" : workFiles)
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

-- | Run vsim
run_vsim :: [String] -- ^ arguments
         -> VHDLM ()
run_vsim = runModelsimCommand "vsim"

-- | run a ModelSim command
runModelsimCommand :: String -- ^ Command to execute 
                   -> [String] -- ^ Command arguments
                   -> VHDLM ()
runModelsimCommand command args = do
  success <- liftIO $ runWait msg command args
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
                       executablePresent "vcom" <&&>
                       executablePresent "vsim"
 where executablePresent = (liftM (maybe False (\_-> True))) .findExecutable

-- | short-circuit and for monads
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
x <&&> y = do p <- x
              if p then y else return False
