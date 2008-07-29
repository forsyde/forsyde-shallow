#! /usr/bin/env runhaskell
module Main (main) where

import Control.Monad (liftM, when)
import Data.List (intersperse)
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Process
import System.Exit
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{postInst=forsydePostInst,
                                            postCopy=forsydePostCopy}

forsydePostInst :: Args -> InstallFlags -> PackageDescription -> 
                   LocalBuildInfo -> IO () 
forsydePostInst _ _  = compile_forsyde_vhd NoCopyDest

forsydePostCopy :: Args -> CopyFlags -> PackageDescription -> 
                   LocalBuildInfo -> IO ()
forsydePostCopy _ cf  = compile_forsyde_vhd (copyDest cf)


-- NOTE: Most of this code is duplicated from ForSyDe.Backend.VHDL.Modelsim,
--       however, it allows Setup.hs to be selfcontained

-- Compile forsyde.vhd if possible, showing what's going on to the end user
compile_forsyde_vhd :: CopyDest -> PackageDescription -> LocalBuildInfo 
                    -> IO ()
compile_forsyde_vhd cd pd lbi = do
    putStrLn "Compiling ForSyDe's VHDL library with Modelsim ..." 
    (ifNot isModelsimInstalled  
           (modelsimError "Modelsim executables could not be found.")) <&&>
     (ifNot (do_compile_forsyde_vhd forsyde_vhd_dir)
            ( modelsimError "Compilation failed.")) <&&>
     (putStrLn "Compilation succeded." >> return True)                        
    return ()
 where 
   forsyde_vhd_dir = (datadir $ absoluteInstallDirs pd lbi cd) </> 
                     "lib"
   modelsimError err = putStrLn $ 
     "Error: " ++ err ++ "\n" ++
     "       ForSyDe will work, but you will not be able to automatically\n" ++
     "       simulate the ForSyDe-generated VHDL models with Modelsim\n" ++
     "       (see function ForSyDe.Backend.VHDL.writeAndSimulateVHDL).\n\n" ++
     "       In order to fix this, install Modelsim and reinstall ForSyDe"

-- Look for modelsim executables
isModelsimInstalled :: IO Bool
isModelsimInstalled =  executablePresent "vlib" <&&> 
                       executablePresent "vmap" <&&> 
                       executablePresent "vcom"
 where executablePresent = (liftM (maybe False (\_-> True))) .findExecutable
 
-- Create a modelsim library for forsyde.vhd 
-- in the same directory in which forsyde.vhd was copied
do_compile_forsyde_vhd :: FilePath -- ^ absolute directory  which 
                                   --   forsyde.vhd was copied into 
                      -> IO Bool
do_compile_forsyde_vhd dir = 
 (runModelsimCommand "vlib" [dir </> "modelsim"])            <&&> 
 (runModelsimCommand
         "vcom" ["-93", "-quiet", "-nologo", "-work", dir </> "modelsim", 
                 dir </> "forsyde.vhd"])
 where runWait :: String -> FilePath -> [String] -> IO Bool
       runWait msg proc args = do
           putStrLn msg 
           h <- runProcess proc args (Just dir) Nothing Nothing Nothing Nothing
           code <- waitForProcess h
           return $ code == ExitSuccess 

-- | run a ModelSim command
runModelsimCommand :: String -- ^ Command to execute 
                   -> [String] -- ^ Command arguments
                   -> IO Bool
runModelsimCommand command args = runWait msg command args
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


-- | short-circuit and for monads
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
x <&&> y = do p <- x
              if p then y else return False

-- | execute an action when the argument is False
--   and return the boolean value
ifNot :: Monad m => m Bool -> m () -> m Bool
ifNot x a = do p <- x
               when (not p) a
               return p