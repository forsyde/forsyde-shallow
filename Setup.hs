#! /usr/bin/env runhaskell
module Main (main) where

import Control.Monad (liftM, when)
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Process
import System.Exit
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{postInst=forsydePostInst}

forsydePostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo
                -> IO ()
forsydePostInst _ _ pd lbi = do
    putStrLn "Compiling ForSyDe's VHDL library with Modelsim ..." 
    (ifNot isModelsimInstalled  
           (modelsimError "Modelsim executables could not be found.")) <&&>
     (ifNot (compile_forsyde_vhd forsyde_vhd_dir)
            ( modelsimError "Compilation failed.")) <&&>
     (putStrLn "Compilation succeded." >> return True)                        
    return ()
 where 
   forsyde_vhd_dir = (datadir $ absoluteInstallDirs pd lbi NoCopyDest) </> 
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
compile_forsyde_vhd :: FilePath -- ^ absolute directory to which forsyde.vhd 
                                --   was copied 
                    -> IO Bool
compile_forsyde_vhd dir = 
 (runWait "Executing: vlib forsyde" "vlib" ["forsyde"])            <&&> 
 (runWait "Executing: vmap forsyde forsyde" "vmap" ["forsyde","forsyde"])  <&&>
 (runWait "Executing: vcom -93 -work forsyde forsyde.vhd"
          "vcom" ["-93", "-work","forsyde", "forsyde.vhd"])
 where runWait :: String -> FilePath -> [String] -> IO Bool
       runWait msg proc args = do
           putStrLn msg 
           h <- runProcess proc args (Just dir) Nothing Nothing Nothing Nothing
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