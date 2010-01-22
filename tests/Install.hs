module Install (testInstall) where

import Distribution.Simple.SetupWrapper
import System.Process
import System.Directory
import System.FilePath
import System.Exit
import System.IO

-- Test if the ForSyDe project configures, builds, generates
-- the haddock documentation and installs without problems.
-- In order to avoid poisoning the haskell-package databases
-- and the file system, the package is configured with 
-- the local prefix "testInstallation/" and registered
-- in a fresh local package database called "testInstallation.conf"
-- Note that this test will only work with ghc because the creation
-- of a new package database relies on ghc-pkg.
-- Also note that the current working directory must be the root of the project.
testInstall :: IO ()
testInstall = do
  cwd <- getCurrentDirectory
  putStrLn "Configuring ForSyDe ..." 
  setupCWD ["configure","--user","--prefix=" ++ cwd </> "testInstallation",
            "-v0"]
  putStrLn "  done.\n"  
  putStrLn "Building ForSyDe ..." 
  setupCWD ["build","-v0"]
  putStrLn "  done.\n"
  putStrLn "Testing the haddock markup of ForSyDe ... " 
  setupCWD ["haddock", "-v0"]
  putStrLn "  done.\n"
  putStrLn "Copying ForSyDe under testInstallation/ ... "
  setupCWD ["copy", "-v0"]
  putStrLn "  done.\n"
  putStrLn ("Registering ForSyDe in fresh package database " ++
            "(testInstallation.conf) ...")
  -- Cabal doesn't allow specifying an alternative package database:
  -- http://hackage.haskell.org/trac/hackage/ticket/307
  -- Thus, we register the package by hand using ghc-pkg
  setupCWD ["register", "--gen-pkg-config=ForSyDeconfig","-v0"]
  code <- waitForProcess =<< 
          runCommand  "ghc-pkg -f testInstallation.conf register ForSyDeconfig"
  case code of
     ExitSuccess -> return ()
     e@(ExitFailure _) -> exitWith e
  putStrLn "done."
 where setupCWD command = setupWrapper command Nothing 
