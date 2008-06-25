module Install (testInstall) where

import Distribution.Simple.SetupWrapper
import System.Process
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
-- Also note current working directory must be the root of the project.
testInstall :: [String]    -- ^ extra arguments to Setup.hs configure 
            -> IO ()
testInstall configFlags = do
  setupCWD (["configure","--user","--prefix=testInstallation"] ++ configFlags)
  setupCWD ["build"]
  setupCWD ["haddock"]
  setupCWD ["copy"]
  setupCWD ["register", "--gen-pkg-config=pkgconfig"]
  writeFile "testInstallation.conf" "[]" 
  code <- waitForProcess =<< 
          runCommand  "ghc-pkg -f testInstallation.conf register pkgconfig"
  case code of
     ExitSuccess -> return ()
     e@(ExitFailure _) -> exitWith e
 where setupCWD command = setupWrapper command Nothing 