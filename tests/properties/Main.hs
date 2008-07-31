-- Property-testing wrapper module
module Main (main) where
import Test.HUnit
import System.Exit
import VHDLBackend (vhdlBackendTest)


main :: IO ()
main = do
    putStrLn "Running ForSyDe's unit test suite"
    runTestCount $ test ["VHDL Backend Test" ~: vhdlBackendTest]
  where runTestCount t = do c <- runTestTT t 
                            if errors c /= 0 || failures c /= 0 
                               then exitFailure 
                               else exitWith ExitSuccess