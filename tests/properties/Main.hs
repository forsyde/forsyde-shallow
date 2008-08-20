-- Property-testing wrapper module
module Main (main) where
import Test.HUnit
import System.Exit
import System.IO
import VHDLBackend (vhdlBackendTest)
import Shallow (shallowTest)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Running ForSyDe's unit test suite"
    runTestCount $ test ["VHDL Backend Test" ~: vhdlBackendTest,
                         "Shallow Library Test" ~: shallowTest]
  where runTestCount t = do (c, _) <- myRunTestText t 
                            if errors c /= 0 || failures c /= 0 
                               then exitFailure 
                               else exitWith ExitSuccess
        myRunTestText = runTestText (PutText (\str _ _ -> putStrLn str) ())