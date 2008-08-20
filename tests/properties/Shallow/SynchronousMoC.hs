module Shallow.SynchronousMoC (synchronousMoCTest) where

import ForSyDe.Shallow
import Test.HUnit

synchronousMoCTest :: Test
synchronousMoCTest = test [mapSYTest]

mapSYTest :: Test
mapSYTest = "mapSYTest" ~: test $ do putStrLn "Test mapSY"
                                     expectedOutput @=? 
                                        mapSY (+1) (signal [1,2,3])
   where expectedOutput :: Signal Int
         expectedOutput = signal [2,3,4] 