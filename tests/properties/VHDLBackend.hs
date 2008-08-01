{-# LANGUAGE TemplateHaskell #-}
-- VHDL-Backend property testing module
-- ALl the tests are based on asserting that the internal simulation of the system
-- equals the simulation of the generated vhdl code with modelsim
module VHDLBackend (vhdlBackendTest) where
-- import the example-modules
import ALU
import Counter
import ParAddFour
import Multiplexer
import Multiplexer_FSVector
import SeqAddFour
import ButtonEncoder        
import ZipTwist
import CarrySelectAdder     
import Null

import Data.List (transpose)
import Data.Param.FSVec (vectorTH, reallyUnsafeVector)
import Test.HUnit
import ForSyDe

vhdlBackendTest :: Test
vhdlBackendTest = test [aluTest, counterTest]

-- systematic test for the ALU
aluTest :: Test
aluTest =  "aluTest" ~: outSim <~=?> outVHDL
 where and = $(vectorTH [H,H])
       or =  $(vectorTH [H,L])
       add = $(vectorTH [L,H])
       shiftl = $(vectorTH [L,L])
       fourBitCombs = map reallyUnsafeVector (combN 4 [L,H])
       [arg1,arg2] = parCombN 2 fourBitCombs
       l = length arg1
       select = replicate l and ++
                replicate l or ++
                replicate l add ++
                replicate l shiftl
       op1 = arg1 ++ arg1 ++ arg1 ++ arg1
       op2 = arg2 ++ arg2 ++ arg2 ++ arg2
       outSim = simALU select op1 op2
       outVHDL = vhdlTest Nothing aluSys select op1 op2

counterTest :: Test
counterTest = "counterTest" ~: "first 400 outputs" ~: 
                                   out400Sim <~=?>  out400VHDLSim
 where out400Sim = take 400 simCounter
       out400VHDLSim = vhdlTest (Just 400) counterSys

vhdlTest :: SysFunToIOSimFun sysF simF => Maybe Int -> SysDef sysF -> simF
vhdlTest = writeAndModelsimVHDLOps testVHDLOps

testVHDLOps :: VHDLOps
testVHDLOps = defaultVHDLOps{analyzeQuartus=True}

(<~=?>) :: (Eq a, Show a) => a -> IO a -> Test  
expected <~=?> actualM  = TestCase $ do a <- actualM
                                        expected @=? a



-------------------
-- Helper functions
-------------------

-- | permutation a list
--   Each element of the input list indicates the posibilities of each
--   positional elements in the resulting lists
permut :: [[a]] -> [[a]]
permut [] = []
permut [xs] = map (\x -> [x]) xs
permut (xs:xss) = let res = permut xss
                  in concatMap (\e -> map (:e) xs) res

-- | Combinations of 2 elements from a set of values 
comb2 :: [a] -> [(a,a)]
comb2 xs = concatMap (\i -> map (\j -> (i,j) ) xs) xs

-- | Parallel combination of N elements
parCombN :: Int -> [a] -> [[a]]
parCombN n xs = transpose $ combN n xs

-- | Combinations os N elements from a set of values
combN :: Int -> [a] -> [[a]]
combN n _ | n <= 0 = []
combN 1 xs = map (\x -> [x]) xs
combN n xs = let res = combN (n-1) xs
             in concatMap (\i -> map (\j -> j:i) xs) res