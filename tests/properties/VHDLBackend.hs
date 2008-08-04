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

import Control.Monad (liftM, replicateM)
import Data.List (transpose)
import Data.Param.FSVec (vectorTH, reallyUnsafeVector)
import System.Random
import Test.HUnit
import ForSyDe
import Data.Int

vhdlBackendTest :: Test
vhdlBackendTest = test [aluTest, 
                        counterTest,
                        parAddFourTest,
                        muxTest,
                        muxFSVecTest,
                        seqAddFourTest,
                        buttonEncoderTest,
                        zipTwistTest,
                        nullTest]

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

parAddFourTest :: Test
parAddFourTest = "parAddFourTest" ~: test doTest
 where -- testt the overflows, and 400 different pseudorandom numbers
       lastInt32 :: Num a => a
       lastInt32 = 2147483647
       firstInt32 :: Num a => a
       -- FIXME: this should be -2147483648, but due to bug http://code.google.com/p/forsyde-comp/issues/detail?id=22 it was set to -2147483647
       firstInt32 = -2147483647 
       gen400Int32 :: IO [Int32]
       gen400Int32 = do
          let randomInt32 :: IO Int32
              randomInt32 = do
                 let getInt32Range :: IO Integer
                     getInt32Range = getStdRandom (randomR (firstInt32, lastInt32))
                 liftM fromIntegral getInt32Range
          replicateM 400 randomInt32
       doTest :: IO ()
       doTest = do  arg1 <- gen400Int32  
                    arg2 <- gen400Int32
                    arg3 <- gen400Int32
                    arg4 <- gen400Int32 
                    arg5 <- gen400Int32  
                    let sim = simParAddFour (1:(-1):arg1) 
                                            -- (firstInt32:lastInt32:arg2)
                                            -- (lastInt32:firstInt32:arg3)
                                            arg2
                                            arg3
                                            (-1:1:arg4)
                                            (0:1:arg5)
                        simVHDL = vhdlTest Nothing
                                           parAddFourSys
                                           (1:(-1):arg1) 
                                           -- (firstInt32:lastInt32:arg2)
                                           -- (lastInt32:firstInt32:arg3)
                                           arg2
                                           arg3
                                           (-1:1:arg4)
                                           (0:1:arg5)
                    resVHDL <- simVHDL
                    sim @=? resVHDL                    


muxTest :: Test
muxTest = "muxTest" ~:  Multiplexer.simMux sel input <~=?> 
                        vhdlTest Nothing Multiplexer.muxSysDef sel input
 where sel = [(L,L),(L,H),(H,L),(H,H)]
       input = [(L,L,L,H),(L,L,H,L),(L,H,L,L),(H,L,L,L)]
        


muxFSVecTest :: Test
muxFSVecTest = "muxTest" ~:  Multiplexer_FSVector.simMux sel input <~=?> 
                        vhdlTest Nothing Multiplexer_FSVector.muxSysDef sel input
 where sel = [$(vectorTH [L,L]),$(vectorTH [L,H]),$(vectorTH [H,L]),$(vectorTH [H,H])]
       input = [$(vectorTH [L,L,L,H]), $(vectorTH [L,L,H,L]), $(vectorTH [L,H,L,L]),
                $(vectorTH [H,L,L,L])]

seqAddFourTest :: Test
seqAddFourTest = "seqAddFourTest" ~: simAddFour input <~=?>
                                     vhdlTest Nothing addFourSys input
 where input = [-100,-99..100] 

buttonEncoderTest :: Test
buttonEncoderTest = "buttonEncoderTest" ~: simButtonEncoder input <~=?>
                                           vhdlTest Nothing buttonEncoderSys input
 where input = [(H,L,L,L), (L,H,L,L), (L,L,H,L), (L,L,L,H), (L,L,L,L)]

zipTwistTest :: Test
zipTwistTest = "zipTwistTest" ~: 
  simZipTwist input1 input2 input3 input4 input5 input6 <~=?> 
  vhdlTest Nothing zipTwistSys input1 input2 input3 input4 input5 input6
 where input1 = [-100..0]
       input2 = [-10..90]
       input3 = [100..200]
       input4 = [350..450]
       input5 = [-400..(-300)]
       input6 = [-50..50]

nullTest :: Test
-- we don't test quartus here, because it complains about no logic
nullTest = "nullTest" ~: simNull <~=?>  writeAndModelsimVHDL Nothing nullSysDef 
       


-------------------
-- Helper functions
-------------------


-- run a vhdl testbench with custom backend options
vhdlTest :: SysFunToIOSimFun sysF simF => Maybe Int -> SysDef sysF -> simF
vhdlTest = writeAndModelsimVHDLOps testVHDLOps

-- testing VHDL options
testVHDLOps :: VHDLOps
testVHDLOps = defaultVHDLOps{analyzeQuartus=True}

-- Compare an IO test with a pure expected result
(<~=?>) :: (Eq a, Show a) => a -> IO a -> Test  
expected <~=?> actualM  = TestCase $ do a <- actualM
                                        expected @=? a


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