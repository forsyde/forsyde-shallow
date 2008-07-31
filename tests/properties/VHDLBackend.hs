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

import Test.HUnit
import ForSyDe

vhdlBackendTest :: Test
vhdlBackendTest = test [counterTest]

 
counterTest :: Test
counterTest = "counterTest" ~: "first 400 outputs" ~: 
                                   out400Sim <~=?>  out400VHDLSim
 where out400Sim = take 400 simCounter
       out400VHDLSim = writeAndModelsimVHDL (Just 400) counterSys

(<~=?>) :: (Eq a, Show a) => a -> IO a -> Test  
expected <~=?> actualM  = TestCase $ do a <- actualM
                                        expected @=? a