import ForSyDe.Shallow
-- import System.IO.Silently
import Test.Hspec

test_mulacc :: Integer -> Signal Integer
test_mulacc n = sim_run $ mulacc constant1 siggen1
  where
    sim_run val = zipWithSY (\v _ -> v) val ticks 
    mulacc a b  = result
      where addi1  = comb2SY (*) a b         -- Multiplier
            acci   = comb2SY (+) addi1 addi2 -- Adder
            addi2  = delaySY 0 acci          -- Accumulator
            result = acci                    -- Output of the system
    constant1 = sourceSY id   3 :: Signal Integer
    siggen1   = sourceSY (+1) 1 :: Signal Integer
    ticks     = signal [1..n]   :: Signal Integer


main :: IO ()
main = hspec $ do describe "ForSyDe.Shallow : " $ lab2tests
  where
    lab2tests = do
      it "SY Multiply Accumulator" $
        test_mulacc 10 `shouldBe`(read "{3,9,18,30,45,63,84,108,135,165}" :: Signal Integer)
