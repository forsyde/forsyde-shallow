import ForSyDe.Shallow
import Test.Hspec

-- | Taken from <https://github.com/forsyde/forsyde-shallow-examples>
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


-- | Taken from <https://github.com/forsyde/forsyde-shallow-examples>
test_fibonacciRabbitsDeath :: Integer -> Signal Integer
test_fibonacciRabbitsDeath months = fibonacciRabbitsDeath $ signal [1..months]
  where
    fibonacciRabbitsDeath ticks = zipWith4SY fusion n a d ticks
      where n = newborns a
            a = adults n
            d = dead n
            fusion x y z ctrl = x + y - z
    newborns = delaySY 1
    adults   = mooreSY nsf out 0
      where nsf state input = (state + input)
            out state = state
    dead = delaynSY 0 4

-- | Taken from <https://github.com/forsyde/forsyde-shallow-examples>
test_adaptiveAmp :: Integer -> Signal Integer
test_adaptiveAmp n = sout
    where s1   = p1 s3 sin -- Process p1
          sout = p2 s1     -- Process p2
          s2   = p3 sout   -- Process p3
          s3   = p4 s2     -- Process p4
          sin  = signal [10..n]
          p1 = zipUs 1 5
          p2 = mapU 1 mult
            where mult [([control], signal)] = map (* control) signal
          p3 = scanU (\_ -> 5) g 10  
            where g :: (Ord a, Num a) => a -> [a] -> a
                  g state signal 
                    | sum signal > 500  = state - 1
                    | sum signal < 400  = state + 1
                    | otherwise         = state
          p4 = initU [10] 

main :: IO ()
main = hspec $ do describe "ForSyDe.Shallow : " $ lab2tests
  where
    lab2tests = do
      it "SY Multiply Accumulator" $ test_mulacc 10
        `shouldBe`(read "{3,9,18,30,45,63,84,108,135,165}" :: Signal Integer)
      it "SY Fibonacci Reproduction" $ test_fibonacciRabbitsDeath 10
        `shouldBe`(read "{1,1,2,3,4,8,12,20,32,52}" :: Signal Integer)
      it "U Adaptive Amplifier" $ test_adaptiveAmp 20
        `shouldBe`(read "{100,110,120,130,140,135,144,153,162,171}" :: Signal Integer)
