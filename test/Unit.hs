import ForSyDe.Shallow
import ForSyDe.Shallow.Utility.FIR
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

-- | Testing feedback loops in in SDF library
test_toySDF :: Signal Double -> Signal Double
test_toySDF s_in = s_out where
    s_1   = p_add s_in s_2
    s_2   = p_delay s_1
    s_out = p_average s_2   
  -- Process specification
    p_add s1 s2 = actor21SDF (1,1) 1 add s1 s2
    p_delay s   = delaySDF [0] s
    p_average s = actor11SDF 3 1 average s
  -- Function definition
    add [x] [y] = [x + y]
    average [x1,x2,x3] = [(x1 + x2 + x3) / 3.0]
s_test = signal [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]

-- test1: CSDF graph from the paper Cyclo-Static Dataflow
test_toyCSDF :: Num a => Signal a
test_toyCSDF = s3
  where s3 = delayCSDF [1,1] s2
        s2 = v2 s1
        s1 = v1 s4
        s4 = v3 s3
        v1 = actor11CSDF [(1, 1, \[a] -> [a]), (1, 0, \_ -> []), (1, 0, \_ -> [])]
        v2 = actor11CSDF [(1, 0, \_ -> []), (1, 2, \[a] -> [a, 2*a])]
        v3 = actor11CSDF [(1, 3, \[a] -> [a, 2*a, 3*a])]

test_toy1SADF :: Num a => Signal a
test_toy1SADF = s_out where
  s_out   = k_2 c2 s_1
  s_1     = k_1 c1 s_in
  (c1,c2) = d s_in
  s_in = takeS 10 $ infiniteS (+1) 0
  k_1 = kernel11SADF
  k_2 = kernel11SADF
  d = detector12SADF consume_rate next_state select_scenario initial_state
    where
      consume_rate = 1
      -- Next State Function 'next_state' ignores input value
      next_state 0 _ = 1
      next_state 1 _ = 0
      -- Definition of scenarios  
      k_1_scenario_0 = (1,2, \[x] -> [x, x])   -- k1-Scenario 0: output two input tokens
      k_2_scenario_0 = (1,1, \[x] -> [x])      -- k2-Scenario 0: id-function
      k_1_scenario_1 = (1,1, \[x] -> [100*x])  -- k1-Scenario 1: multiply by 100
      k_2_scenario_1 = (2,1, \[x,y] -> [x+y])  -- k2-Scenario 1: add numbers

      -- Function for Selection of scenarios
      --    select_scenario 0 = ((1,1), ([k_1_scenario_0],[k_2_scenario_0]))
      --    select_scenario 1 = ((1,1), ([k_1_scenario_1],[k_2_scenario_1]))
      select_scenario 0 = ((1,1), ([k_1_scenario_0],[k_2_scenario_0]))
      select_scenario 1 = ((1,1), ([k_1_scenario_1],[k_2_scenario_1]))

      -- Initial State
      initial_state = 0

test_toy2SADF :: Num a => Signal a
test_toy2SADF = s_out where
  s_out      = k_2 c_2 s_1 s_2
  (s_1, s_2) = k_1 c_1 s_in
  (c_1, c_2) = d s_in
  s_in = takeS 10 $ infiniteS (+1) 0
  k_1 = kernel12SADF
  k_2 = kernel21SADF
  d = detector12SADF consume_rate next_state select_scenario initial_state
    where
      consume_rate = 1
      -- Next State Function 'next_state' ignores input value
      next_state 0 _ = 1
      next_state 1 _ = 0
      -- Definition of scenarios  
      k_1_scenario_0 = (1,(2,0), \[x]    -> ([x,x],[])) -- k1-Scenario 0: output two input tokens
      k_2_scenario_0 = ((1,0),1, \[x] [] -> [x])        -- k2-Scenario 0: id-function
      k_1_scenario_1 = (1,(0,1), \[x] -> ([], [100*x])) -- k1-Scenario 1: multiply by 100
      k_2_scenario_1 = ((0,2),1, \[] [x,y] -> [x+y])    -- k2-Scenario 1: add numbers

      -- Function for Selection of scenarios
      --    select_scenario 0 = ((1,1), ([k_1_scenario_0],[k_2_scenario_0]))
      --    select_scenario 1 = ((1,1), ([k_1_scenario_1],[k_2_scenario_1]))
      select_scenario 0 = ((1,1), ([k_1_scenario_0],[k_2_scenario_0]))
      select_scenario 1 = ((1,1), ([k_1_scenario_1],[k_2_scenario_1]))

      -- Initial State
      initial_state = 0
      
test_antiWindUpSADF :: (Num a, Ord a) => Signal a -> Signal a
test_antiWindUpSADF input = output
  where
    output = integrator c1 s1 s3
    s3 = delaySADF [0] output
    s1 = kernel11SADF c2 input
    (c1, c2) = detector s3 input
    integrator = kernel21SADF
    -- Detector
    detector :: (Num a, Ord a) => Signal a -> Signal a
             -> (Signal ((Int, Int), Int, [a] -> [a] -> [a]),
                 Signal (Int, Int, [a] -> [a]))
    detector = detector22SADF (1,1) f g 1
    -- State transition function for the detector
    f :: (Num a, Ord a) => Int -> [a] -> [a] -> Int
    f 1 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0)
                  then 2
                  else 1
    f 2 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0)
                  then 2
                  else 1
    -- Output function for the detector
    g :: Num a => Int -> ((Int, Int), ([((Int, Int), Int, [a] -> [a] -> [a])], [(Int, Int, [a] -> [a])]))
    g 1 = ((1,1),
           ([((1,1), 1, \[a] [b] -> [a+b])],
            [(1, 1, \[a] -> [a])]))
    g 2 = ((1,1),
           ([((0,1), 1, \_ [b] -> [b])],
            [(1, 0, \[a] -> [])]))
s2_test = signal $ [10..110]

-- Test of FIR-filter
test_firSY :: Signal Double
test_firSY = firSY (vector [1.0, 0.75, 0.5, 0.25]) $ signal [1.0, 0.0, 0.0, 0.0, 0.0]

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
      it "SDF Feedback Loop" $ test_toySDF s_test
        `shouldBe`(read "{1.3333333333333333,10.333333333333334}" :: Signal Double)
      it "CSDF Feedback Loop" $ takeS 10 test_toyCSDF
        `shouldBe`(read "{1,1,1,2,2,4,4,8,8,16}" :: Signal Integer)
      it "SADF Toy System" $  takeS 10 test_toy1SADF
        `shouldBe`(read "{1,1,203,3,405,5,607,7,809,9}" :: Signal Integer)
      it "SADF Toy System" $  takeS 10 test_toy2SADF
        `shouldBe`(read "{200,1,1000,1}" :: Signal Integer)
      it "SADF Anti Wind-up System" $ takeS 10 (test_antiWindUpSADF s2_test)
        `shouldBe`(read "{10,21,33,46,60,75,91,108,108,108}" :: Signal Integer)
      it "SY FIR Filter" $ test_firSY
        `shouldBe`(read "{1.0,0.75,0.5,0.25,0.0}" :: Signal Double)
