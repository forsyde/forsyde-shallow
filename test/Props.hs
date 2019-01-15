import ForSyDe.Shallow
import Test.QuickCheck

type Rate = Positive Int

instance (Arbitrary a) => Arbitrary (Signal a) where
  arbitrary = do
    x <- arbitrary
    return (signal x)

instance Arbitrary a => Arbitrary (AbstExt a) where
  arbitrary = do
    x <- arbitrary
    return (Prst x)    

countEvent _ NullS = 0
countEvent a (x :- xs) | a == x    = 1 + countEvent a xs
                       | otherwise = countEvent a xs   

-- SY Process Properties

prop_delaySY xs = 1 + lengthS xs == lengthS (delaySY 1 xs)
  where types = xs :: Signal Int

prop_mealySY xs = lengthS xs == lengthS (mealySY (+) (-) 1 xs)
  where types = xs :: Signal Int

prop_mapSY xs = lengthS xs == lengthS (mapSY (+1) xs)
  where types = xs :: Signal Int

prop_zipWithSY xs ys = min (lengthS xs) (lengthS ys) == lengthS (zipWithSY (+) xs ys)
  where types = (xs :: Signal Int, ys :: Signal Int)

prop_AbstSY1 xs = inputAbst xs == outputAbst xs
  where types = xs :: Signal (AbstExt Int)
        inputAbst    = countEvent Abst
        outputAbst x = countEvent Abst (mapSY (psi (+1)) x)
     

-- SDF Process Properties

prop_feedbackSDF xs = (lengthS xs) `div` (3 * 2) >= (lengthS out) `div` (2 * 3)
  where types = xs :: Signal Int
        out   = actor21SDF (3,1) 3 (\x y -> (+(head y)) <$> x) xs st
        st    = delaySDF [1,1,1] $ actor21SDF (2,4) 2 (zipWith (+)) st xs

prop_actor11SDF gc gp xs = rateI >= rateO && rateI <= rateO + 1
  where types  = (gc :: Rate, gp :: Rate, xs :: Signal Int)
        (c, p) = (getPositive gc, getPositive gc)
        rateI  = lengthS xs  `div` c
        rateO  = lengthS out `div` p
        out    = actor11SDF c p (take p . repeat . head) xs

prop_actor21SDF gc gp xs ys = rateI1 >= rateO && rateI2 >= rateO && (min rateI1 rateI2) <= rateO + 1
  where types  = (gc :: (Rate,Rate), gp :: Rate, xs :: Signal Int, ys :: Signal Int)
        (c1, c2, p) = (getPositive $ fst gc, getPositive $ snd gc, getPositive gp)
        rateI1 = lengthS xs  `div` c1
        rateI2 = lengthS ys  `div` c2
        rateO  = lengthS out `div` p
        out    = actor21SDF (c1,c2) p (\x -> take p . repeat . head) xs ys

main = do
  let runTest s prop = putStr (s ++ " ") >> quickCheck prop
  runTest "SY delay num events" prop_delaySY
  runTest "SY mealy num events" prop_mealySY
  runTest "SY map num events" prop_mapSY
  runTest "SY map num absents" prop_AbstSY1
  runTest "SY zipWith num events" prop_zipWithSY
  runTest "SDF feedback tokens" prop_zipWithSY
  runTest "SDF actor11 tokens" prop_actor11SDF
  runTest "SDF actor21 tokens" prop_actor21SDF
