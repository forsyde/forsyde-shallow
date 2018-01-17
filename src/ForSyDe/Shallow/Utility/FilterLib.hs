-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Utility.FilterLib
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the filter library for ForSyDe heterogeneous MoCs - CT-MoC, SR-MoC,
-- and Untimed-MoC.
--
-- The filters at CT-MoC are based on the filters implemented at SR-MoC and Untimed-MoC, 
-- which means a signal in CT-MoC is always digitalized by a A\/D converter, processed by 
-- digital filters at SR or Untimed domain, and converted back into a CT domain signal by 
-- a D\/A converter. A CT-filter is composed of one A\/D converter, one digital filter, and 
-- one D\/A converter.
--
-- The implementation of the filters at untimed and synchronous domains follows the
-- way in a paper /An introduction to Haskell with applications to digital signal processing, 
-- David M. Goblirsch, in Proceedings of the 1994 ACM symposium on Applied computing./,
-- where the details of the FIR\/IIR, AR, and ARMA filters are introduced. The ARMA filter
-- is the kernel part of our general linear filter 'zLinearFilter' in Z-domain at SR\/Untimed
-- MoC, and is also the kernel digital filter for the linear filter 'sLinearFilter' in 
-- S-domain at CT-MoC.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Utility.FilterLib (
      -- *FIR filter
      firFilter,
      -- *AR and ARMA filter trim
      arFilterTrim, armaFilterTrim,
      -- *The solver mode
      SolverMode(..),
      -- *The general linear filter in S-domain
      sLinearFilter,
      -- *The general linear filter in Z-domain
      zLinearFilter,
      -- *s2z domain coefficient tranformation
      s2zCoef,
      -- *The Z-domain to ARMA coefficient tranformation
      h2ARMACoef
     )
    where 

import ForSyDe.Shallow.MoC
--import ForSyDe.Shallow.MoC.CT
import ForSyDe.Shallow.Core
import ForSyDe.Shallow.Utility.PolyArith
import Data.List (zipWith5)

-- |The FIR filter. Let '[x_n]' denote the input signal, '[y_n]' denote the ouput
-- signal, and '[h_n]' the impulse response of the filter. Suppose the length of
-- the impulse responses is M samples. The formula for '[y_n]' is 
-- $sum_{k=0}^{M-1} h_k*x_{n-k}$.
firFilter :: (Num a) => [a]  -- ^Coefficients of the FIR filter
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal
firFilter hs xs = mealySY stateF (outF hs) (repeatN (length hs) 0) xs
  where
    stateF xs0 x = fixedList xs0 x
    outF hs xs0 x = iprod hs $ fixedList xs0 x

-- |The autoregressive filter is the simplest IIR filter. It is characterized 
-- by a list of numbers '[a_1,a_2,...,a_M]' called the autoregression 
-- coefficients and a single number 'b' called the gain. 'M' is the order of 
-- the filter. Let '[x_n]' denote the input signal, '[y_n]' denote the ouput
-- signal. The formula for '[y_n]' is  $\sum_{k=1}^M {a_k*y_{n-k}+b*x_n}$. 
-- Although it is an IIR filter, here we only get the same length of ouput 
-- signal as the input signal.
arFilterTrim :: (Num a, Fractional a) => 
           [a]   -- ^Coefficients of the AR filter.
         -> a    -- ^The gain
         -> Signal a -- ^Input signal
         -> Signal a -- ^Output signal
arFilterTrim as b xs = 
    mealySY (stateF as b) (outF as b) (repeatN (length as) 0) xs
  where
    stateF as b xs0 x = fixedList xs0 $ outF as b xs0 x 
    outF as b xs0 x = b*x + (iprod as xs0)

-- |The ARMA filter combines the FIR and AR filters. Let '[x_n]' denote the 
-- input signal, '[y_n]' denote the ouput signal. The formula for '[y_n]' is
--  $\sum_{k=1}^M {a_k*y_{n-k}+b*x_n} + sum_{i=0}^{N-1} b_i*x_{n-i}$. The ARMA
-- filter can be defined as the composition of an FIR filter having the impulse
-- reponse '[b_0,b_1,...,b_N-1]' and an AR filter having the regression 
-- coefficients '[a_1,a_2,...,a_M]' and a gain of '1'. Although it is an IIR 
-- filter, here we only get the same length of ouput signal as the input signal.
armaFilterTrim :: (Num a, Fractional a) => 
             [a]  -- ^Coefficients of the FIR filter
          -> [a]  -- ^Coefficients of the AR filter.
          -> Signal a -- ^Input signal
          -> Signal a -- ^Output signal
armaFilterTrim bs as = arFilterTrim as 1 . firFilter bs


-- |The solver mode.
data SolverMode = S2Z   -- ^Tustin tranfer from s-domain to z-domain
        | RK4   -- ^Runge Kutta 4 with fixed simulation steps
  deriving (Show, Eq)

-- |The general linear filter in S-domain at CT-MoC. As the kernel
-- implementation is in Z-domain, the smapling rate should be specified. 
-- It is used on the S-transformation with the following forms, with 
-- coefficients for the descending powers of 's' and m < n.
--
-- >    b_0*s^m + b_1*s^m-1 + ... + b_m-1*s^1 + b_m*s^0
-- >H(s) = ------------------------------------------------     (Eq 1)
-- >    a_0*s^n + a_1*s^n-1 + ... + a_n-1*s^1 + a_n*s^0
--
-- If we multiply both the numerator and the denominator with s^-n, we get 
-- another equivelent canonical form
--
-- >    b_0*s^m-n + b_1*s^m-n-1 + ... + b_m-1*s^1-n + b_m*s^-n
-- >H(s) = -----------------------------------------------------    (Eq 2)
-- >    a_0*s^0 + a_1*s^-1 + ... + a_n-1*s^1-n + a_n*s^-n
--
-- To instantiate a filter, with sampling interval 'T ', we use
--
-- > filter1 = sLinearFilter T [1] [2,1]
-- 
-- to get a filter  with the transfer function
-- 
-- >      1
-- >H(s) = --------
-- >   2*s + 1
-- 
-- and
--
-- > filter2 = sLinearFilter T [2,1] [1,2,2]
--
-- to get another filter with the transfer function
-- 
-- >       2*s +1
-- >H(s) = ----------------
-- >    s^2 + 2*s + 2
--
-- There are two solver modes, 'S2Z' and 'RK4'.
-- Caused by the precision problem, the time interval in CT uses Rational data
-- type and the digital data types in all the domains are Double.
sLinearFilter :: (Num a, Fractional a, Show a, Eq a) =>
    SolverMode   -- ^Specify the solver mode
    ->  Rational     -- ^Fixed simulation interval
    -> [a]  -- ^Coefficients of the polynomial numerator in s-domain
    -> [a]  -- ^Coefficients of the polynomial denominator in s-domain
    -> Signal (SubsigCT a)-- ^Input CT-signal
    -> Signal (SubsigCT a)-- ^Output CT-signal
sLinearFilter filterMode step bs as inS =  outS 
  where
    -- A2D conversion
    inSDigital = a2dConverter step inS
    -- D2A conversion
    outS =  d2aConverter DAhold step outSDigital
    -- Digital filter
    outSDigital | filterMode == S2Z = armaFilterTrim bs' as' inSDigital
        | otherwise =  rk4FilterDigital step as bs inSDigital
          where (bs',as') = h2ARMACoef $ s2zCoef step bs as

-- |Digital filter using Runge Kutta 4 solver.
rk4FilterDigital :: (Fractional a, Show a, Eq a) => 
        Rational -> [a] -> [a] -> Signal a -> Signal a
rk4FilterDigital step as bs inSDigital = outSDigital
  where
    -- Below are the skeletons of the RK-4 solver, with
    -- input signal 'inSDigital' and output signal 'outSDigital'
    -- Coefficients handling
    as'' = dropWhile (\x -> x==0.0) as
    a0 = head as''
    -- Normalized the coefficients
    as' = reverse $ tail $ map (\x -> -x/a0) as''
    bs' = reverse $ map (\x -> x/a0) bs
    -- Order of the filter
    orderFilter = length as'
    -- The last state function, '0' is for the time 
    fXn = iprod (0:as')
    -- The functions for the observalbe state matrix 'A'
    stateFunctions = ffn' orderFilter ++ [fXn]
    -- Initial states
    initialStates = repeatN orderFilter 0.0
    inputSteps = signal $ repeat step'
    -- The states signal
    statesSignal = rks4InSY 0.0 initialStates stateFunctions 
             inputSteps inSDigital --xs
    -- The ouput digital signal 
    outSDigital = mapSY (iprod bs') statesSignal
    -- The fixed simulation step
    step' = fromRational step

-- The length of the function list is 'n-1' for nth order filter
ffn' :: (Num t, Num t1, Eq t) => t -> [[t1] -> t1]
ffn' n = ffn 0 n

-- Construct the functions for the diagonal '1'
ffn :: (Num t1, Num t, Eq t) => Int -> t -> [[t1] -> t1]
ffn _ 1 = []
ffn m n = ff1 m : ffn (m+1) (n-1) 

ff1 :: Num t => Int -> [t] -> t
ff1 m = iprod ([0,0] ++ (repeatN m 0) ++ [1] ++ (repeat 0) )

-- |RK-4 to solve the 1st-order ODEs, with input signal.
rks4InSY :: (Num a, Fractional a) =>
      a     -- ^The initial time
  -> [a]       -- ^The initial state values
  -> [([a] -> a)] -- ^List of the functions of the ODEs.
  ->  Signal  a  -- ^Input signal of steps
  ->  Signal  a  -- ^Input signal
  ->  Signal [a] -- ^Next state signal
rks4InSY x0 ys0 fFs hs us = scanl3SY stateF ys0 xs hs us
  where
    stateF ysn xn h ut = zipWith (+) (repeatN orderODE' 0.0 ++ [ut*h]) $ --Input value
              rks4 h xn fFs ysn 
    xs = scanldSY (+) x0 hs
    -- Order -1 of the ODEs
    orderODE' = length ys0 - 1

-- |One step RK-4 for the 1st-order ordinary differential equations (ODEs).
rks4 ::  (Num a, Fractional a) =>
     a    -- ^The step
     ->  a    -- ^Initial value of time
     -> [[a] -> a] -- ^List of the funcitons of the ODEs.
     -> [a]   -- ^List of the value at the current state
     -> [a]   -- ^List of the value at the next state
rks4 h x0 fFs ys0 = ys1
  where
    h_2 = h/2.0
    ks1 = map (h*) $ map' (x0:ys0) fFs  --    -- map (h *) $ applyFt x0 fFs ys0
    ks2 = map (h*) $ map' (x0+h_2:zipWith (\y k-> y+k/2.0) ys0 ks1) fFs 
    ks3 = map (h*) $ map' (x0+h_2:zipWith (\y k-> y+k/2.0) ys0 ks2) fFs 
    ks4 = map (h*) $ map' (x0+h:zipWith (\y k-> y+k) ys0 ks3) fFs 
    ys1 = zipWith5 (\y0 k1 k2 k3 k4 -> y0 + k1/6 + k2/3 + k3/3 + k4/6)
        ys0 ks1 ks2 ks3 ks4

-- |The general linear filter in Z-domain.
zLinearFilter :: Fractional a => [a] -> [a] -> Signal a -> Signal a
zLinearFilter bs as = armaFilterTrim bs' as'
  where
    bs' = map ((\x y-> y/x ) (head as)) bs
    as' = map ((\x y-> -y/x ) (head as)) $ tail as

-- |s2z domain coefficient tranformation with a specified sampling rate.
-- The Tustin transformation is used for the transfer, with
--
-- >  2(z - 1)  
-- > s = ----------                 (Eq 3)
-- >  T(z + 1)
--
-- in which, 'T' is the sampling interval.
s2zCoef :: (Num a, Fractional a, Eq a) =>
    Rational      -- ^ Sampling rate in Z-domain
    -> [a]        -- ^ Coefficients of the polynomial numerator in s-domain
    -> [a]        -- ^ Coefficients of the polynomial denominator in s-domain
    -> ([a], [a]) -- ^ Tuple of the numerator and denominator 
                  --   coefficients in Z-domain
s2zCoef sampleT bs as = (reverse bs', reverse as')
  where
    (bs',as') = getCoef hZ    
    bsInv = reverse bs
    asInv = reverse as
    numerator' = foldl (\x y -> addPoly x $ scalePoly (fst y) (snd y)) 
                 (Poly [0]) $ zip bsInv sList
    denominator' = foldl (\x y -> addPoly x $ scalePoly (fst y) (snd y)) 
                   (Poly [0]) $ zip asInv sList
    hZ = divPoly numerator' denominator'
    -- Tustin transform
    s = PolyPair (Poly [-2,2],Poly [fromRational sampleT,fromRational sampleT])
    sList = map (powerPoly s) [0..]

-- |The Z-domain to ARMA coefficient tranformation. It is used on the 
-- Z-transfer function
--
-- >    b_0*z^m-n + b_1*z^m-n-1 + ... + b_m-1*z^1-n + b_m*z^-n
-- >H(z) = -----------------------------------------------------    (Eq 4)
-- >    a_0*z^0 + a_1*z^-1 + ... + a_n-1*z^1-n + a_n*z^-n
--
-- which is normalized as
--
-- >    b_0/a_0*z^m-n + b_1/a_0*z^m-n-1 + ... + b_m/a_0*z^-n
-- >H(z) = -------------------------------------------------------  (Eq 5)
-- >    1 + a_1/a_0*z^-1 + ... + a_n-1/a_0*z^1-n + a_n/a_0*z^-n
--
-- The implementation coudl be
--
-- >y(k) = b_0/a_0*x_k+m-n + b_1/a_0*x_k+m-n-1 + ... + b_m/a_0*x_k-n
-- >                        (Eq 6)
-- >           - a_1/a_0*y_k-1 - ... - a_n/a_0*y_k-n
--
-- Then, we could get the coefficients of the ARMA filter.
h2ARMACoef :: (Num a, Fractional a) =>
       ([a], [a]) -- ^Coefficients in Z-domain
    -> ([a], [a]) -- ^Coefficients of the ARMA filter
h2ARMACoef (bs,as) = (scalePolyCoef a0_1 bs, 
          scalePolyCoef (0-a0_1) $ tail as)
  where
    a0_1 = 1.0/ head as

-- Helper functions

map' :: a -> [a->b] -> [b]
map' = flip $ sequence 


-- |Computes the inner product.
iprod :: Num b => [b] -> [b] -> b
iprod xs ys = sum [x*y | (x, y) <- zip xs ys]

-- |Repeat an element for a given times.
repeatN :: Int -> a -> [a]
repeatN n = take n . repeat

-- |Maintain a fixed length of list like Fifo, except the outputs are ignored.
fixedList :: [a] -> a -> [a]
fixedList xs y = take (length xs) $ y:xs
