-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.CT
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the ForSyDe library for continuous time MoC (CT-MoC).
-- It is still experimental.
-- Right now there are only constructors 'combCT', 'scaleCT', 'addCT',
-- 'multCT' and 'absCT'.
--
-- The main idea is to represent continuous time signals as functions
-- @Real --> a@ with @a@ being a numerical type. This allows us to represent a 
-- continuous time signal without loss of information because no sampling or 
-- ADC is required. The sampling occurs only when a signal is evaluated, 
-- for instance when it is plotted. 
-- 
-- Thus, a /signal/ is represented as a sequence of functions and intervals. For
-- instance a signal 
-- 
-- @s = \<(sin,[0,100])\>@ 
--
-- represents a sinus signal in the interval between 0 and 100. The signal 
--
-- @s2 = \<(f1(x)=2x, [0,2]), (f2(x)=3+x,[2,4])\>@
--
-- defines a signal that is defined by function @f1@ in the interval @[0,2]@ 
-- and by function @f2@ in the interval @[2,4]@. 
--
-- A /process/ transforms the incoming functions into outgoing functions. 
-- The approach is described in more detail in the ANDRES deliverable D1.1a.
-- Here we only briefly comment the main functions and constructors.
----------------------------------------------------------------------------
module ForSyDe.Shallow.MoC.CT (
  -- * The signal data type
  SubsigCT(..),
  ctSignal,
  liftCT,
  timeStep,
  -- * Primary process constructors
  mapCT, zipWithCT,
  combCT, comb2CT,
  delayCT, addTime, -- initCT, mooreCT, mealyCT,
  -- * Derived process constructors
  -- | These constructors instantiate very useful processes.
  -- They could be defined in terms of the basic constructors
  -- but are typically defined in a more direct way for 
  -- the sake of efficieny.
  scaleCT, addCT, multCT, absCT,
  -- * Convenient functions and processes
  -- | Several helper functions are available to obtain parts
  -- of a signal, the duration, the start time of a signal, and
  -- to generate a sine wave and constant signals.
  takeCT, dropCT, duration, startTime, sineWave, -- constCT, zeroCT,
  -- * AD and DA converters
  DACMode(..), a2dConverter, d2aConverter,
  -- * Some helper functions
  applyF1, applyF2, applyG1, cutEq, 
  -- * Sampling, printing and plotting
  -- $plotdoc
  plot, plotCT, plotCT' ,showParts, vcdGen
  ) where

import ForSyDe.Shallow.Core
import System.Process
import System.Time
--import System.IO
import System.Directory
import Control.Exception as Except
import Data.Ratio
--import Numeric()

-- The revision number of this file:
revision :: String
revision=filter (\ c -> (not (c=='$'))) "$Revision: 1.7 $, $Date: 2007/07/11 08:38:34 $"

-- |The type of a sub-signal of a continuous signal. It consisits of the 
--  function and the interval on which the function is defined.
-- The continuous time signal is then defined as a sequence of SubsigCT 
-- elements: Signal SubsigCT
data SubsigCT a = SubsigCT ((Rational -> a),     -- The function Time -> Value
                            (Rational,Rational)) -- The interval on which the
                                                 -- function is defined

instance (Num a, Show a) => Show (SubsigCT a) where
  show ss = show (sampleSubsig timeStep ss)

-- | The function 'liftCT' creates a CT-compliant function (using the
-- Rationals as domain) from a normal mathematical function that uses
-- a fractional (Double) as domain
liftCT :: Fractional a => (a -> b) -> Rational -> b
liftCT f = f . fromRational

-- | The function 'ctSignal' creates a CT signal from a list of
-- subsignals that are given by a function, an a time range.
--
-- > *ForSyDe.Shallow.MoC.CT> ctsig1 = ctSignal [(liftCT sin, (0, 3.14)), (\t -> 1, (3.14, 6.28))]
-- > *ForSyDe.Shallow.MoC.CT> :t ctsig1
-- > ctsig1 :: Floating a => Signal (SubsigCT a)-- ctsig1 = ctSignal [(liftCT sin, (0, 3.14)), (\t -> 1, (3.14, 6.28))]
ctSignal :: [(Rational -> a, (Rational, Rational))] -> Signal (SubsigCT a)
ctSignal [] = NullS
ctSignal ((f, (start, end)) : xs) = SubsigCT (f, (start, end)) :- ctSignal xs

--unit :: String -- all time numbers are in terms of this unit.
--unit = "sec" 

-- | This constant gives the default time step for sampling and plotting.
-- Its value is 10ns.
timeStep :: Rational 
--timeStep = 10.0e-9
timeStep = 10.0e-2

mapCT :: (a -> b) -> Signal (SubsigCT a) -> Signal (SubsigCT b)
mapCT _ NullS = NullS
mapCT g (SubsigCT (f, (f_start, f_end)):-fs)
  = (SubsigCT (\x -> g (f x), (f_start, f_end)) :- mapCT g fs)

zipWithCT :: (a -> b -> c) -> Signal (SubsigCT a) -> Signal (SubsigCT b) -> Signal (SubsigCT c)
zipWithCT _ NullS _ = NullS
zipWithCT _ _ NullS = NullS
zipWithCT h (SubsigCT (f, (f_start, f_end)):-fs) (SubsigCT (g, (g_start, g_end)):-gs)
    | f_start /= g_start  = error "Start times not aligned"
    | f_end == g_end      = (SubsigCT (\x -> h (f x) (g x), (f_start, f_end)) :- zipWithCT h fs gs)
    | f_end < g_end       = (SubsigCT (\x -> h (f x) (g x), (f_start, f_end))
                             :- zipWithCT h fs (SubsigCT (g, (f_end, g_end)) :- gs))           
    | f_end > g_end       = (SubsigCT (\x -> h (f x) (g x), (f_start, g_end))
                             :- zipWithCT h (SubsigCT (f, (g_end, f_end)) :- fs) gs)
    | otherwise           = error "zipWithCT: pattern not covered"

combCT :: (a -> b) -> Signal (SubsigCT a) -> Signal (SubsigCT b)
combCT = mapCT

comb2CT :: (a -> b -> c) -> Signal (SubsigCT a)
        -> Signal (SubsigCT b) -> Signal (SubsigCT c)
comb2CT = zipWithCT

delayCT :: Rational -> a -> Signal (SubsigCT a) -> Signal (SubsigCT a)    
delayCT period value fs
  = (SubsigCT (\_ -> value, (0,period))) :- addTime period fs            
 
addTime :: Rational -> Signal (SubsigCT a) -> Signal (SubsigCT a)
addTime _ NullS = NullS
addTime delay (SubsigCT (f, (start, end)) :- fs)
  = (SubsigCT (f, (start+delay, end+delay)) :- addTime delay fs)

{-
----
-- | initCT takes an initial signal, outputs it and then copies its second 
-- input signal, which is delayed by the duration of the initial signal.
-- The delay is realized by 'delayCT' 
initCT :: (Num a, Show a) => 
      Signal (SubsigCT a) -- ^ The initial signal output first.
   -> Signal (SubsigCT a) -- ^ Then this signal is output, but delayed.
   -> Signal (SubsigCT a) -- ^ The concatation of the two inputs.
initCT s0 s1 = s0 +-+ (delayCT (duration s0) s1)
-}
    
{-   
-----------------------------------------------------------------------------
-- |The state-full constructor 'mealyCT' resembles a Mealy machine.
mealyCT :: (Num b, Num c, Show b, Show c) =>
       (a -> Rational)      -- ^The gamma function which defines
               -- the partitioning of the input
               -- signal. 
   -> (a -> (Rational -> b) -> a) -- ^The next state function g
   -> (a -> (Rational -> b) -> (Rational -> c))
               -- ^The output encoding function f 
   -> a          -- ^The initial state
   -> Signal (SubsigCT b)        -- ^The input signal
   -> Signal (SubsigCT c)        -- ^The output signal
mealyCT _     _ _ _ NullS = NullS
mealyCT gamma g f w s
    | (duration (takeCT c s)) < c = NullS
    | otherwise = applyF1 (f w) (takeCT c s) 
    +-+ mealyCT gamma g f w' (dropCT c s)
    where c = gamma w
  w' = applyG1 g w (takeCT c s)

-- |The state-full constructor 'mooreCT' resembles a Moore machine.
mooreCT :: (Num b, Num c, Show b, Show c) =>
       (a -> Rational)      -- ^The gamma function which defines
               -- the partitioning of the input
               -- signal. 
   -> (a -> (Rational -> b) -> a) -- ^The next state function g
   -> (a -> (Rational -> c))
               -- ^The output encoding function f 
   -> a          -- ^The initial state
   -> Signal (SubsigCT b)        -- ^The input signal
   -> Signal (SubsigCT c)        -- ^The output signal
mooreCT _     _ _ _ NullS = NullS
mooreCT gamma g f w s
    | (duration (takeCT c s)) < c = NullS
    | otherwise = (SubsigCT ((f w),(a,b))) 
      :- mooreCT gamma g f w' (dropCT c s)
    where c = gamma w
      a = startTime s
      b = a + c
  w' = applyG1 g w (takeCT c s)

-------------------------------------------------------------------------
-- Some useful process constructors:
-- 
-}
-- |'scaleCT' amplifies an input by a constant factor:
scaleCT :: (Num a, Show a) =>
           a                   -- ^The scaling factor
        -> Signal (SubsigCT a) -- ^The input signal
        -> Signal (SubsigCT a) -- ^The output signal of the process
scaleCT factor = mapCT (* factor)

-- |'addCT' adds two input signals together.
addCT :: (Num a, Show a) =>
         Signal (SubsigCT a) -- ^The first input signal
      -> Signal (SubsigCT a) -- ^The second input signal
      -> Signal (SubsigCT a) -- ^The output signal
addCT = zipWithCT (+)

-- |'multCT' multiplies two input signals together.
multCT :: (Num a, Show a) =>
          Signal (SubsigCT a) -- ^The first input signal
       -> Signal (SubsigCT a) -- ^The second input signal
       -> Signal (SubsigCT a) -- ^The output signal
multCT = zipWithCT (*)

-- |'absCT' takes the absolute value of a signal.
absCT :: (Num a,Ord a, Show a) =>
         Signal (SubsigCT a) -- ^The input signal
      -> Signal (SubsigCT a) -- ^The output signal
absCT = mapCT abs

--scaleCT k = applyF1 f'
--    where f' f x = k * (f x)

{-
-- |'scaleCT' has the same functionality as scaleCT but operates with a
-- given signal partitioning rather than on the 
-- SubsigCT elements.
--scaleCT' :: (Num a) =>
--    Rational -- The sampling period
--     -> a    -- The scaling factor
--     -> Signal (SubsigCT a) -> Signal (SubsigCT a)
--scaleCT' step k = combCT step f
--    where f g = f'
--  where f' x = k * (g x)
-}
--addCT s1 s2 = applyF2 f s1' s2'
--    where (s1',s2') = cutEq s1 s2
--      f g1 g2 = f'
--      where f' x = (g1 x) + (g2 x)
{-  
-- addCT' has the same functionality as addCT but operates with a
-- given signal partitioning rather than on the SubsigCT elements.
-- addCT' :: (Num a) =>
--       Rational      -- The sampling period
--    -> Signal (SubsigCT a) -- The first input signal
--    -> Signal (SubsigCT a) -- The second input signal
--    -> Signal (SubsigCT a) -- The output signal
-- addCT' step = combCT2 step f
--     where f g1 g2 = f'
--   where f' x = (g1 x) + (g2 x)
-}
{-
multCT s1 s2 = applyF2 f s1' s2'
    where (s1',s2') = cutEq s1 s2
      f g1 g2 = f'
      where f' x = (g1 x) * (g2 x)

-- multCT' has the same functionality as multCT but operates with a
-- given signal partitioning rather than on the SubsigCT elements.
-- multCT' :: (Num a) =>
--    Rational      -- The sampling period
--     -> Signal (SubsigCT a) -- The first input signal
--     -> Signal (SubsigCT a) -- The second input signal
--     -> Signal (SubsigCT a) -- The output signal
-- multCT' step = combCT2 step f
--     where f g1 g2 = f'
--       where f' x = (g1 x) * (g2 x)
-}
{-  
absCT = applyF1 f
    where f g = f'
  where f' x | (g x) <= 0 = (-1) * (g x)
         | otherwise  = (g x)
-}
-- | 'sineWave' generates a sinus signal with the given frequency defined
-- over  a given period. The function is defined as @f(x)=sin(2*pi*freq*x)@.
sineWave :: (Floating a, Show a) =>
            Rational            -- ^The frequency
         -> (Rational,Rational) -- ^The interval of the signal
         -> Signal (SubsigCT a) -- ^The generated signal
sineWave freq timeInterval 
  = signal [SubsigCT (sineFunction, timeInterval)]
  where 
    sineFunction :: (Floating a) => Rational -> a
    --sineFunction t = sin (2*pi * freq * t)
    sineFunction t = (sin (2*pi * (fromRational (freq * t))))

{-
-- | constCT generates a constant signal for a given time duration.
constCT :: (Num a, Show a) => 
       Rational -- ^ The time duration of the generated signal.
    -> a    -- ^ The constant value of the signal.
    -> Signal (SubsigCT a) -- ^ The resulting signal.
constCT t c = signal [SubsigCT ((\_->c), (0,t))]

-- | zeroCT generates a constant 0 signal for the given time duration.
zeroCT :: (Num a, Show a) => 
      Rational    -- ^ The time duration
   -> Signal (SubsigCT a) -- ^ The generated signal.
zeroCT t = constCT t 0
-}
    
-----------------------------------------------------------------------------
-- DA and AD converter processes:
--
-- |For the digital-analog conversion we have two different possibilities
-- which is determined by this data type 'DACMode'.
data DACMode = DAlinear -- ^linear interpolation
             | DAhold   -- ^the last digital value is frozen
             deriving (Show, Eq)

{- |'d2aConverter' converts an untimes or synchronous signal into a 
 continuous time signal.
 The process 'd2aConverter' converts a signal of the digital domain
 into a continuous time signal. There are two modes, 'DAlinear',
 which makes a smooth transition between adjacent digital values and
 'DAhold', where the analog value directly follows the digital
 value. This means that in 'DAhold'-mode a staircase function
 remains a staircase function, while in 'DAlinear' the staircase
 function would resemble at least partially a /saw tooth/-curve. 

 The resolution of the converter is given by the parameter
 'timeStep'.

 Note, that the process 'd2aConverter' is an ideal component, i.e. there
 are no losses due to a limited resolution due to a fixed number of bits. 
-}
d2aConverter :: (Fractional a, Show a) =>
                DACMode             -- ^Mode of conversion
             -> Rational            -- ^Duration of input signal
             -> Signal a            -- ^Input signal (untimed MoC)
             -> Signal (SubsigCT a) -- ^Output signal (continuous time MoC)
d2aConverter mode c xs
    | mode == DAlinear = d2aLinear c 0.0 xs
    | otherwise = d2aHolder c 0.0 xs
  where
    d2aHolder :: (Num a, Show a) => 
                 Rational -> Rational -> Signal a -> Signal (SubsigCT a)
    d2aHolder _ _ NullS = NullS
    d2aHolder c holdT (x:-xs) = (SubsigCT (constRationalF x,(holdT,holdT+c)) )
                                :- d2aHolder c (holdT+c) xs

    d2aLinear :: (Fractional a, Show a) =>
                 Rational -> Rational -> Signal a -> Signal (SubsigCT a)
    d2aLinear _ _ NullS = NullS
    d2aLinear _ _ (_:-NullS) = NullS
    d2aLinear c holdT (x:-y:-xs)
      = (SubsigCT (linearRationalF c holdT x y,(holdT,holdT+c)) )
        :- d2aLinear c (holdT+c) (y:-xs)

constRationalF :: (Num a) => a -> Rational -> a
constRationalF = (\x _->x)

linearRationalF :: (Fractional a) =>
                   Rational -> Rational -> a -> a -> Rational -> a
linearRationalF c holdT m n x = (1-alpha)*m + alpha*n
  where alpha :: (Fractional a) => a
        alpha = fromRational ((x-holdT)/c)

{- | The process 'a2dConverter' converts a continuous time signal to
   an untimed or synchronous signal. The first parameter gives the
   sampling period of the converter.

   Note, that the process 'a2dConverter' is an ideal component,
   i.e. there are no losses due to a limited resolution due to a fixed
   number of bits.  
-}
a2dConverter :: (Num a, Show a) =>
                Rational            -- ^Sampling Period
             -> Signal (SubsigCT a) -- ^Input signal (continuous time)
             -> Signal a            -- ^Output signal (untimed)
a2dConverter _ NullS = NullS
a2dConverter c s | (duration (takeCT c s)) < c = NullS
                 | otherwise = f (takeCT c s)
                   +-+ a2dConverter c (dropCT c s)
  where f :: (Num a, Show a) => Signal (SubsigCT a) -> Signal a
        f NullS = NullS
        f (SubsigCT (g,(a,_)) :- _) = signal [g a]

--------------------------------------------------------------------
-- Helpter functions for the CT MoC:
-- | applyF1 applies a function on a sub-signal, which means the function of 
-- the subsignal is transformed to another function:
applyF1 :: (Num a, Num b, Show a, Show b) =>
           ((Rational -> a) -> (Rational -> b)) -- The transformer
        -> Signal (SubsigCT a)                  -- The input signal
        -> Signal (SubsigCT b)                  -- The output signal
applyF1 _ NullS = NullS
applyF1 f (ss :- s) = (applyF' f ss) :- (applyF1 f s)
  where applyF' :: (Num a, Num b, Show a, Show b)
                => ((Rational -> a) -> (Rational -> b)) 
                -> (SubsigCT a) -> (SubsigCT b)
        applyF' f (SubsigCT (f',(a,b))) = SubsigCT ((f f'), (a,b))

-- | applyF2 works just like applyF1 but operates on two incoming signals.
applyF2 :: (Num a, Num b, Num c, Show a, Show b, Show c) =>
           ((Rational -> a) -> (Rational->b) -> (Rational -> c))
        -> Signal (SubsigCT a) 
        -> Signal (SubsigCT b) 
        -> Signal (SubsigCT c) 
applyF2 _ NullS _ = NullS
applyF2 _ _ NullS = NullS
applyF2 f (ss1 :- s1) (ss2 :- s2) = (applyF' f ss1 ss2) :- (applyF2 f s1 s2)
  where applyF' f (SubsigCT (f1,(a,b))) (SubsigCT (f2,(c,d))) 
          | (a==c) && (b==d) 
            || (abs (a-c)< 0)
            || (abs (b-d)< 0) = SubsigCT ((f f1 f2), (a,b))
          | otherwise = error ("applyF2: The two subintervals are"
                                ++ " not identical: (a,b) = ("
                                ++ (show a) ++ ", "
                                ++ (show b) ++ "); (c,d) = ("
                                ++ (show c) ++ ", "
                                ++ (show d) ++ ").")

-- | applyG1 is used to apply a next-state function. A very interesting
-- question is, what should be an argument to the next-state function: 
-- the incoming function, defining the value of the input signal?
-- or the incoming function and the incoming interval?
-- or the value of the incoming signal at a particular point, e.g. at the 
-- left most point of the interval?
-- To give the next-state function the interval itself as argument, would mean
-- that the process becomes time variant process, i.e. its behaviour is 
-- dependent on the absolute time values. This is not a good thing to have!
-- Another possibility may be to give a sub-signal that is relative to the 
-- current evaluation, i.e. the left most point is always 0. Would that make
-- sense?
applyG1 :: (Num b, Show b) =>
           (a -> (Rational -> b) -> a) -> a -> Signal (SubsigCT b) -> a
applyG1 _ w NullS = w
applyG1 g w (ss :- _) = applyG1' g w ss
  where 
    applyG1' :: (Num b, Show b) =>
                (a -> (Rational -> b) -> a) -> a -> (SubsigCT b) -> a
    applyG1' g w (SubsigCT (f, (_,_))) = g w f

-- | cutEq partitions the two signals such that the partitioning are identical
-- in both result signals, but only up to the duration of the shorter of the 
-- two signals:
cutEq :: (Num a, Num b, Show a, Show b) =>
     Signal (SubsigCT a) -> Signal (SubsigCT b) 
  -> (Signal (SubsigCT a), Signal (SubsigCT b))
cutEq NullS s2 = (NullS, s2) 
cutEq s1 NullS = (s1, NullS) 
cutEq s1 s2 = unzipCT (cutEq' s1 s2)
  where 
    cutEq' :: (Num a, Num b, Show a, Show b) =>
              Signal (SubsigCT a) -> Signal  (SubsigCT b) 
           -> Signal ((SubsigCT a), (SubsigCT b))
    cutEq' NullS _    = NullS
    cutEq' _ NullS    = NullS
    cutEq' (ss1:-s1) (ss2:-s2) 
      | dur1 == dur2 = (ss1,ss2) :- (cutEq' s1 s2)
      | dur1 <  dur2 = (ss1, takeSubSig dur1 ss2) 
                       :- (cutEq' s1 ((dropSubSig dur1 ss2) :- s2))
      | dur1 >  dur2 = (takeSubSig dur2 ss1, ss2)
                       :- (cutEq' ((dropSubSig dur2 ss1) :- s1) s2)
      | otherwise = error ("cutEq' pattern match error: dur1="++(show dur1)
                           ++ ", dur2="++ (show dur2)++";")
      where dur1 = durationSS ss1
            dur2 = durationSS ss2

unzipCT :: Num a => Signal ((SubsigCT a), (SubsigCT b)) 
        -> (Signal (SubsigCT a), Signal (SubsigCT b))
unzipCT NullS = (NullS, NullS)
unzipCT ((ss1,ss2) :- s) = (ss1:-s1, ss2:-s2)
    where (s1,s2) = unzipCT s

-- The take and drop functions on CT signals:
takeCT :: (Num a, Show a) => 
          Rational -> Signal (SubsigCT a) -> Signal (SubsigCT a)
takeCT _ NullS = NullS
takeCT 0 _     = NullS
takeCT c (ss:-s) | (durationSS ss) >= c = (takeSubSig c ss) :- NullS
                 | otherwise    = ss :- (takeCT (c - (durationSS ss)) s)

dropCT :: (Num a, Show a) =>
          Rational -> Signal (SubsigCT a) -> Signal (SubsigCT a)
dropCT _ NullS   = NullS
dropCT 0 s   = s
dropCT c (ss:-s) | (durationSS ss > c) = dropSubSig c ss :- s
                 | otherwise       = dropCT (c - (durationSS ss)) s

-- The interval length of a signal:
duration :: (Num a, Show a) => Signal (SubsigCT a) -> Rational
duration NullS = 0
duration (ss:- s) = (durationSS ss) + (duration s)  

-- The interval length of a sub-signal:
durationSS :: (Num a, Show a) => (SubsigCT a) -> Rational
durationSS (SubsigCT (_, (a,b))) = b-a

-- The start time of a signal:
startTime :: (Num a, Show a) => Signal (SubsigCT a) -> Rational
startTime NullS = 0
startTime (SubsigCT (_,(a,_)) :- _) = a

-- The take and drop functions for sub-signals:
takeSubSig :: (Num a, Show a) => Rational -> (SubsigCT a) -> (SubsigCT a)
takeSubSig c (SubsigCT (f,(a,b))) | c >= (b-a) = SubsigCT (f,(a,b))
                                  | otherwise  = SubsigCT (f,(a,a+c))


dropSubSig :: (Num a, Show a) => Rational -> (SubsigCT a) -> (SubsigCT a)
dropSubSig c (SubsigCT (f,(a,b))) | c > (b-a) = SubsigCT (f,(b,b))
                                  | otherwise = SubsigCT (f,(a+c,b))



-----------------------------------------------------------------------
-- Functions to display and plot signals:
-----------------------------------------------------------------------
-- The function 'sample' evaluates the signal and returns a list of 
-- (time,value) pairs, which can be displayed as text or used in any other way.
{- $plotdoc
   Several functions are available to display a signal in textual or 
   graphics form. All displaying of signals is based on sampling and 
   evaluation the signal at regular sampling points. 
   
   'showParts' does not evaluate the signal; it only shows how it is 
   partitioned. Hence, it returns a sequence of intervals.
  
   'plot', 'plotCT' and 'plotCT'' can plot a signal or a list of signals 
   in a graph. They use @gnuplot@ for doing the actual work.
   They are in the IO monad because they write to the file system.
  
   'plot' is defined in terms of 'plotCT' but it uses the default sampling 
   period 'timeStep' and it can plot only one signal in a plot.
  
   'plotCT' can plot a list of signals in the same plot.
   'plotCT' is defined in terms of 'plotCT'' but uses 
   default label names for the plot.

   'vcdGen' writes the values of signals in Value Change Dump (VCD) format to 
   a file. There are public domain wave viewers which understand this format 
   and can display the signals.
-}

-- |'sample' computes the values of a signal with a given step size. 
-- It returns a list with (x, (f x)) pairs of type [(Rational,Rational)].
sample :: (Num a, Show a) =>
          Rational    -- ^ The sampling period
       -> Signal (SubsigCT a) -- ^The signal to be sampled
       -> [(Rational,a)]  -- ^The list of (time,value) pairs of the 
          -- evaluated signal
sample _ NullS = []
sample step (ss :- s) = sampleSubsig step ss ++ (sample step s)

-- sampleSubsig samples a Subsig signal:
sampleSubsig :: (Num a, Show a) => Rational -> (SubsigCT  a) -> [(Rational,a)]
sampleSubsig step (SubsigCT (f,(a,b)))
  | b>a = (a,(f a)) : (sampleSubsig step (SubsigCT (f,(a+step,b))))
  | otherwise = []

-- |'showParts' allows to see how a signal is partitioned into
-- sub-signals.  It returns the sequence of intervals.
showParts :: (Num a, Show a) =>
             Signal (SubsigCT a)   -- ^The partitioned signal
          -> [(Double,Double)] -- ^The sequence of intervals
showParts NullS = []
showParts (SubsigCT (_,(a,b)):-s) = (fromRational a,fromRational b) : (showParts s)

-----------------------------------------------------------------------------
-- |'plot' plots one signal in a graph with the default sampling
-- period of 1\/200 of the duration of the signal.
plot :: (Num a, Show a) =>
        Signal (SubsigCT a) -- ^The signal to be plotted.
     -> IO String           -- ^A reporting message.
plot s = plotCT step [s]
  where step = (duration s) / 200.0

-- |'plotCT' plots a list of signals in the same graph. The sampling
-- period has to be given as argument. In the graph default label
-- names are used to identify the signals.
plotCT :: (Num a, Show a) =>
          Rational              -- ^The sampling period
       -> [Signal (SubsigCT a)] -- ^The list of signals to be ploted 
                                -- in the same graph
       -> IO String             -- ^A messeage reporting what has been done.
plotCT step sigs = plotCT' step (map (\ s -> (s,"")) sigs)

{- |
   'plotCT'' is the work horse for plotting and the functions 'plot' and 
   'plotCT' use it with specialising arguments.

   'plotCT'' plots all the signals in the list in one graph. If a label is
   given for a signal, this label appears in the graph. If the label string is 
   \"\", a default label like \"sig-1\"  is used.

   In addition to displaying the graph on the screen, the following files
   are created in directory .\/fig:

   [ct-moc-graph.eps]      an eps file of the complete graph

   [ct-moc-graph.pdf]      A pdf file of the complete graph
 
   [ct-moc-graph-latex.eps]    included by ct-moc-graph-latex.tex

   [ct-moc-graph-latex.tex]    This is the tex file that should be included
           by your latex document. It in turn includes
        the file ct-moc-graph-latex.eps.
       These two files have to be used together;
        the .eps file contains only the graphics,
       while the .tex file contains the labels and 
           text.
-}
plotCT' :: (Num a, Show a) =>
           Rational -- ^Sampling period
        -> [(Signal (SubsigCT a), String)]
        -- ^A list of (signal,label) pairs. The signals are plotted and
        -- denoted by the corresponding labels in the plot.
        -> IO String -- ^A simple message to report completion
plotCT' _ [] = return []
plotCT' 0 _    = error "plotCT: Cannot compute signal with step=0.\n"
plotCT' step sigs = plotSig (expandSig 1 sigs)
  where 
    expandSig :: (Num a, Show a) => 
                 Int -> [(Signal (SubsigCT a),String)] 
              -> [(Int,String,[(Rational,a)])]
    expandSig _ [] = []
    expandSig i ((sig,label):sigs) 
      = (i, label, (sample step sig)) : (expandSig (i+1) sigs)
    plotSig :: (Num a, Show a) => [(Int,String,[(Rational,a)])] -> IO String
    plotSig sigs 
      = do mkDir "./fig"
           writeDatFiles sigs
           -- We write the gnuplot script to a file;
           -- But we try several times with a different name because 
           -- with ghc on cygwin we cannot write to a script file while
           -- gnuplot is still running with the old script file:
           fname <- tryNTimes 10 
                    (\ file -> (writeFile file
                                (mkPlotScript (map mkDatFileName sigs))))
           -- We fire up gnuplot:
           _ <- system ("gnuplot -persist " ++ fname)
           -- We return some reporting string:
           return ("Signal(s) " ++(mkAllLabels sigs) ++ " plotted.")
    writeDatFiles [] = return ()
    writeDatFiles (s@(_, _, sig): sigs)
      = do writeFile (fst (mkDatFileName s)) (dumpSig sig)
           writeDatFiles sigs
    mkDatFileName :: (Int,String,a) -> (String,String)
    mkDatFileName (sigid,label,_) = ("./fig/ct-moc-" ++ (replChar ">" label) 
                                     ++(show sigid)++".dat", 
                                     (mkLabel label sigid))
    mkLabel :: String -> Int -> String
    mkLabel "" n = "sig-" ++ show n 
    mkLabel l _  = l
    mkAllLabels :: (Num a) => [(Int,String,[(Rational,a)])] -> String
    mkAllLabels sigs = drop 2 (foldl f "" sigs)
      where f labelString (n,label,_) 
              = labelString ++ ", " ++ (mkLabel label n)
    replChar :: String -- all characters given in this set are replaced by '_'
             -> String -- the string where characters are replaced
             -> String -- the result string with all characters replaced
    replChar [] s = s
    replChar _ [] = []
    replChar replSet (c:s) | elem c replSet = '_' : (replChar replSet s)
                           | otherwise  = c   : (replChar replSet s)

    dumpSig :: (Num a, Show a) => [(Rational,a)] -> String
    dumpSig points = concatMap f points
      where f (x,y) = show ((fromRational x) :: Float) ++ "    " 
                      ++ show (y) ++ "\n"

    mkPlotScript :: [(String  -- the file name of the dat file
                     ,String  -- the label for the signal to be drawn
                     )] -> String  -- the gnuplot script
    mkPlotScript ns = "set xlabel \"seconds\" \n"
                      ++ "plot " ++ (f1 ns) ++ "\n"
                      ++ "set terminal postscript eps color\n"
                      ++ "set output \"" ++ plotFileName++".eps\"\n"
                      ++ "replot \n"
                      ++ "set terminal epslatex color\n"
                      ++ "set output \"" ++ plotFileName++"-latex.eps\"\n"
                      ++ "replot\n"
                      -- ++ "set terminal pdf\n"
                      -- ++ "set output \"fig/ct-moc-graph.pdf\"\n"
                      -- ++ "replot\n"
      where f1 :: [(String,String)] -> String
            f1 ((datfilename,label):(n:ns)) 
              = "\t\"" ++ datfilename
                ++ "\" with linespoints title \""++label++"\",\\\n"
                ++ "    " ++ (f1 (n:ns))
            f1 ((datfilename,label):[]) 
              = "\"" ++ datfilename 
                ++ "\" with linespoints title \""++label++"\"\n"
            f1 [] = ""
            plotFileName = "fig/ct-moc-graph-" ++ (f2 ns)
            -- f2 generates part of the filename for the eps and latex
            -- files, which is determined by the signal labels.
            f2 :: [(String,String)] -> String 
            f2 [] = ""
            f2 ((_,label):[]) = label
            f2 ((_,label):_) = label ++ "_"
    -- tryNTimes applies a given actions at most n times. Everytime
    -- the action is applied and an error occurrs, it tries again but 
    -- with a decremented first argument. It also changes the file name
    -- because the file name uses the n as part of the name.
    -- The idea is that the action tries different files to operate on.
    -- The problem was that when gnuplot was called on a gnuplot script
    -- file, it was not possible to write a new script file with the same
    -- name and start a new gnuplot process (at least not with ghc or ghci on 
    -- cygwin; it worked fine with hugs on cygwin). 
    -- So we go around the problem here by trying different file names until
    -- we succeed or until the maximum number of attempts have been performed.
    tryNTimes :: Int -> (String -> IO ()) -> IO String
    tryNTimes n a | n <= 0 = error "tryNTimes: not succedded"
                  | n > 0 = 
                      do Except.catch (action fname a) (handler a)
                           where handler :: (String -> IO()) -> IOError -> IO String
                                 handler a _ = tryNTimes (n-1) a
                                 fname = "./fig/ct-moc-" ++ (show n) ++ ".gnuplot"
                                 action :: String -> (String -> IO ()) -> IO String
                                 action fname a = do (a fname)
                                                     return fname
    tryNTimes _ _ = error "tryNTimes: Unexpected pattern."

----------------------------------------------------------------------------
{- |
vcdGen dumps the values of a list of signal in VCD (Value Change Dump) format 
(IEEE Std 1364-2001), which is part of the Verilog standard 
(<http://en.wikipedia.org/wiki/Value_change_dump>).
There are public domain tools to view VCD files. For instance, 
GTKWave (<http://home.nc.rr.com/gtkwave/>) is a popular viewer available for
Windows and Linux.

The values are written to the file ./fig/ct-moc.vcd. If the file exists, it
is overwritten. If the directory does not exist, it is created.

-}
vcdGen :: (Num a, Show a) 
      => Rational     -- ^Sampling period; defines for what
              -- time stamps the values are written.
      -> [(Signal (SubsigCT a), String)]
      -- ^A list of (signal,label) pairs. The signal values written and
      -- denoted by the corresponding labels.
      -> IO String    -- ^A simple message to report completion
vcdGen _ [] = return []
vcdGen 0    _  = error "vcdgen: Cannot compute signals with step=0.\n"
vcdGen step sigs = 
    do 
  -- putStr (show (distLabels (expandSig 1 sigs)))
  plotSig (expandSig 1 sigs)
    where 
  expandSig :: (Num a, Show a) => 
       Int -> [(Signal (SubsigCT a),String)] 
           -> [(Int,String,[(Rational,a)])]
  expandSig _ [] = []
  expandSig i ((sig,label):sigs) 
      = (i, label, (sample step sig)) : (expandSig (i+1) sigs)
  plotSig :: (Num a, Show a) => [(Int,String,[(Rational,a)])] -> IO String
  plotSig sigs 
      = do writeVCDFile sigs
           -- We return some reporting string:
           return ("Signal(s) " ++(mkAllLabels sigs) ++ " dumped.")
  mkLabel :: String -> Int -> String
  mkLabel "" n = "sig-" ++ show n 
  mkLabel l _  = l
  mkAllLabels sigs = drop 2 (foldl f "" sigs)
      where f labelString (n,label,_) 
              = labelString ++ ", " ++ (mkLabel label n)
  writeVCDFile :: (Show a) => [(Int,String,[(Rational,a)])] -> IO()
  writeVCDFile sigs
    = do mkDir "./fig"
         clocktime <- getClockTime
         let {date = calendarTimeToString (toUTCTime clocktime);
              labels = getLabels sigs;
              timescale = findTimescale sigs;}
           in writeFile mkVCDFileName ((vcdHeader timescale labels date)
                                       ++ (valueDump timescale (prepSigValues sigs)))
  mkVCDFileName :: String
  mkVCDFileName = ("./fig/ct-moc.vcd")

mkDir :: String -> IO()
mkDir dir = do dirExists <- doesDirectoryExist dir
               if (not dirExists) 
                 then (createDirectory dir) 
                 else return ()

-- prepSigValues rearranges the [(label,[(time,value)])] lists such 
-- that we get a list of time time stamps and for each time stamp 
-- we have a list of (label,value) pairs to be dumped:
prepSigValues :: (Show a) => [(Int,String,[(Rational,a)])]
      -> [(Rational,[(String,a)])]
prepSigValues sigs = f2 (distLabels sigs)
    where 
  -- f2 transforms a [[(label,time,value)]] 
  -- into a [(time, [label,value])] structure:
  f2 :: (Show a) 
    => [[(String,Rational,a)]] -> [(Rational,[(String,a)])]
  f2 [] = []
  f2 ([]:_) = []  
  f2 xs = f3 hdxs : f2 tailxs
      where 
    -- here we take all first elements of the lists in xs
    -- and the tail of the lists in xs:
    (hdxs,tailxs) = (map g1 xs,
             map (\ (_:ys)-> ys) xs)
    g1 [] = error ("prepSig.f2.g1: first element of xs is empty:"
           ++ "xs="++show xs)
    g1 (y:_) = y
    f3 :: (Show a) 
      => [(String,Rational,a)] -> (Rational,[(String,a)])
    f3 (valList@((_, time, _):_)) = (time, f4 time valList)
    f3 [] = error ("prepSigValues.f2.f3: "
           ++ "empty (label,time,value)-list")
    f4 :: (Show a) 
      => Rational -> [(String,Rational,a)] -> [(String,a)]
    f4 _ [] = []
    f4 time ((label,time1,value):valList) 
       | time == time1 = (label,value) : f4 time valList
       | otherwise 
       = error ("prepSigValues: Time stamps in different"
            ++ " signals do not match: time="
            ++(show time)++", time1="++(show time1)
            ++", label="++label++", value="++(show value)
            ++"!")
-- distLabels inserts the labels into its corresponding 
-- (time,value) pair list to get a (label,time,value) triple:
distLabels :: [(Int,String,[(Rational,a)])] 
       -> [[(String,Rational,a)]]
distLabels [] = []
distLabels ((_,label,valList):sigs) 
    = (map (\ (t,v) -> (label,t,v)) valList) : (distLabels sigs)
getLabels :: [(Int,String,[(Rational,a)])] -> [String]
getLabels = map (\(_,label,_)-> label)
vcdHeader :: Rational -> [String] -> String -> String
vcdHeader timescale labels date = "$date\n"
           ++ date ++ "\n"
           ++ "$end\n"
           ++ "$version\n"
           ++ "ForSyDe CTLib " ++ revision ++ "\n"
           ++ "$end\n"
           ++ "$timescale 1 " ++ (timeunit timescale) ++ " $end\n"
           ++ "$scope module top $end\n"
           ++ (concatMap (\ label -> ("$var real 64 "++label
                  ++ " " ++ label 
                  ++ " $end\n")) labels)
           ++ "$upscope $end\n"
           ++ "$enddefinitions $end\n"
           ++ "#0\n"
           ++ "$dumpvars\n"
           ++ (concatMap (\ label -> "r0.0 "++label++ "\n") 
             labels)
           ++ "\n"
valueDump :: (Show a) => Rational -> [(Rational,[(String,a)])] -> String
valueDump _ [] = ""
valueDump timescale ((t,values):valList) 
  = "#"++(show (g (t/timescale)))++"\n" 
    ++ (f values) ++ (valueDump timescale valList)
  where 
    f :: (Show a) => [(String,a)] -> String
    f [] = ""
    f ((l,v):values) = "r"++(show v)++" "++l++"\n" ++ (f values)
    g :: Rational -> Integer
    -- Since the VCD format expects integers for the timestamp, we make
    -- sure that only an integer is printed in decimal format (no exponent):
    g t = round t


timeunit :: Rational -> String
timeunit timescale | timescale == 1 % 1    = "s"
       | timescale == 1 % 1000 = "ms"
       | timescale == 1 % 1000000 = "us"
       | timescale == 1 % 1000000000 = "ns"
       | timescale == 1 % 1000000000000 = "ps"
       | timescale == 1 % 1000000000000000 = "fs"
       | otherwise = error ("timeunit: unexpected timescale: "
                ++ (show timescale))

findTimescale :: [(Int,String,[(Rational,a)])] -> Rational
findTimescale sigs 
    = f 1 (concatMap (\ (_,_,valList) -> (fst (unzip valList))) sigs)
  where 
    f :: Rational -> [Rational] -> Rational
    f scale [] = scale
    f scale (x:xs) | r == 0    = f scale xs
           | otherwise = f (scale/1000) xs
           where (_,r) = (properFraction (abs (x / scale))) 
                         :: (Int,Rational)

-------------------------------------------------------------------------
-----------------------------------------------------------
-- Testing the CT signals and process constructors:

{--
main = testAll
testAll = 
    do 
  testScaleCT 
  testAddCT 
  testMultCT 
  testAbsCT 
  testDelayCT
  testConverters
  testFeedback
-- test scaleCT:
testScaleCT = plotCT' 1E-4 [(toneA, "Tone A"), 
            ((scaleCT 1.5 toneA), "Tone A x 1.5"),
            ((scaleCT 2.0 toneA), "Tone A x 2.0")]

-- test addCT:
testAddCT = plotCT' 1e-4 [(toneA, "Tone A"),
          (toneE, "Tone E"), 
          ((addCT toneA toneE), "Tones A+E")]

-- test multCT:
testMultCT = plotCT' 1e-4 [(toneA, "Tone A"),
          (toneE, "Tone E"), 
          ((multCT toneA toneE), "Tones A x E")]

-- test absCT:
testAbsCT = plotCT' 1E-4 [(toneA, "Tone A"), 
          ((absCT toneA), "abs (Tone A)")]

-- test delayCT:
testDelayCT = plotCT' 1E-4 
      [(toneA, "Tone A"), 
       (takeCT 0.02 ((delayCT 0.0025 toneA)), 
           "Tone A delayed by 2.5ms"),
       (takeCT 0.02 ((shiftCT 0.003 toneA)), "Tone A shifted by 3ms")]

-- test a2dConverter:
testConverters = 
    do (plotCT' 1e-4
    [(toneA, "Tone A"),
     (d2aConverter DAlinear 1e-3 (a2dConverter 1e-3 toneA),
      "Tone A (A->D->A) converted with linear mode, 1ms sampling period")])
   (plotCT' 1e-4
    [(toneA, "Tone A"),
     (d2aConverter DAhold 1e-3 (a2dConverter 1e-3 toneA),
      "Tone A (A->D->A) converted with hold mode, 1ms sampling period")])

-- test a feed back loop:
block sin = [sin,s1,s2,sout]
    where sout = p2 s1
      s1 = p1 sin s2
      s2 = p3 sout
      -- The individual processes:
      p1 :: Signal (SubsigCT Double) -> Signal (SubsigCT Double)
     -> Signal (SubsigCT Double)
      p2,p3 :: Signal (SubsigCT Double) -> Signal (SubsigCT Double)
      p1 = addCT
      p2 = scaleCT 0.5
      p3 = initCT (zeroCT 0.0005)
testFeedback = plotCT' 0.0001 ss
    where ss = [(sin, "sin"), (s1, "s1"), (s2, "s2"), (sout, "sout")]
      [sin,s1,s2,sout] = block (takeCT 0.005 toneA)



toneA = sineWave (440.0) (0, 0.02)
toneE = sineWave 520.0 (0, 0.02)
-}

{- Some performance tests on my laptop under cygwin:

***********************************************************************
With ghc:

with 
toneA = sineWave (440.0) (0, 2.0)
toneE = sineWave 520.0 (0, 2.0)

****
we make testAll with Double data types on

ghc --make CTLib.hs -O3 -o ttt.exe
time ttt
   
real    0m33.749s
user    0m0.010s
sys     0m0.010s

****
we make testAll with Rational data types on

ghc --make CTLib.hs -O3 -o ttt.exe
time ttt
   
real    0m53.687s
user    0m0.010s
sys     0m0.010s

****
hence the performance penalty when using Rational instead of Double is
1.59 (60%) longer delay.


************************************************************************
On hugs: (when using 0.2 second long waves, hugs aborted with an out of memory 
message both with Double and Rational; but with Double it aborted much faster;)

toneA = sineWave (440.0) (0, 0.02)
toneE = sineWave 520.0 (0, 0.02)

****************
**** with Double:
time runhugs.exe -h500k CTLib.hs

real    0m1.702s
user    0m0.020s
sys     0m0.010s

******************
**** with Rational:

time runhugs.exe -h500k CTLib.hs

real    0m21.501s
user    0m0.010s
sys     0m0.020s

****************
hence we have a factor of 12.5 longer delay with Rational compared to Double.


-}

--eulerCT :: Signal (SubsigCT a) -> Signal (SubsigCT a)
--eulerCT = undefined

{-
s1 = signal [SubsigCT (sine', (0,6.28)), SubsigCT (\x -> 1, (6.28, 10.0))]

sine' :: (Floating a) => Rational -> a
sine' t = sin (fromRational t)

s2 = mapCT (*2.0) s1
s3 = zipWithCT (+) s1 s2

s4 = delayCT 0.5 (-4.0) s1

integratorCT :: Signal (SubsigCT a) -> Signal (SubsigCT a)
integratorCT = undefined

--eulerCT :: Rational -> Signal (SubsigCT a) -> Signal (SubsigCT a)
--eulerCT step (SubsigCT (f, (t_start, t_end)) :- fs)
--  =  undefined

--eulerCT' :: Rational -> Signal (SubsigCT a) -> Signal (SubsigCT a)
eulerCT stepsize (SubsigCT (f, (t_start, t_end))) =
   signal (SubsigCT (\x -> stepsize * f t_start, (t_start, t_start + stepsize)) :
   eulerCT' stepsize (SubsigCT (f, (t_start + stepsize, t_end))) (stepsize * f t_start))

eulerCT' stepsize (SubsigCT (f, (t_start, t_end))) y_i
  | t_start <= t_end - stepsize
   = SubsigCT (\x -> y_i + stepsize * f t_start, (t_start, t_start + stepsize))
       : eulerCT' stepsize (SubsigCT (f, (t_start + stepsize, t_end))) (y_i + stepsize * f t_start)
  | otherwise
   = []

s6 = signal [SubsigCT (\x -> 1.0, (0.0, 5.0))]
s5 = eulerCT 0.5 (SubsigCT (\x -> 1.0, (0.0, 5.0)))

plotEuler = plotCT' 1e-1 [(s5, "s5")]

ctsig1 = ctSignal [(liftCT sin, (0, 3.14)), (\t -> 1, (3.14, 6.28))]
ctsig2 = ctSignal [(liftCT cos, (0, 6.28))]
-}
