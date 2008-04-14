{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RelaxedPolyRec,
             PatternGuards #-}
-- The PatternGuards are used to hush innapropiate compiler warnings
-- see http://hackage.haskell.org/trac/ghc/ticket/2017
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process.SynchProc
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides the synchronous process constructors of ForSyDe 
--
----------------------------------------------------------------------------- 
module ForSyDe.Process.SynchProc (
 -- * Combinational process constructors
 -- | Combinational process constructors are used for processes that do not 
 --   have a state.
 mapSY, zipWithSY, zipWith3SY, 
 zipWith4SY, zipWith5SY, zipWith6SY, zipWithxSY, 
 -- * Sequential process constructors
 -- | Sequential process constructors are used for processes that have a state.
 --   One of the input parameters is the initial state.
 delaySY, delaynSY,
 scanlSY, scanl2SY, scanl3SY, scanldSY, scanld2SY,
 scanld3SY, mooreSY, moore2SY, moore3SY, mealySY,
 mealy2SY, mealy3SY, sourceSY, 
 filterSY, fillSY, holdSY,
 -- * Synchronous Processes
 -- | The library contains a few simple processes that are applicable to many 
 -- cases.
 whenSY, zipSY, zip3SY, zip4SY, zip5SY, zip6SY, 
 unzipSY, unzip3SY, unzip4SY, unzip5SY, unzip6SY,
 zipxSY, unzipxSY, mapxSY, 
 fstSY, sndSY, groupSY) where


import ForSyDe.Ids
import ForSyDe.Process.ProcFun
import ForSyDe.Process.ProcVal
import ForSyDe.OSharing
import ForSyDe.Netlist
import ForSyDe.Signal
import ForSyDe.AbsentExt

import qualified Data.Param.FSVec as V
import Data.Param.FSVec hiding ((++), map)
import Data.TypeLevel (Nat, toInt)
import Data.Maybe
import Data.Dynamic
import Data.Typeable
import Language.Haskell.TH.Syntax (Lift)


-----------------------------------
-- Synchronous Process Constructors
-----------------------------------

----------------------------------------                                      
--                                    --                                      
-- COMBINATIONAL PROCESS CONSTRUCTORS --                                      
--                                    --                                      
----------------------------------------

-- | The process constructor 'mapSY' takes an identifier and a 
--   combinational function as arguments and returns a process with one 
--   input signal and one output signal.         
mapSY :: (Typeable a, Typeable b) =>
         ProcId           -- ^ Identifier of the process 
      -> ProcFun (a -> b) -- ^ Function applied to the input signal 
                          --   in every cycle
      -> Signal a         -- ^ Input 'Signal' 
      -> Signal b         -- ^ Output 'Signal'
mapSY id  f s = Signal (newNodeOutSig nodeRef ZipWithNSYOut) 
  where nodeRef = newURef $ Proc id $ ZipWithNSY (procFun2Dyn f) [unSignal s]

-- | The process constructor 'zipWithSY' takes an identifier and a 
--  combinational function as arguments and returns a process with 
--  two input signals and one output signal.
zipWithSY :: (Typeable a, Typeable b, Typeable c) => 
             ProcId                -- ^ Identifier of the process
          -> ProcFun (a -> b -> c) -- ^ Function applied to the input signals 
                                   --   in every cycle
          -> Signal a              -- ^ First input 'Signal'
          -> Signal b              -- ^ Second input 'Signal'
          -> Signal c              -- ^ Output Signal 
zipWithSY id f s1 s2 = Signal (newNodeOutSig nodeRef ZipWithNSYOut)
  where nodeRef = newURef $ Proc id $ 
                     ZipWithNSY (procFun2Dyn f) [unSignal s1,unSignal s2]


-- | The process constructor 'zipWith3SY' takes an identifier and a 
--   combinational function as arguments and returns a process with 
--   three input signals and one output signal. 
zipWith3SY :: (Typeable a, Typeable b, Typeable c, Typeable d) => 
              ProcId                -- ^ Identifier of the process
           -> ProcFun (a -> b -> c -> d) -- ^ Function applied to the input 
                                         -- signals in every cycle
           -> Signal a              -- ^ First input 'Signal'
           -> Signal b              -- ^ Second input 'Signal'
           -> Signal c              -- ^ Third input 'Signal'
           -> Signal d              -- ^ Output Signal 
zipWith3SY id f s1 s2 s3 = Signal (newNodeOutSig nodeRef ZipWithNSYOut)
  where nodeRef = newURef $ Proc id $ 
                    ZipWithNSY (procFun2Dyn f) 
                                   [unSignal s1, 
                                    unSignal s2, 
                                    unSignal s3]



-- | The process constructor 'zipWith4SY' takes an identifier and a 
--   combinational function as arguments and returns a process with 
--   four input signals and one output signal. 
zipWith4SY :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => 
              ProcId                -- ^ Identifier of the process
           -> ProcFun (a -> b -> c -> d -> e) -- ^ Function applied to the  
                                              --   input signals in every cycle
           -> Signal a              -- ^ First input 'Signal'
           -> Signal b              -- ^ Second input 'Signal'
           -> Signal c              -- ^ Third input 'Signal'
           -> Signal d              -- ^ Fourth input 'Signal'
           -> Signal e              -- ^ Output Signal 
zipWith4SY id f s1 s2 s3 s4 = Signal (newNodeOutSig nodeRef ZipWithNSYOut)
  where nodeRef = newURef $ Proc id $ 
                    ZipWithNSY (procFun2Dyn f) 
                                   [unSignal s1, 
                                    unSignal s2, 
                                    unSignal s3,
                                    unSignal s4]

-- | The process constructor 'zipWith4SY' takes an identifier and a 
--   combinational function as arguments and returns a process with 
--   five input signals and one output signal. 
zipWith5SY :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e,
               Typeable f) => 
              ProcId                -- ^ Identifier of the process
           -> ProcFun (a -> b -> c -> d -> e -> f) 
                                              -- ^ Function applied to the  
                                              --   input signals in every cycle
           -> Signal a              -- ^ First input 'Signal'
           -> Signal b              -- ^ Second input 'Signal'
           -> Signal c              -- ^ Third input 'Signal'
           -> Signal d              -- ^ Fourth input 'Signal'
           -> Signal e              -- ^ Fifth input 'Signal'
           -> Signal f              -- ^ Output Signal 
zipWith5SY id f s1 s2 s3 s4 s5 = Signal (newNodeOutSig nodeRef ZipWithNSYOut)
  where nodeRef = newURef $ Proc id $ 
                    ZipWithNSY (procFun2Dyn f) 
                                   [unSignal s1, 
                                    unSignal s2, 
                                    unSignal s3,
                                    unSignal s4,
                                    unSignal s5]



-- | The process constructor 'zipWith4SY' takes an identifier and a 
--   combinational function as arguments and returns a process with 
--   five input signals and one output signal. 
zipWith6SY :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e,
               Typeable f, Typeable g) => 
              ProcId                -- ^ Identifier of the process
           -> ProcFun (a -> b -> c -> d -> e -> f -> g) 
                                              -- ^ Function applied to the  
                                              --   input signals in every cycle
           -> Signal a              -- ^ First input 'Signal'
           -> Signal b              -- ^ Second input 'Signal'
           -> Signal c              -- ^ Third input 'Signal'
           -> Signal d              -- ^ Fourth input 'Signal'
           -> Signal e              -- ^ Fifth input 'Signal'
           -> Signal f              -- ^ Sixth input 'Signal'
           -> Signal g              -- ^ Output Signal 
zipWith6SY id f s1 s2 s3 s4 s5 s6 = Signal (newNodeOutSig nodeRef ZipWithNSYOut)
  where nodeRef = newURef $ Proc id $ 
                    ZipWithNSY (procFun2Dyn f) 
                                   [unSignal s1, 
                                    unSignal s2, 
                                    unSignal s3,
                                    unSignal s4,
                                    unSignal s5,
                                    unSignal s6]

-- | The process constructor 'mapxSY' creates a process network that maps a 
-- function onto all signals in a vector of signals. The identifier is used 
-- as the identifier prefix of the processes created (a number starting with 1
-- will be appended to each identifier)
mapxSY  :: (Nat s, Typeable a, Typeable b) => 
           ProcId
        -> ProcFun (a -> b) 
        -> FSVec s (Signal a) 
        -> FSVec s (Signal b)
mapxSY id f = V.zipWith (\n s -> mapSY (id ++ show n) f s) 
                        (V.reallyUnsafeVector [(1::Int)..]) 

-- | The process constructor 'zipWithxSY' works as 'zipWithSY', but takes a 
--   vector of signals as input.                                             
zipWithxSY      :: (Nat s, Typeable s, Typeable a, Typeable b) => 
                   ProcId
                -> ProcFun (FSVec s a -> b) 
                -> FSVec s (Signal a) 
                -> Signal b
zipWithxSY id f sv = Signal (newNodeOutSig nodeRef ZipWithxSYOut)
  where nodeRef = newURef $ Proc id $ 
                    ZipWithxSY ((vecProcFun2List.contProcFun2Dyn) f) 
                               (map unSignal (V.fromVector sv)) 
        -- Transform the vector argument of a procfun into a list
        vecProcFun2List :: TypedProcFun (FSVec s a -> b) -> 
                           TypedProcFun ([a] -> b)
        vecProcFun2List f = f{tval = \x -> (tval f) (reallyUnsafeVector x)}

                                   

-------------------------------------
--                                 --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--                                 --
-------------------------------------                                   

-- | The process constructor 'delaySY' delays the signal one event cycle      
--   by introducing an initial value at the beginning of the output signal.   
--   Note, that this implies that there is one event (the first) at the       
--   output signal that has no corresponding event at the input signal.       
--   One could argue that input and output signals are not fully synchronized,
--   even though all input events are synchronous with a corresponding output 
--   event. However, this is necessary to initialize feed-back loops.
delaySY :: (Typeable a, Lift a) => 
           ProcId      -- ^ Identifier of the process
        -> a           -- ^ Initial value
        -> Signal a    -- ^ 'Signal' to be delayed         
        -> Signal a    -- ^ Resulting delayed 'Signal'
delaySY id v s = Signal (newNodeOutSig nodeRef DelaySYOut)
  where procVal = mkProcVal v
        nodeRef = newURef $ Proc id $ DelaySY procVal (unSignal s)


-- | The process constructor 'delaynSY' delays the signal n events by 
--   introducing n identical default values. It creates a chain of 'delaySY'
--   processes.
delaynSY :: (Typeable a, Lift a) =>
            ProcId    -- ^ Identifier prefix of the processes
                      --   a number from 1 to `Delay cycles` 
                      --   will be appended to each process.
         -> a         -- ^Initial state
	 -> Int       -- ^ Delay cycles 
	 -> Signal a  -- ^Input signal
	 -> Signal a  -- ^Output signal
delaynSY id e n s = (\(a,_,_) -> a) $ delaynSYacum (s, 1, n)
  where delaynSYacum acum@(lastSig, curr, max)
            | curr > max = acum
            | otherwise  = (delaySY (id ++ show curr) e lastSig, curr+1, max) 
         


-- | The process constructor 'scanlSY' is used to construct a finite state 
--   machine process without output decoder. It takes an initial value and 
--   a function for the next state decoder. The process constructor behaves 
--   similar to the Haskell prelude function 'scanlSY' and has the value of 
--   the new state as its output value as illustrated by the 
--   following example.  
-- 
-- FIXME: change example
--
-- > SynchronousLib\> scanldSY (+) 0 (signal [1,2,3,4])
--
-- > {1,3,6,10} :: Signal Integer
-- 
-- This is in contrast to the function 'scanldSY', which has its current 
-- state as its output value. 
scanlSY	:: (Lift a, Typeable a, Typeable b) =>
           ProcId -- ^Process Identifier
        -> ProcFun (a -> b -> a) -- ^Combinational function for next 
                                 -- state decoder
        -> a -- ^Initial state
	-> Signal b -- ^ Input signal 
	-> Signal a -- ^ Output signal
scanlSY id f mem s = s'
    where s' = zipWithSY zId f (delaySY dId mem s') s 
          zId = id ++ "_scanlSYzipWith"
          dId = id ++ "_scanlSYdelay"

-- | The process constructor 'scanl2SY' behaves like 'scanlSY', but has two 
--   input signals.
scanl2SY :: (Lift a, Typeable a, Typeable b, Typeable c) =>
           ProcId -- ^Process Identifier
        -> ProcFun (a -> b -> c -> a) -- ^Combinational function for next 
                                      -- state decoder
        -> a -- ^Initial state
	-> Signal b -- ^ First Input signal 
        -> Signal c -- ^ Second Input signal
	-> Signal a -- ^ Output signal
scanl2SY id f mem s1 s2 = s'
    where s' = zipWith3SY zId f (delaySY dId mem s') s1 s2 
          zId = id ++ "_scanlSYzipWith3"
          dId = id ++ "_scanlSYdelay"


-- | The process constructor 'scanl2SY' behaves like 'scanlSY', but has two 
--   input signals.
scanl3SY :: (Lift a, Typeable a, Typeable b, Typeable c, Typeable d) =>
           ProcId -- ^Process Identifier
        -> ProcFun (a -> b -> c -> d -> a) -- ^Combinational function for next 
                                           -- state decoder
        -> a -- ^Initial state
	-> Signal b -- ^ First Input signal 
        -> Signal c -- ^ Second Input signal
        -> Signal d -- ^ Third Input signal
	-> Signal a -- ^ Output signal
scanl3SY id f mem s1 s2 s3 = s'
    where s' = zipWith4SY zId f (delaySY dId mem s') s1 s2 s3
          zId = id ++ "_scanl3SYzipWith4"
          dId = id ++ "_scanl3SYdelay"


-- | The process constructor 'scanldSY' is used to construct a finite state
--  machine process without output decoder. It takes an initial value and a
--  function for the next state decoder. The process constructor behaves
--  similarly to the Haskell prelude function 'scanlSY'. In contrast to the
--  process constructor 'scanlSY' here the output value is the current state
--  and not the one of the next state.
--
-- FIXME: change example
--
-- > SynchronousLib> scanlSY (+) 0 (signal [1,2,3,4])
--
-- > {0,1,3,6} :: Signal Integer
scanldSY :: (Lift a, Typeable a, Typeable b) => 
           ProcId
        -> ProcFun (a -> b -> a) -- ^Combinational function 
                                 -- for next state decoder
        -> a -- ^Initial state
        -> Signal b -- ^ Input signal 
        -> Signal a -- ^ Output signal
scanldSY id f mem s = s'
    where s' = delaySY dId mem $ zipWithSY zId f s' s 
          zId = id ++ "_scanldSYzipWith"
          dId = id ++ "_scanldSYdelay"

-- | The process constructor 'scanld2SY' behaves like 'scanldSY', but has 
--   two input signals.
scanld2SY :: (Lift a, Typeable a, Typeable b, Typeable c) => 
            ProcId
         -> ProcFun (a -> b -> c -> a) -- ^Combinational function 
                                       -- for next state decoder
         -> a -- ^Initial state
         -> Signal b -- ^ First Input signal
         -> Signal c -- ^ Second Input signal
         -> Signal a -- ^ Output signal
scanld2SY id f mem s1 s2 = s'
    where s' = delaySY dId mem $ zipWith3SY zId f s' s1 s2 
          zId = id ++ "_scanld2SYzipWith3"
          dId = id ++ "_scanld2SYdelay"



-- | The process constructor 'scanld2SY' behaves like 'scanldSY', but has 
--   two input signals.
scanld3SY :: (Lift a, Typeable a, Typeable b, Typeable c, Typeable d) => 
            ProcId
         -> ProcFun (a -> b -> c -> d -> a) -- ^Combinational function 
                                       -- for next state decoder
         -> a -- ^Initial state
         -> Signal b -- ^ First Input signal
         -> Signal c -- ^ Second Input signal
         -> Signal d -- ^ Second Input signal
         -> Signal a -- ^ Output signal
scanld3SY id f mem s1 s2 s3 = s'
    where s' = delaySY dId mem $ zipWith4SY zId f s' s1 s2 s3 
          zId = id ++ "_scanld3SYzipWith4"
          dId = id ++ "_scanld3SYdelay"


-- | The process constructor 'mooreSY' is used to model state machines
-- of \"Moore\" type, where the output only depends on the current
-- state. The process constructor is based on the process constructor
-- 'scanldSY', since it is natural for state machines in hardware, that
-- the output operates on the current state and not on the next
-- state. The process constructors takes a function to calculate the
-- next state, another function to calculate the output and a value for
-- the initial state.
--
-- In contrast the output of a process created by the process constructor
-- 'mealySY' depends not only on the state, but also on the input values.
mooreSY :: (Lift a, Typeable a, Typeable b, Typeable c) =>
           ProcId
        -> ProcFun (a -> b -> a) -- ^ Combinational function for
                                 --   next state decoder
        -> ProcFun (a -> c) -- ^Combinational function for output decoder
        -> a -- ^Initial state
        -> Signal b -- ^Input signal
        -> Signal c -- ^Output signal
mooreSY id nextState output initial
        = mapSY (id ++ "_mooreSYmap") output .
          (scanldSY (id ++ "_mooreSYscanld") nextState initial)


-- | The process constructor 'moore2SY' behaves like 'mooreSY', but has two 
--   input signals.
moore2SY :: (Lift a, Typeable a, Typeable b, Typeable c, Typeable d) =>
            ProcId
         -> ProcFun (a -> b -> c -> a) -- ^ Combinational function for
                                       --   next state decoder
         -> ProcFun (a -> d) -- ^Combinational function for output decoder
         -> a -- ^Initial state
         -> Signal b -- ^First Input signal
         -> Signal c -- ^Second Input signal
         -> Signal d -- ^Output signal
moore2SY id nextState output initial i1 i2
        = mapSY (id ++ "_moore2SYmap") output $
          (scanld2SY (id ++ "_moore2SYscanld2") nextState initial i1 i2)



-- | The process constructor 'moore2SY' behaves like 'mooreSY', but has two 
--   input signals.
moore3SY :: (Lift a, 
             Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) =>
            ProcId
         -> ProcFun (a -> b -> c -> d -> a) -- ^ Combinational function for
                                            --   next state decoder
         -> ProcFun (a -> e) -- ^Combinational function for output decoder
         -> a -- ^Initial state
         -> Signal b -- ^First Input signal
         -> Signal c -- ^Second Input signal
         -> Signal d -- ^Third Input signal
         -> Signal e -- ^Output signal
moore3SY id nextState output initial i1 i2 i3
        = mapSY (id ++ "_moore2SYmap") output $
          (scanld3SY (id ++ "_moore2SYscanld2") nextState initial i1 i2 i3)


-- | The process constructor 'melaySY' is used to model state machines of
-- \"Mealy\" type, where the output only depends on the current state and
-- the input values. The process constructor is based on the process
-- constructor 'scanldSY', since it is natural for state machines in
-- hardware, that the output operates on the current state and not on the
-- next state. The process constructors takes a function to calculate the
-- next state, another function to calculate the output and a value for the
-- initial state.
--
-- In contrast the output of a process created by the process constructor 
-- 'mooreSY' depends only on the state, but not on the input values.
mealySY :: (Lift a, Typeable a, Typeable b, Typeable c) => 
           ProcId
        -> ProcFun (a -> b -> a) -- ^Combinational function for next 
                                 -- state decoder  
        -> ProcFun (a -> b -> c) -- ^Combinational function for output decoder
        -> a -- ^Initial state
        -> Signal b -- ^Input signal 
        -> Signal c -- ^Output signal
mealySY id nextState output initial i = 
       zipWithSY (id ++ "_mealySYzipWith") output state i
 where state = scanldSY (id ++ "_mealySYscanld") nextState initial i

-- | The process constructor 'mealy2SY' behaves like 'mealySY', but has 
--   two input signals.
mealy2SY :: (Lift a, Typeable a, Typeable b, Typeable c, Typeable d) => 
           ProcId
        -> ProcFun (a -> b -> c -> a) -- ^Combinational function for next 
                                      -- state decoder  
        -> ProcFun (a -> b -> c -> d) -- ^Combinational function for output 
                                      -- decoder
        -> a -- ^Initial state
        -> Signal b -- ^First Input signal
        -> Signal c -- ^Second Input signal 
        -> Signal d -- ^Output signal
mealy2SY id nextState output initial i1 i2 = 
       zipWith3SY (id ++ "_mealySYzipWith3") output state i1 i2
 where state = scanld2SY (id ++ "_mealySYscanld2") nextState initial i1 i2


-- | The process constructor 'mealy2SY' behaves like 'mealySY', but has 
--   two input signals.
mealy3SY :: (Lift a, Typeable a, Typeable b, Typeable c, Typeable d, 
             Typeable e) => 
           ProcId
        -> ProcFun (a -> b -> c -> d -> a) -- ^Combinational function for next 
                                           -- state decoder  
        -> ProcFun (a -> b -> c -> d -> e) -- ^Combinational function for 
                                           -- output decoder
        -> a -- ^Initial state
        -> Signal b -- ^First Input signal
        -> Signal c -- ^Second Input signal 
        -> Signal d -- ^Third Input signal 
        -> Signal e -- ^Output signal
mealy3SY id nextState output initial i1 i2 i3 = 
       zipWith4SY (id ++ "_mealySYzipWith4") output state i1 i2 i3
 where state = scanld3SY (id ++ "_mealySYscanld3") nextState initial i1 i2 i3



-- | The process constructor 'filterSY' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filterSY       :: Typeable a =>
                  ProcId
               -> ProcFun (a -> Bool) -- ^ Predicate function
               -> Signal a -- ^ Input signal
               -> Signal (AbstExt a) -- ^ Output signal
filterSY id pred = mapSY id (filterer `defArgPF` pred) 
 where filterer = 
        $(newProcFun [d| filterer :: (a -> Bool) -> a -> AbstExt a
                         filterer pred val = 
                              if pred val then Prst val 
                                          else Abst  |])

-- | The process 'sourceSY' takes a function and an initial state and generates
--   an infinite signal starting with the initial state as first output 
--   followed by the recursive application of the function on the current 
--   state. The state also serves as output value.             
--                                                                            
-- The process that has the infinite signal of natural numbers as output is 
-- con structed by                                                            
--                                                                           
-- sourceSY \"naturals\" (+1) 0
sourceSY :: (Typeable a, Lift a) =>
            ProcId 
         -> ProcFun (a -> a) 
         -> a 
         -> Signal a
sourceSY id f s0 = o
 where o = delaySY ("sourceSY_" ++ id ++ "_delaySY") s0 s
       s = mapSY  ("sourceSY_" ++ id ++ "_mapSY") f o




-- | The process constructor 'fillSY' creates a process that 'fills' a signal 
--   with present values by replacing absent values with a given value. The 
--   output signal is not any more of the type 'AbstExt'.        
fillSY :: (Typeable a, Lift a) =>
          ProcId
       -> a                  -- ^Default value                                
       -> Signal (AbstExt a) -- ^Absent extended input signal             
       -> Signal a           -- ^Output signal                                
fillSY id v s = mapSY id (replaceAbst `defArgVal` v)  s
  where replaceAbst :: ProcFun (a -> AbstExt a -> a)
        replaceAbst = $(newProcFun 
                          [d| replaceAbst :: a -> AbstExt a -> a
                              replaceAbst x y = fromAbstExt x y |])     

-- | The process constructor 'holdSY' creates a process that 'fills' a signal 
--   with values by replacing absent values by the preceding present value. 
--   Only in cases, where no preceding value exists, the absent value is 
--   replaced by a default value. The output signal is not any more of the 
--   type 'AbstExt'.
holdSY :: (Lift a, Typeable a) => 
          ProcId -- ^Default value 
       -> a
       -> Signal (AbstExt a) -- ^Absent extended input signal 
       -> Signal a -- ^Output signal
holdSY id a s = scanlSY id hold a s
 where hold = $(newProcFun [d| hold :: a -> AbstExt a -> a
                               hold a abs = fromAbstExt a abs |])

---------------------------
--                       --
-- SYNCHRONOUS PROCESSES --
--                       --
---------------------------


-- | The process constructor 'whenSY' creates a process that synchronizes a 
--   signal of absent extended values with another signal of absent extended 
--   values. The output signal has the value of the first signal whenever an 
--   event has a present value and 'Abst' when the event has an absent value.
whenSY :: (Typeable a, Typeable b) => 
           ProcId
        -> Signal (AbstExt a) -> Signal (AbstExt b) -> Signal (AbstExt a)
whenSY id = zipWithSY id whenF 
  where whenF = $(newProcFun [d| whenF :: AbstExt a -> AbstExt b -> AbstExt a
                                 whenF v1 v2 = if isAbsent v2
                                                  then Abst
                                                  else v1 |])


-- | The process 'zipSY' \"zips\" two incoming signals into one signal of 
--   tuples.
zipSY :: (Typeable a, Typeable b) => 
         ProcId
      -> Signal a 
      -> Signal b 
      -> Signal (a,b)
zipSY id = zipWithSY id tup2
  where tup2 :: ProcFun (a -> b -> (a,b))
        tup2 = $(newProcFun [d| tup2 :: a -> b -> (a,b)
                                tup2 a b = (a,b) |])

-- | The process 'zip3SY' works as 'zipSY', but takes three input signals.
zip3SY :: (Typeable a, Typeable b, Typeable c) => 
          ProcId ->
          Signal a -> 
          Signal b -> 
          Signal c ->
          Signal (a,b,c)
zip3SY id = zipWith3SY id tup3
  where tup3 :: ProcFun (a -> b -> c -> (a,b,c))
        tup3 = $(newProcFun [d| tup3 :: a -> b -> c -> (a,b,c)
                                tup3 a b c = (a,b,c) |])


-- | The process 'zip4SY' works as 'zipSY', but takes four input signals.
zip4SY :: (Typeable a, Typeable b, Typeable c, Typeable d) => 
          ProcId -> 
          Signal a -> 
          Signal b -> 
          Signal c ->
          Signal d ->
          Signal (a,b,c,d)
zip4SY id = zipWith4SY id tup4
  where tup4 :: ProcFun (a -> b -> c -> d -> (a,b,c,d))
        tup4 = $(newProcFun [d| tup4 :: a -> b -> c -> d -> (a,b,c,d)
                                tup4 a b c d = (a,b,c,d) |])


-- | The process 'zip5SY' works as 'zipSY', but takes five input signals.
zip5SY :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => 
          ProcId ->
          Signal a -> 
          Signal b -> 
          Signal c ->
          Signal d ->
          Signal e ->
          Signal (a,b,c,d,e)
zip5SY id = zipWith5SY id tup5
  where tup5 :: ProcFun (a -> b -> c -> d -> e -> (a,b,c,d,e))
        tup5 = $(newProcFun [d| tup5 :: a -> b -> c -> d -> e -> (a,b,c,d,e)
                                tup5 a b c d e = (a,b,c,d,e) |])


-- | The process 'zip6SY' works as 'zipSY', but takes six input signals.
zip6SY :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, 
           Typeable f) =>
          ProcId -> 
          Signal a -> 
          Signal b -> 
          Signal c ->
          Signal d ->
          Signal e ->
          Signal f ->
          Signal (a,b,c,d,e,f)
zip6SY id = zipWith6SY id tup6
  where tup6 :: ProcFun (a -> b -> c -> d -> e -> f -> (a,b,c,d,e,f))
        tup6 = $(newProcFun [d| tup6 :: a -> b -> c -> d -> e -> f -> 
                                        (a,b,c,d,e,f)
                                tup6 a b c d e f = (a,b,c,d,e,f) |])


-- | The process 'unzipSY' \"unzips\" a signal of tuples into two signals.
unzipSY :: forall a b . (Typeable a, Typeable b) =>  
          ProcId
       -> Signal (a,b) 
       -> (Signal a,Signal b)
unzipSY id s = (Signal (newNodeOutSig nodeRef (UnzipNSYOut 1)),
                Signal (newNodeOutSig nodeRef (UnzipNSYOut 2)))
  where ts = [typeOf (undefined :: a), typeOf (undefined :: b)]
        nodeRef = newURef $ Proc id $ 
                     UnzipNSY ts untup (unSignal s)
        untup :: Dynamic -> [Dynamic]
        untup i = let (t1,t2) = ((fromJust.fromDynamic) i) :: (a,b)    
                  in [toDyn t1, toDyn t2]



-- | The process 'unzip3SY' \"unzips\" a signal of tuples into three signals.
unzip3SY :: forall a b c . (Typeable a, Typeable b, Typeable c) =>  
           ProcId 
        -> Signal (a,b,c) 
        -> (Signal a, Signal b, Signal c)
unzip3SY id s = (Signal (newNodeOutSig nodeRef (UnzipNSYOut 1)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 2)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 3)))
  where ts = [typeOf (undefined :: a), typeOf (undefined :: b),
              typeOf (undefined :: c)]
        nodeRef = newURef $ Proc id $ 
                     UnzipNSY ts untup3 (unSignal s)
        untup3 :: Dynamic -> [Dynamic]
        untup3 i = let (t1,t2,t3) = ((fromJust.fromDynamic) i) :: (a,b,c)    
                   in [toDyn t1, toDyn t2, toDyn t3]


-- | The process 'unzip4SY' \"unzips\" a signal of tuples into four signals.
unzip4SY :: forall a b c d . (Typeable a, Typeable b, Typeable c, 
                              Typeable d) => 
           ProcId   
        -> Signal (a,b,c,d) 
        -> (Signal a, Signal b, Signal c, Signal d)
unzip4SY id s = (Signal (newNodeOutSig nodeRef (UnzipNSYOut 1)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 2)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 3)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 4)))
  where ts = [typeOf (undefined :: a), typeOf (undefined :: b),
              typeOf (undefined :: c), typeOf (undefined :: d)]
        nodeRef = newURef $ Proc id $ 
                     UnzipNSY ts untup4 (unSignal s)
        untup4 :: Dynamic -> [Dynamic]
        untup4 i = let (t1,t2,t3,t4) = ((fromJust.fromDynamic) i) :: (a,b,c,d)
                   in [toDyn t1, toDyn t2, toDyn t3, toDyn t4]


-- | The process 'unzip5SY' \"unzips\" a signal of tuples into five signals.
unzip5SY :: forall a b c d e . (Typeable a, Typeable b, Typeable c, 
                                Typeable d, Typeable e) => 
           ProcId 
        -> Signal (a,b,c,d,e) 
        -> (Signal a, Signal b, Signal c, Signal d, Signal e)
unzip5SY id s = (Signal (newNodeOutSig nodeRef (UnzipNSYOut 1)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 2)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 3)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 4)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 5)))
  where ts = [typeOf (undefined :: a), typeOf (undefined :: b),
              typeOf (undefined :: c), typeOf (undefined :: d),
              typeOf (undefined :: e)]
        nodeRef = newURef $ Proc id $ 
                     UnzipNSY ts untup5 (unSignal s)
        untup5 :: Dynamic -> [Dynamic]
        untup5 i = let (t1,t2,t3,t4,t5) 
                        = ((fromJust.fromDynamic) i) :: (a,b,c,d,e)
                   in [toDyn t1, toDyn t2, toDyn t3, toDyn t4, toDyn t5]


-- | The process 'unzip6SY' \"unzips\" a signal of tuples into six signals.
unzip6SY :: forall a b c d e f . (Typeable a, Typeable b, Typeable c, 
                                  Typeable d, Typeable e, Typeable f) =>  
           ProcId 
        -> Signal (a,b,c,d,e,f) 
        -> (Signal a, Signal b, Signal c, Signal d, Signal e, Signal f)
unzip6SY id s = (Signal (newNodeOutSig nodeRef (UnzipNSYOut 1)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 2)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 3)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 4)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 5)),
                 Signal (newNodeOutSig nodeRef (UnzipNSYOut 6)))
  where ts = [typeOf (undefined :: a), typeOf (undefined :: b),
              typeOf (undefined :: c), typeOf (undefined :: d),
              typeOf (undefined :: e), typeOf (undefined :: f)]
        nodeRef = newURef $ Proc id $ 
                     UnzipNSY ts untup6 (unSignal s)
        untup6 :: Dynamic -> [Dynamic]
        untup6 i = let (t1,t2,t3,t4,t5,t6) 
                        = ((fromJust.fromDynamic) i) :: (a,b,c,d,e,f)
                   in [toDyn t1, toDyn t2, toDyn t3, toDyn t4, toDyn t5, 
                       toDyn t6]

-- | The process 'zipxSY' \"zips\" a signal of vectors into a vector of signals.
zipxSY :: (Nat s, Typeable s, Typeable a) =>
          ProcId
       -> FSVec s (Signal a) 
       -> Signal (FSVec s a)
zipxSY id = zipWithxSY id vectId 
  where vectId = $(newProcFun [d| vectId :: FSVec s a -> FSVec s a
                                  vectId v = v |]) 

-- | The process 'unzipxSY' \"unzips\" a vector of n signals into a signal of 
--   vectors.
unzipxSY :: forall s a . (Typeable s, Nat s, Typeable a) => 
            ProcId
         -> Signal (FSVec s a) 
         -> FSVec s (Signal a)
unzipxSY id vs = V.map (\tag -> Signal (newNodeOutSig nodeRef tag) )
                        (reallyUnsafeVector [UnzipxSYOut i | i <- [1..n]])
  where n = toInt (undefined :: s)
        t = typeOf (undefined :: a)
        nodeRef = newURef $ Proc id $
                    UnzipxSY t n unvector (unSignal vs)
        unvector :: Dynamic -> [Dynamic]
        unvector i = let v = ((fromJust.fromDynamic) i) :: FSVec s a
                     in map toDyn (V.fromVector v)

-- | The process 'fstSY' selects always the first value from a signal of pairs
fstSY :: (Typeable a, Typeable b) => ProcId -> Signal (a,b) -> Signal a
fstSY id = mapSY id  first
  where first = $(newProcFun [d| first :: (a,b) -> a
                                 first (a,_) = a |])


-- | The process 'sndSY' selects always the second value from a signal of pairs
sndSY :: (Typeable a, Typeable b) => ProcId -> Signal (a,b) -> Signal b
sndSY id = mapSY id second
  where second = $(newProcFun [d| second :: (a,b) -> b
                                  second (_,b) = b |])



-- | The function 'groupSY' groups values into a vector of size n, which takes 
--   n cycles. While the grouping takes place the output from this process 
--   consists of absent values.
groupSY :: forall k a . (Nat k, Typeable k, Lift k, Typeable a, Lift a) => 
           ProcId -> k -> Signal a -> Signal (AbstExt (FSVec k a))
groupSY id k = mooreSY id (f `defArgVal` kV)  (g `defArgVal` kV) s0 
  where
   kV = toInt k
   -- FIXME, FIXME, this won't work in th VHDL backend
   --               due to the undefined and probably unsafeReplace
   s0 = (0, V.copy k (undefined :: a)) 
   f = $(newProcFun [d| f :: Nat k' => Int -> (Int, FSVec k' a') -> a' -> 
                             (Int, FSVec k' a')
                        f k (count,v)  a =
                           (count+1 `mod` k, unsafeReplace v count a) |])
   g = $(newProcFun [d| g :: Nat k' => Int -> (Int, FSVec k' a') -> AbstExt (FSVec k' a') 
                        g k (count,v) = if  k-1 == count then Prst v else Abst |])
   unsafeReplace :: Nat s => FSVec s a' -> Int -> a' ->FSVec s a'
   unsafeReplace v i a = 
      reallyUnsafeVector $ unsafeReplace' (fromVector v) i a
     where unsafeReplace' []     _ _ = []
           unsafeReplace' (_:xs) 0 y = (y:xs)
           unsafeReplace' (x:xs) n y = x : (unsafeReplace' xs (n - 1) y)

