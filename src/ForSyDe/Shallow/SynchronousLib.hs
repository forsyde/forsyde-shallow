-- | The synchronuous library 'SynchronousLib' defines process constructors and processes for the synchronous computational model. A process constructor is a higher order function which together with combinational function(s) and values as arguments constructs a process.
module ForSyDe.Shallow.SynchronousLib(
--			module ForSyDe.Shallow.CoreLib,
			-- * Combinational process constructors
			-- | Combinational process constructors are used for processes that do not have a state.
			mapSY, zipWithSY, zipWith3SY, 
			zipWith4SY, zipWithxSY, 
			-- * Sequential process constructors
			-- | Sequential process constructors are used for processes that have a state. One of the input parameters is the initial state.
			delaySY, delaynSY,
			scanlSY, scanl2SY, scanl3SY, scanldSY, scanld2SY,
			scanld3SY, mooreSY, moore2SY, moore3SY, mealySY,
			mealy2SY, mealy3SY, sourceSY, 
			filterSY, fillSY, holdSY,
			-- * Synchronous Processes
			-- | The library contains a few simple processes that are applicable to many cases.
			whenSY, zipSY, zip3SY, zip4SY, zip5SY, zip6SY, 
                        unzipSY, unzip3SY, unzip4SY, unzip5SY, unzip6SY,
                        zipxSY, unzipxSY, mapxSY, 
			fstSY, sndSY
        	     ) where

import ForSyDe.Shallow.CoreLib



----------------------------------------
--                                    --
-- COMBINATIONAL PROCESS CONSTRUCTORS --
--                                    --
----------------------------------------

-- | The process constructor 'mapSY' takes a combinational function as argument and returns a process with one input signal and one output signal.
mapSY		:: (a -> b) -> Signal a	-> Signal b
mapSY _ NullS	= NullS
mapSY f (x:-xs)	= f x :- (mapSY f xs)

-- | The process constructor 'zipWithSY' takes a combinational function as argument and returns a process with two input signals and one output signal.
zipWithSY	:: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWithSY _ NullS   _       = NullS
zipWithSY _ _       NullS   = NullS
zipWithSY f (x:-xs) (y:-ys) = f x y :- (zipWithSY f xs ys)

-- | The process constructor 'zipWith3SY' takes a combinational function as argument and returns a process with three input signals and one output signal.
zipWith3SY	:: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3SY _ NullS   _       _       = NullS
zipWith3SY _ _       NullS   _       = NullS
zipWith3SY _ _       _       NullS   = NullS
zipWith3SY f (x:-xs) (y:-ys) (z:-zs) = f x y z 
				       :- (zipWith3SY f xs ys zs)

-- | The process constructor 'zipWith4SY' takes a combinational function as argument and returns a process with four input signals and one output signal.
zipWith4SY	:: (a -> b -> c -> d -> e) -> Signal a -> Signal b 
		-> Signal c -> Signal d	-> Signal e
zipWith4SY _ NullS   _       _       _      = NullS
zipWith4SY _ _       NullS   _       _      = NullS
zipWith4SY _ _       _       NullS   _      = NullS
zipWith4SY _ _       _       _       NullS  = NullS
zipWith4SY f (w:-ws) (x:-xs) (y:-ys) (z:-zs) 
				      = f w x y z 
				        :- (zipWith4SY f ws xs ys zs)

-- | The process constructor 'mapxSY' creates a process network that maps a function onto all signals in a vector of signals.
mapxSY		:: (a -> b) -> Vector (Signal a) -> Vector (Signal b)
mapxSY f = mapV (mapSY f)

-- | The process constructor 'zipWithxSY' works as 'zipWithSY', but takes a vector of signals as input.
zipWithxSY	:: (Vector a -> b) -> Vector (Signal a)	-> Signal b
zipWithxSY f = mapSY f . zipxSY



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
delaySY	       :: a -- ^Initial state 
	       -> Signal a -- ^Input signal
	       -> Signal a -- ^Output signal
delaySY e es = e:-es

-- | The process constructor 'delaynSY' delays the signal n events by introducing n identical default values.   
delaynSY       :: a -- ^Initial state
	       -> Int -- ^ Delay cycles 
	       -> Signal a -- ^Input signal
	       -> Signal a -- ^Output signal
delaynSY e n xs | n <= 0    = xs
                | otherwise = e :- delaynSY e (n-1) xs 

-- | The process constructor 'scanlSY' is used to construct a finite state machine process without output decoder. It takes an initial value and a function for the next state decoder. The process constructor behaves similar to the Haskell prelude function 'scanlSY' and has the value of the new state as its output value as illustrated by the following example.  
--
-- > SynchronousLib> scanldSY (+) 0 (signal [1,2,3,4])
--
-- > {1,3,6,10} :: Signal Integer
-- 
-- This is in contrast to the function 'scanldSY', which has its current state as its output value. 
scanlSY	       :: (a -> b -> a) -- ^Combinational function for next state decoder
	       -> a -- ^Initial state
	       -> Signal b -- ^ Input signal 
	       -> Signal a -- ^ Output signal
scanlSY f mem xs = s'
    where s' = zipWithSY f (delaySY mem s') xs 

-- | The process constructor 'scanl2SY' behaves like 'scanlSY', but has two input signals.
scanl2SY       :: (a -> b -> c -> a) -> a -> Signal b -> Signal c -> Signal a
scanl2SY f mem xs ys = s'
    where s' = zipWith3SY f (delaySY mem s') xs ys

-- | The process constructor 'scanl3SY' behaves like 'scanlSY', but has three input signals.
scanl3SY       :: (a -> b -> c -> d -> a) -> a -> Signal b 
	           -> Signal c -> Signal d -> Signal a
scanl3SY f mem xs ys zs = s'
    where s' = zipWith4SY f (delaySY mem s') xs ys zs

-- | The process constructor 'scanldSY' is used to construct a finite state machine process without output decoder. It takes an initial value and a function for the next state decoder. The process constructor behaves similar to the Haskell prelude function 'scanlSY'. In contrast to the process constructor 'scanlSY' here the output value is the current state and not the one of the next state.
--
-- > SynchronousLib> scanlSY (+) 0 (signal [1,2,3,4])
--
-- > {0,1,3,6} :: Signal Integer
scanldSY       :: (a -> b -> a) -- ^Combinational function for next state decoder
	       -> a -- ^Initial state
	       -> Signal b -- ^ Input signal 
	       -> Signal a -- ^ Output signal
scanldSY f mem xs = s'
    where s' = delaySY mem $ zipWithSY f s' xs 


-- | The process constructor 'scanld2SY' behaves like 'scanldSY', but has two input signals.
scanld2SY      :: (a -> b -> c -> a) -> a -> Signal b -> Signal c 
		   -> Signal a
scanld2SY f mem xs ys = s'
    where s' = delaySY mem $ zipWith3SY f s' xs ys

-- | The process constructor 'scanld3SY' behaves like 'scanldSY', but has three input signals.
scanld3SY      :: (a -> b -> c -> d -> a) -> a -> Signal b 
		   -> Signal c -> Signal d -> Signal a
scanld3SY f mem xs ys zs = s'
    where s' = delaySY mem $ zipWith4SY f s' xs ys zs

-- | The process constructor 'mooreSY' is used to model state machines of \"Moore\" type, where the output only depends on the current state. The process constructor is based on the process constructor 'scanldSY', since it is natural for state machines in hardware, that the output operates on the current state and not on the next state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mealySY' depends not only on the state, but also on the input values.
mooreSY	       :: (a -> b -> a) -- ^Combinational function for next state decoder 
	       -> (a -> c) -- ^Combinational function for output decoder
	       -> a -- ^Initial state
	       -> Signal b -- ^Input signal
	       -> Signal c -- ^Output signal
mooreSY nextState output initial 
        = mapSY output . (scanldSY nextState initial)

-- | The process constructor 'moore2SY' behaves like 'mooreSY', but has two input signals.
moore2SY       :: (a -> b -> c -> a) -> (a -> d) -> a -> Signal b 
	           -> Signal c -> Signal d
moore2SY nextState output initial inp1 inp2 =
 	 mapSY output (scanld2SY nextState initial inp1 inp2)

-- | The process constructor 'moore3SY' behaves like 'mooreSY', but has three input signals.
moore3SY       :: (a -> b -> c -> d -> a) -> (a -> e) -> a -> Signal b 
		   -> Signal c -> Signal d -> Signal e
moore3SY nextState output initial inp1 inp2 inp3 =
 	 mapSY output (scanld3SY nextState initial inp1 inp2 inp3)

-- | The process constructor 'melaySY' is used to model state machines of \"Mealy\" type, where the output only depends on the current state and the input values. The process constructor is based on the process constructor 'scanldSY', since it is natural for state machines in hardware, that the output operates on the current state and not on the next state. The process constructors takes a function to calculate the next state, another function to calculate the output and a value for the initial state. 
--
-- In contrast the output of a process created by the process constructor 'mooreSY' depends only on the state, but not on the input values.
mealySY	       :: (a -> b -> a) -- ^Combinational function for next state decoder  
	       -> (a -> b -> c) -- ^Combinational function for output decoder
	       -> a -- ^Initial state
	       -> Signal b -- ^Input signal 
	       -> Signal c -- ^Output signal
mealySY nextState output initial sig =
        zipWithSY output state sig
        where state = scanldSY nextState initial sig

-- | The process constructor 'mealy2SY' behaves like 'mealySY', but has two input signals.
mealy2SY       :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a 
	           -> Signal b -> Signal c -> Signal d
mealy2SY nextState output initial inp1 inp2 =
 	zipWith3SY output (scanld2SY nextState initial inp1 inp2)
	                  inp1 inp2      

-- | The process constructor 'mealy3SY' behaves like 'mealySY', but has three input signals.
mealy3SY       :: (a -> b -> c -> d -> a) -> (a -> b -> c -> d -> e) -> a 
	           -> Signal b -> Signal c -> Signal d -> Signal e
mealy3SY nextState output initial inp1 inp2 inp3 =
        zipWith4SY output (scanld3SY nextState initial inp1 inp2 inp3)
		         inp1 inp2 inp3 

-- | The process constructor 'filterSY' discards the values who do not fulfill a predicate given by a predicate function and replaces them with absent events.
filterSY       :: (a -> Bool) -- Predicate function
		  -> Signal a -- Input signal
		  -> Signal (AbstExt a) -- Output signal
filterSY _ NullS	= NullS
filterSY p (x:-xs)	= if (p x == True) then
			     Prst x :- filterSY p xs
			  else
			     Abst :- filterSY p xs	

-- | The process 'sourceSY' takes a function and an initial state and generates an infinite signal starting with the initial state as first output followed by the recursive application of the function on the current state. The state also serves as output value. 
--
-- The process that has the infinite signal of natural numbers as output is constructed by 
--
-- > SynchronousLib> takeS 5 (sourceSY (+1) 0)
--
-- > {0,1,2,3,4} :: Signal Integer
sourceSY       :: (a -> a) -> a -> Signal a
sourceSY f s0		= o
  where 
    o			= delaySY s0 s
    s			= mapSY f o

-- | The process constructor 'fillSY' creates a process that 'fills' a signal with present values by replacing absent values with a given value. The output signal is not any more of the type 'AbstExt'.
fillSY	     :: a -- ^Default value  
	     -> Signal (AbstExt a) -- ^Absent extended input signal 
	     -> Signal a -- ^Output signal
fillSY a xs = mapSY (replaceAbst a) xs
              where replaceAbst a' Abst     = a'
                    replaceAbst _  (Prst x) = x

-- | The process constructor 'holdSY' creates a process that 'fills' a signal with values by replacing absent values by the preceding present value. Only in cases, where no preceding value exists, the absent value is replaced by a default value. The output signal is not any more of the type 'AbstExt'.
holdSY       :: a -- ^Default value 
	     -> Signal (AbstExt a) -- ^Absent extended input signal 
	     -> Signal a -- ^Output signal
holdSY a xs = scanlSY hold a xs
   	      where hold a' Abst     = a'
 		    hold _  (Prst x) = x



---------------------------
--                       --
-- SYNCHRONOUS PROCESSES --
--                       --
---------------------------

-- | The process constructor 'whenSY' creates a process that synchronizes a signal of absent extended values with another signal of absent extended values. The output signal has the value of the first signal whenever an event has a present value and 'Abst' when the event has an absent value.
whenSY	     :: Signal (AbstExt a) -> Signal (AbstExt b) 
		-> Signal (AbstExt a)
whenSY NullS   _            = NullS
whenSY _       NullS        = NullS
whenSY (_:-xs) (Abst:-ys)   = Abst :- (whenSY xs ys)
whenSY (x:-xs) (_:-ys)      = x    :- (whenSY xs ys)

-- | The process 'zipSY' \"zips\" two incoming signals into one signal of tuples.
zipSY        :: Signal a -> Signal b -> Signal (a,b)
zipSY (x:-xs) (y:-ys) = (x, y) :- zipSY xs ys
zipSY _	     _	     = NullS

-- | The process 'zip3SY' works as 'zipSY', but takes three input signals.
zip3SY       :: Signal a -> Signal b -> Signal c -> Signal (a,b,c)
zip3SY (x:-xs) (y:-ys) (z:-zs) = (x, y, z) :- zip3SY xs ys zs
zip3SY _	     _	_      = NullS

-- | The process 'zip4SY' works as 'zipSY', but takes four input signals.
zip4SY	     :: Signal a -> Signal b -> Signal c -> Signal d 
		-> Signal (a,b,c,d)
zip4SY (w:-ws) (x:-xs) (y:-ys) (z:-zs) = (w, x, y, z) 
					 :- zip4SY ws xs ys zs
zip4SY _	_      _	_      = NullS

-- | The process 'zip5SY' works as 'zipSY', but takes four input signals.
zip5SY	     :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e
		-> Signal (a,b,c,d,e)
zip5SY (x1:-x1s) (x2:-x2s) (x3:-x3s) (x4:-x4s) (x5:-x5s) 
    = (x1,x2,x3,x4,x5) :- zip5SY x1s x2s x3s x4s x5s
zip5SY _	 _         _	     _         _ = NullS

-- | The process 'zip6SY' works as 'zipSY', but takes four input signals.
zip6SY	     :: Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> Signal f -> Signal (a,b,c,d,e,f)
zip6SY (x1:-x1s) (x2:-x2s) (x3:-x3s) (x4:-x4s) (x5:-x5s)  (x6:-x6s) 
    = (x1,x2,x3,x4,x5,x6) :- zip6SY x1s x2s x3s x4s x5s x6s
zip6SY _	 _         _	     _         _          _ = NullS

-- | The process 'unzipSY' \"unzips\" a signal of tuples into two signals.
unzipSY      :: Signal (a,b) -> (Signal a,Signal b)
unzipSY NullS	     = (NullS, NullS)
unzipSY ((x, y):-xys) = (x:-xs, y:-ys) where (xs, ys) = unzipSY xys

-- | The process 'unzip3SY' works as 'unzipSY', but has three output signals.
unzip3SY     :: Signal (a, b, c) -> (Signal a, Signal b, Signal c)
unzip3SY NullS             = (NullS, NullS, NullS)
unzip3SY ((x, y, z):-xyzs) = (x:-xs, y:-ys, z:-zs) where
  			     (xs, ys, zs) = unzip3SY xyzs

-- | The process 'unzip4SY' works as 'unzipSY', but has four output signals.
unzip4SY     :: Signal (a,b,c,d) 
		-> (Signal a,Signal b,Signal c,Signal d)
unzip4SY NullS              = (NullS, NullS, NullS, NullS)
unzip4SY ((w,x,y,z):-wxyzs) = (w:-ws, x:-xs, y:-ys, z:-zs) where
  			      (ws, xs, ys, zs) = unzip4SY wxyzs

-- | The process 'unzip5SY' works as 'unzipSY', but has four output signals.
unzip5SY     :: Signal (a,b,c,d,e) 
		-> (Signal a,Signal b,Signal c,Signal d,Signal e)
unzip5SY NullS              = (NullS, NullS, NullS, NullS,NullS)
unzip5SY ((x1,x2,x3,x4,x5):-xs) = (x1:-x1s, x2:-x2s, x3:-x3s, x4:-x4s, x5:-x5s)
    where (x1s, x2s, x3s, x4s,x5s) = unzip5SY xs

-- | The process 'unzip6SY' works as 'unzipSY', but has four output signals.
unzip6SY     :: Signal (a,b,c,d,e,f) 
		-> (Signal a,Signal b,Signal c,Signal d,Signal e,Signal f)
unzip6SY NullS              = (NullS, NullS, NullS, NullS,NullS,NullS)
unzip6SY ((x1,x2,x3,x4,x5,x6):-xs) 
    = (x1:-x1s, x2:-x2s, x3:-x3s, x4:-x4s, x5:-x5s, x6:-x6s)
    where (x1s, x2s, x3s, x4s, x5s, x6s) = unzip6SY xs

-- | The process 'zipxSY' \"zips\" a signal of vectors into a vector of signals.
zipxSY       :: Vector (Signal a) -> Signal (Vector a)
zipxSY NullV            = NullS
zipxSY (NullS   :> xss) = zipxSY xss
zipxSY ((x:-xs) :> xss) = (x :> (mapV headS xss)) 
 			     :- (zipxSY (xs :> (mapV tailS xss)))

-- | The process 'unzipxSY' \"unzips\" a vector of signals into a signal of vectors.
unzipxSY     :: Signal (Vector a) -> Vector (Signal a)
unzipxSY NullS            = NullV
unzipxSY (NullV   :- vss) = unzipxSY vss
unzipxSY ((v:>vs) :- vss) = (v :- (mapSY headV vss)) 
 			    :> (unzipxSY (vs :- (mapSY tailV vss)))

-- | The process 'fstSY' selects always the first value from a signal of pairs.
fstSY        :: Signal (a,b) -> Signal a
fstSY = mapSY fst		  

-- | The process 'sndSY' selects always the second value from a signal of pairs.
sndSY        :: Signal (a,b) -> Signal b
sndSY = mapSY snd

