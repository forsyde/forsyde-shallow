-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.Dataflow
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The dataflow library defines data types, process constructors and
-- functions to model dataflow process networks, as described by Lee and
-- Parks in Dataflow process networks, IEEE Proceedings, 1995 ([LeeParks95]).
--
-- Each process is defined by a set of firing rules and corresponding
-- actions. A process fires, if the incoming signals match a firing
-- rule. Then the process consumes the matched tokens and executes the
-- action corresponding to the firing rule.
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.Dataflow
    (
  -- * Data Types       
  -- | The data type @FiringToken@ defines the data type for tokens. The
  --   constructor @Wild@ constructs a token wildcard, the constructor
  --   @Value a@ constructs a token with value @a@.
  -- 
  -- A sequence (pattern) matches a signal, if the sequence is a prefix of
  -- the signal. The following list illustrates the firing rules:
  -- 
  --   * [&#x22A5;] matches always  (/NullS/ in ForSyDe)
  --
  --   * [*] matches signal with at least one token (/[Wild]/ in ForSyDe)
  --
  --   * [v] matches signal with v as its first value (/[Value v]/ in ForSyDe)
  --
  --   * [*,*] matches signals with at least two tokens (/[Wild,Wild]/ in ForSyDe) 
  -- 
  FiringToken(Wild, Value),
  -- * Combinational Process Constructors 
  -- | Combinatorial processes
  -- do not have an internal state. This means, that the output
  -- signal only depends on the input signals.
  --
  -- To illustrate the concept of data flow processes, we create a process that selects tokens from two inputs according to a control signal. 
  --
  -- The process has the following firing rules [LeeParks95]:
  --
  -- 
  --   * R1 = {[*], &#x22A5;, [T]}
  --
  --   * R2 = {&#x22A5;, [*], [F]}
  -- 
  --
  -- The corresponding ForSyDe formulation of the firing rules is:
  --
  -- @
  --  selectRules = [ ([Wild], [], [Value True]),
  --      ([], [Wild], [Value False]) ]
  -- @
  --
  -- For the output we formulate the following set of output functions:
  -- 
  -- @
  --  selectOutput xs ys _  = [ [headS xs], [headS ys] ]
  -- @
  -- 
  -- The select process /selectDF/ is then defined by:
  --
  -- @
  --  selectDF :: Eq a => Signal a -> Signal a 
  --       -> Signal Bool -> Signal a
  --  selectDF =  zipWith3DF selectRules selectOutput
  -- @
  --
  -- Given the signals /s1/, /s2/ and /s3/
  --
  -- @
  --  s1 = signal [1,2,3,4,5,6]
  --  s2 = signal [7,8,9,10,11,12]
  --  s3 = signal [True, True, False, False, True, True]
  -- @
  --
  -- the executed process gives the following results:
  --
  -- @ 
  --  DataflowLib> selectDF s1 s2 s3
  --  {1,2,7,8,3,4} :: Signal Integer
  -- @
  --
  -- The library contains the following combinational process constructors:
  mapDF, zipWithDF, zipWith3DF, 
  -- * Sequential Process Constructors 
  -- | Sequential processes have
  -- an internal state. This means, that the output signal may
  -- depend internal state and on the input signal. 
  --     
  -- As an example we can view a process calculating the running sum
  -- of the input tokens. It has only one firing rule, which is
  -- illustrated below.
  --
  -- @
  --  Firing Rule    Next State    Output
  --  ------------------------------------
  --  (*,[*])    state + x     {state}
  -- @
  --
  -- A dataflow process using these firing rules and the initial state 0 can be formulated in ForSyDe as 
  --
  -- @
  --  rs xs = mealyDF firingRule nextState output initState xs
  --     where 
  --    firingRule    = [(Wild, [Wild])]
  --    nextState state xs = [(state + headS xs)]
  --    output state _    = [[state]]
  --    initState     = 0
  -- @
  --
  -- Execution of the process gives
  --
  -- @     
  --  DataflowLib> rs (signal[1,2,3,4,5,6])
  --    {0,1,3,6,10,15} :: Signal Integer
  -- @
  -- 
  -- Another 'running sum' process /rs2/ takes two tokens, pushes
  -- them into a queue of five elements and calculates the sum as
  -- output.
  --
  -- @
  --  rs2 = mealyDF fs ns o init
  --     where 
  --    init    = [0,0,0,0,0]
  --    fs      = [(Wild, ([Wild, Wild]))]
  --    ns state xs = [drop 2 state ++ fromSignal (takeS 2 xs)]
  --    o state _   = [[(sum state)]]
  -- @
  -- 
  -- Execution of the process gives
  --
  -- @
  --  DataflowLib>rs2 (signal [1,2,3,4,5,6,7,8,9,10])
  --  {0,3,10,20,30} :: Signal Integer
  -- @
  scanlDF, mooreDF, mealyDF
    ) where

import ForSyDe.Shallow.Core 


------------------------------------------------------------------------
--
-- DATA TYPES
--
------------------------------------------------------------------------

data FiringToken a = Wild
       | Value a deriving (Eq, Show)


------------------------------------------------------------------------
--
-- COMBINATIONAL PROCESS CONSTRUCTORS
--
------------------------------------------------------------------------

-- |The process constructor @mapDF@ takes a list of firing rules, a list of corresponding output functions and generates a data flow process with one input and one output signal.
mapDF       :: Eq a => [[FiringToken a]] 
           -> (Signal a -> [[b]]) -> Signal a -> Signal b

mapDF _  _  NullS       =  NullS   
mapDF rs as xs      =  output +-+ mapDF rs as xs'
   where
       xs'      =  if matchedRule < 0 then
              NullS
               else
              consumeDF rule xs
       matchedRule      =  (matchDF rs xs)
       rule         =  rs !! matchedRule
       output       =  if matchedRule < 0 then
              NullS
               else
              signal ((as xs) !! matchedRule)
-- |The process constructors @zipWithDF@ takes a list of firing rules, a list of corresponding output functions to generate a data flow process with two input signals and one output signal.
zipWithDF       :: (Eq a, Eq b) => 
           [([FiringToken b], [FiringToken a])] 
           -> (Signal b -> Signal a -> [[c]]) -> Signal b 
           -> Signal a -> Signal c

zipWithDF _  _  NullS NullS  = NullS
zipWithDF rs as xs  ys     = output +-+ zipWithDF rs as xs' ys'
   where 
      (xs', ys')       = if matchedRule < 0 then
            (NullS, NullS)
             else
            consume2DF rule xs ys
      matchedRule      = (match2DF rs xs ys)
      rule         = rs !! matchedRule
      output       = if matchedRule < 0 then
            NullS
             else
            signal ((as xs ys) !! matchedRule)

-- |The process constructors @zipWith3DF@ takes a list of firing rules, a list of corresponding output functions to generate a data flow process with three input signals and one output signal.
zipWith3DF      :: (Eq a, Eq b, Eq c) => 
           [([FiringToken a],[FiringToken b],[FiringToken c])] 
           -> (Signal a -> Signal b -> Signal c -> [[d]]) 
           -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3DF _  _  NullS NullS NullS = NullS
zipWith3DF rs as xs ys zs   = output +-+ zipWith3DF rs as xs' ys' zs'
   where 
     (xs', ys', zs')    = if matchedRule < 0 then
             (NullS, NullS, NullS)
              else
            consume3DF rule xs ys zs
     matchedRule    = (match3DF rs xs ys zs)
     rule       = rs !! matchedRule
     output         = if matchedRule < 0 then
             NullS
              else
             signal ((as xs ys zs) !! matchedRule)


------------------------------------------------------------------------
--
-- SEQUENTIAL PROCESS CONSTRUCTORS
--
------------------------------------------------------------------------
-- | The process constructor @scanlDF@ implements a finite state machine without output decoder in the ForSyDe methodology. It takes a set of firing rules and a set of corresponding next state functions as arguments. A firing rule is a tuple. The first value is a pattern for the state, the second value corresponds to an input pattern. When a pattern matches, the process fires, the corresponding next state is executed, and the tokens matching the pattern are consumed.
scanlDF           :: (Eq a, Eq b) => [(FiringToken b,[FiringToken a])]      
             -> (b -> Signal a -> [b]) 
             -> b -> Signal a -> Signal b
scanlDF _  _  _     NullS   = NullS
scanlDF fs ns state xs      = (unitS state) 
              +-+ scanlDF fs ns state' xs'
   where 
       xs'      = if matchedRule < 0 then
             NullS
              else
             consumeDF rule xs
       matchedRule      = matchStDF fs state xs
       rule         = snd (fs !! matchedRule)
       state'       = if matchedRule < 0 then
             error "No rule matches the pattern!"
              else
             (ns state xs) !! matchedRule

-- | The process constructor @mooreDF@ implements a Moore finite state machine in the ForSyDe methodology. It takes a set of firing rules, a set of corresponding next state functions and a set of output functions as argument. A firing rule is a tuple. The first value is a pattern for the state, the second value corresponds to an input pattern. When a pattern matches, the process fires, the corresponding next state and output functions are executed, and the tokens matching the pattern are consumed.
mooreDF           :: (Eq a, Eq b) => [(FiringToken b,[FiringToken a])] 
             -> (b -> Signal a -> [b]) -> (b -> [c]) 
             -> b -> Signal a -> Signal c
mooreDF _  _  _ _     NullS     = NullS
mooreDF fs ns o state xs    = output +-+ mooreDF fs ns o state' xs'
   where 
       xs'      = if matchedRule < 0 then
             NullS
              else
             consumeDF rule xs
       matchedRule      = matchStDF fs state xs
       rule         = snd (fs !! matchedRule)
       output       = signal (o state)
       state'       = if matchedRule < 0 then
             error "No rule matches the pattern!"
              else
             (ns state xs) !! matchedRule 


-- | The process constructor @mealyDF@ implements the most general state machine in the ForSyDe methodology. It takes a set of firing rules, a set of corresponding next state functions and a set of output functions as argument. A firing rule is a tuple. The first value is a pattern for the state, the second value corresponds to an input pattern. When a pattern matches, the process fires, the corresponding next state and output functions are executed, and the tokens matching the pattern are consumed.
mealyDF     :: (Eq a, Eq b) => [(FiringToken b,[FiringToken a])] 
    -> (b -> Signal a -> [b]) -> (b -> Signal a -> [[c]]) 
    -> b -> Signal a -> Signal c
mealyDF _  _  _ _     NullS     = NullS
mealyDF fs ns o state xs    = output +-+ mealyDF fs ns o state' xs'
   where 
       xs'      = if matchedRule < 0 then
             NullS
              else
             consumeDF rule xs
       matchedRule      = matchStDF fs state xs
       rule         = snd (fs !! matchedRule)
       output       = signal ((o state xs) !! matchedRule)
       state'       = if matchedRule < 0 then
             error "No rule matches the pattern!"
              else
             (ns state xs) !! matchedRule  


------------------------------------------------------------------------
--
-- SUPPORTING FUNCTIONS
--
------------------------------------------------------------------------

-- The function 'prefixDF' takes a pattern and a signal and returns
-- 'True', if the pattern is a prefix from the signal.
prefixDF        :: Eq a => [FiringToken a] -> Signal a -> Bool
prefixDF []     _   =  True
prefixDF _      NullS   =  False
prefixDF (Wild:ps)  (_:-xs)     =  prefixDF ps xs
prefixDF ((Value p):ps) (x:-xs) =  if p == x then
              prefixDF ps xs
               else
              False

-- The function 'consumeDF' takes a pattern and a signal and consumes
-- the pattern from the signal. The functions 'consume2DF' and
-- 'consume3DF' work in the same way as 'consumeDF', but with two and
-- three input signals.
consumeDF           :: Eq a => [FiringToken a] 
               -> Signal a -> Signal a
consumeDF _    NullS    =  NullS           
consumeDF []       xs   =  xs
consumeDF (Wild:ts)    (_:-xs)  =  consumeDF ts xs     
consumeDF (Value t:ts) (x:-xs)  =  if t == x then
              consumeDF ts xs
               else
              error "Tokens not correct"

consume2DF           :: (Eq a, Eq b) => 
            ([FiringToken a], [FiringToken b]) 
            -> Signal a -> Signal b -> (Signal a, Signal b)
consume2DF (px, py) xs ys    =  (consumeDF px xs,
             consumeDF py ys)

consume3DF           :: (Eq a, Eq b, Eq c) => 
            ([FiringToken a], [FiringToken b], [FiringToken c]) 
             -> Signal a -> Signal b -> Signal c 
             -> (Signal a,Signal b,Signal c)
consume3DF (px, py, pz) xs ys zs = (consumeDF px xs,
            consumeDF py ys,
            consumeDF pz zs)

-- The function 'matchDF' checks, which firing rule, starting from 0, is
-- matched by the input signal. If no firing rule matches, the output is
-- '-1'. The functions 'maptch2S' and 'match3DF' work in the same way
-- for two and three inputs.
matchDF             :: (Num a, Eq b) => 
               [[FiringToken b]] -> Signal b -> a
matchDF rs xs       =  matchDF' 0 rs xs
   where matchDF' _ []     _    =  -1
         matchDF' n (r:rs) xs   =  if prefixDF r xs then
                                     n
                                   else
                                     matchDF' (n+1) rs xs

match2DF        :: (Num a, Eq b, Eq c) => 
               [([FiringToken b], [FiringToken c])]
               -> Signal b -> Signal c -> a
match2DF rs xs ys       =  match2DF' 0 rs xs ys
   where match2DF' _ [] _ _     =  -1
         match2DF' n ((rx, ry):rs) xs ys
           =  if prefixDF rx xs &&
                 prefixDF ry ys 
              then
                n
              else
                match2DF' (n+1) rs xs ys

match3DF        :: (Num a, Eq b, Eq c, Eq d) => 
               [([FiringToken b], [FiringToken d], [FiringToken c])]
            -> Signal b -> Signal d -> Signal c -> a
match3DF rs xs ys zs    = match3DF' 0 rs xs ys zs
   where match3DF' _ [] _ _ _   = -1 
         match3DF' n ((rx, ry, rz):rs) xs ys zs 
           =  if prefixDF rx xs &&
                 prefixDF ry ys &&
                 prefixDF rz zs 
              then
                n
              else
                match3DF' (n+1) rs xs ys zs  

-- The function 'matchStDF' works in the same way as 'matchDF', but it looks on patterns that include the state.
matchStDF           :: (Num a, Eq b, Eq c) => 
               [(FiringToken c,[FiringToken b])] 
               -> c -> Signal b -> a
matchStDF rs state xs       = matchStDF' 0 rs state xs
  where matchStDF' _ [] _ _     =  -1
        matchStDF' n (r:rs) state xs    
          =  if prefixDF (snd r) xs && 
                matchState (fst r) state
             then
               n
             else
               matchStDF' (n+1) rs state xs  
        
matchState          :: Eq a => FiringToken a -> a -> Bool
matchState Wild  _      = True
matchState (Value v) x      = x == v 



------------------------------------------------------------------------
--
-- CODE FOR TESTING
--
------------------------------------------------------------------------

{-
selectRules :: [([FiringToken a], [FiringToken a1], [FiringToken Bool])]
selectRules = [ ([Wild], [], [Value True]),
       ([], [Wild], [Value False]) ]


selectOutput :: Signal t1 -> Signal t1 -> t -> [[t1]]
selectOutput xs ys _ =  [ [headS xs], [headS ys] ]

selectDF        :: Eq a => Signal a -> Signal a 
               -> Signal Bool -> Signal a
selectDF        =  zipWith3DF selectRules selectOutput



s1 :: Signal Integer
s1 = signal [1,2,3,4,5,6]
s2 :: Signal Integer
s2 = signal [7,8,9,10,11,12]
s3 :: Signal Bool
s3 = signal [True, True, False, False, True, True]

rs :: (Eq c, Num c) => Signal c -> Signal c
rs xs           = mealyDF firingRule nextState output initState xs
   where firingRule     = [(Wild, [Wild])]
     nextState state xs     = [(state + headS xs)]
     output state _         = [[state]]
     initState      = 0

rs2 :: Signal Integer -> Signal Integer
rs2        = mealyDF fs ns o init
   where init      = [0,0,0,0,0]
     fs        = [(Wild, ([Wild, Wild]))]
     ns state xs   = [drop 2 state ++ fromSignal (takeS 2 xs)]
     o state _     = [[(sum state)]]
-}








