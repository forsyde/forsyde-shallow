module GettingStarted where

import ForSyDe.Shallow

adder in1 in2 = zipWithSY (+) in1 in2

s1 = signal [1,2,3]
s2 = signal [2,3,4]

data Direction = UP 
               | HOLD
               | DOWN deriving (Show)

counter dir = scanldSY count 0 dir

count state HOLD = state
count 4     UP   = 0
count state UP   = state + 1
count 0     DOWN = 4
count state DOWN = state - 1 

binCounter = mealySY binCount carry 0

binCount state HOLD = state
binCount 0     UP   = 1
binCount 1     UP   = 0
binCount 0     DOWN = 1
binCount 1     DOWN = 0

carry 1     UP   = UP
carry 0     DOWN = DOWN
carry _     _    = HOLD

ups = UP :- ups

counter3Bit dir = out
   where out = binCounter s2
         s2  = binCounter s1
         s1  = binCounter dir

counter2Bit = binCounter . binCounter

counterNBit n = foldl (.) id $ replicate n binCounter

counter1Bit' = counterNBit 1
counter2Bit' = counterNBit 2
counter3Bit' = counterNBit 3
