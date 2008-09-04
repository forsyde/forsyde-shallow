{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

-- | The synchronous process library 'SynchronousProcessLib' defines processes for the synchronous computational model. It is based on the synchronous library 'SynchronousLib'.
module ForSyDe.Shallow.SynchronousProcessLib(
			       fifoDelaySY, finiteFifoDelaySY,
			       memorySY, mergeSY, groupSY, counterSY
			     ) where

import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.Queue
import ForSyDe.Shallow.Memory

-- | The process 'fifoDelaySY' implements a synchronous model of a FIFO with infinite size. The FIFOs take a list of values at each event cycle and output one value. There is a delay of one cycle.
fifoDelaySY		:: Signal [a] -> Signal (AbstExt a)

-- | The process 'finiteFifoDelaySY' implements a FIFO with finite size. The FIFOs take a list of values at each event cycle and output one value. There is a delay of one cycle.
finiteFifoDelaySY	:: Int -> Signal [a] -> Signal (AbstExt a)

-- | The process 'memorySY' implements a synchronous memory. It uses access functions of the type 'Read adr'  and 'Write adr value'.
memorySY		:: Int -> Signal (Access a) -> Signal (AbstExt a) 

-- The process 'mergeSY' merges two input signals into a single signal. The process has an internal buffer in order to prevent loss of data. The process is deterministic and outputs events according to their time tag. If there are two valid values at on both signals. The value of the first signal is output first.
mergeSY			:: Signal (AbstExt a) -> Signal (AbstExt a) 
			   -> Signal (AbstExt a)

-- | The process 'counterSY' implements a counter, that counts from min to max. The  process 'counterSY has no input and its output is an infinite signal.
counterSY		:: (Enum a, Ord a) => a -> a -> Signal a

-- | The function 'groupSY' groups values into a vector of size n, which takes n cycles. While the grouping takes place the output from this process consists of absent values.
groupSY :: Int -> Signal a -> Signal (AbstExt (Vector a))

fifoDelaySY xs		=  mooreSY fifoState fifoOutput (queue []) xs

fifoState		:: Queue a -> [a] -> Queue a
fifoState (Q []) xs	=  (Q xs)
fifoState q xs		=  fst (popQ (pushListQ q xs))

fifoOutput		:: Queue a -> AbstExt a
fifoOutput (Q [])	=  Abst
fifoOutput (Q (x:_))	=  Prst x

finiteFifoDelaySY n xs	
   = mooreSY fifoStateFQ fifoOutputFQ (finiteQueue n []) xs

fifoStateFQ :: FiniteQueue a -> [a] -> FiniteQueue a
fifoStateFQ (FQ n []) xs =  (FQ n xs)
fifoStateFQ q xs	 =  fst (popFQ (pushListFQ q xs))

fifoOutputFQ :: FiniteQueue a -> AbstExt a
fifoOutputFQ (FQ _ [])	  =  Abst
fifoOutputFQ (FQ _ (x:_)) =  Prst x

memorySY size xs	=  mealySY ns o (newMem size) xs
  where 
     ns mem (Read x)	=  memState mem (Read x)
     ns mem (Write x v)	=  memState mem (Write x v)
     o mem (Read x)	=  memOutput mem (Read x)
     o mem (Write x v)	=  memOutput mem (Write x v)


mergeSY xs ys		=  moore2SY mergeState mergeOutput [] xs ys
   where 
	 mergeState []      Abst     Abst    = []
         mergeState []	    Abst    (Prst y) = [y]
         mergeState []     (Prst x)  Abst    = [x]
         mergeState []     (Prst x) (Prst y) = [x, y]
         mergeState (_:us)  Abst     Abst    = us
	 mergeState (_:us)  Abst    (Prst y) = us ++ [y]
	 mergeState (_:us) (Prst x)  Abst    = us ++ [x]
         mergeState (_:us) (Prst x) (Prst y) = us ++ [x, y]

	 mergeOutput []	    = Abst
         mergeOutput (u:_) = Prst u	

groupSY k = mealySY f g s0 
  where
    s0 = NullV
    f v x | (lengthV v) == 0 = unitV x
	  | (lengthV v) == k = unitV x 
	  | otherwise        = v <: x
    g v _ | (lengthV v) == 0   = Abst
    g v x | (lengthV v) == k-1 = Prst (v<:x)
    g _ _ | otherwise          = Abst
      
counterSY m n = sourceSY f m
  where 
    f x | x >= n    = m
	| otherwise = succ x


