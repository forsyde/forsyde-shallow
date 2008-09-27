-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Queue
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--
-- This provides two data types, that can be used to model queue
-- structures, such as FIFOs. There is a data type for an queue of
-- infinite size 'Queue' and one for finite size 'FiniteQueue'.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Queue where

import ForSyDe.Shallow.AbsentExt 

-- | A queue is modeled as a list. The data type 'Queue' modelles an queue of infinite size.
data Queue a        =  Q [a] deriving (Eq, Show)

-- | The data type 'FiniteQueue' has an additional parameter, that determines the size of the queue.
data FiniteQueue a  =  FQ Int [a] deriving (Eq, Show)

{-
Table \ref{tab:QueueFunctions} shows the functions an the data types \haskell{Queue} and \haskell{FiniteQueue}.
%
\begin{table}
\label{tab:QueueFunctions}
\begin{tabular}{lll}
\hline
infinite & finite & description \\
\hline
\hline
\haskell{pushQ} & \haskell{pushFQ} & pushes one element on the queue \\
\haskell{pushListQ} & \haskell{pushListFQ} & pushes a list of elements on the queue \\
\haskell{popQ} & \haskell{popFQ} & pops one element from the queue \\
\haskell{queue} & \haskell{finiteQueue} & transforms a list into a queue \\
\hline
\end{tabular}
\caption{Functions on the data types \haskell{Queue} and \haskell{FiniteQueue}}
\end{table}
-}

-- | 'pushQ' pushes one element into an infinite queue.
pushQ                         :: Queue a -> a -> Queue a

-- | 'pushListQ' pushes a list of elements into an infinite queue.
pushListQ                     :: Queue a -> [a] -> Queue a

-- | 'popQ' pops one element from an infinite queue.
popQ                          :: Queue a -> (Queue a, AbstExt a)

-- | 'queue' transforms a list into an infinite queue.
queue                         :: [a] -> Queue a

-- | 'pushFQ' pushes one element into a finite queue.
pushFQ                        :: FiniteQueue a -> a -> FiniteQueue a

-- | 'pushListFQ' pushes a list of elements into a finite queue.
pushListFQ                    :: FiniteQueue a -> [a] -> FiniteQueue a

-- | 'popFQ' pops one element from a finite queue.
popFQ                         :: FiniteQueue a 
			      -> (FiniteQueue a, AbstExt a)

-- | 'finiteQueue' transforms a list into an infinite queue.
finiteQueue		      :: Int -> [a] -> FiniteQueue a


-- Implementation

pushQ (Q q) x                 =  Q (q ++ [x])

pushListQ (Q q) xs            =  Q (q ++ xs)

popQ (Q [])                   =  (Q [], Abst)
popQ (Q (x:xs))               =  (Q xs, Prst x)

queue xs                      =  Q xs

pushFQ (FQ n q) x             =  if length q < n then
                                    (FQ n (q ++ [x]))
                                 else 
                                    (FQ n q)

pushListFQ (FQ n q) xs        =  FQ n (take n (q ++ xs))

popFQ (FQ n [])               =  (FQ n [], Abst)
popFQ (FQ n (q:qs))           =  (FQ n qs, Prst q)
                                             
finiteQueue n xs	      =  FQ n (take n xs)





