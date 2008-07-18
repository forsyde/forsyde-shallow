{-# LANGUAGE TemplateHaskell, RelaxedPolyRec, PatternGuards #-}
-- The PatternGuards are used to hush innapropiate compiler warnings
-- see http://hackage.haskell.org/trac/ghc/ticket/2017
-- FIXME: Add header
{-
A FIR-filter is described by the following equation, which is illustrated in Figure \ref{fig:FIRFilter}:

\begin{equation} 
  y_n = \sum_{m=0}^k x_{n-m} h_m
\end{equation}

\begin{figure}[h]
\centering
\input{fig/FIR.pstex_t}
\caption{FIR-filter}
\label{fig:FIRFilter}
\end{figure}

The state of the FIR-Filter can be seen as a shift register with parallel output. The first element in the shift register at cycle $n$ is $x_n$ and the last element is $x_{n-k}$. In the next cycle a new value $x_{n+1}$ is shifted into the register from the left, all other elements are shifted one place to the right, and the value $x_{n-k}$ is discarded. We model the shift register with the process $\mathit{shiftreg}_k$. The process is based on the process constructor \process{scanlSY} which which takes the shift function \texttt{shiftrV} as first argument and an initial vector of size $k+1$ with zeroes as initial values. 

The output of the shiftregister, a signal of vectors, is transformed with the process \process{unzipxSY} into a vector of signals. Then the process \process{innerProd} calculates the inner product of the coefficient vector $h$ and the output of the shift register. The process is implemented by the process constructor \process{zipWithSY} that takes a parametized function \function{ipV(h)} as arguments.

\begin{figure}[h]
\centering
\input{fig/FIR_SpecificationModel.pstex_t}
\caption{FIR-filter model}
\label{fig:FIR-filter model}
\end{figure}

\begin{code}
-}
-- | The module implements a FIR-filter for the synchronous computational model.
module ForSyDe.FIR (fir) where

import ForSyDe.Ids
import ForSyDe.Signal
import ForSyDe.Process

import Data.TypeLevel.Num (Nat, Pos)
import Data.Param.FSVec hiding ((++))
import qualified Data.Param.FSVec as V
import Data.Typeable

-- | All kinds of FIR-filters can now be modeled by means of 'fir'. The only argument needed is the list of coefficients, which is given as a vector of any size. To illustrate this, an 8-th order band pass filter is modeled as follows. 
--
-- > bp = fir (vector [0.06318761339784, 0.08131651217682, 0.09562326700432, 
-- >                   0.10478344432968, 0.10793629404886, 0.10478344432968, 
-- >                   0.09562326700432, 0.08131651217682, 0.06318761339784 ])
-- 

fir :: (Fractional b, ProcType b, Pos s, Typeable s) => 
       ProcId -> FSVec s b -> Signal b -> Signal b
fir id h = innerProd (id ++ "_innerProd") h . sipo (id ++ "_sipo") k 0.0
    where k = V.lengthT h

sipo :: (Pos s, Typeable s, Fractional a, ProcType a) =>
        ProcId -> s -> a -> Signal a -> FSVec s (Signal a)
sipo id n s0 = unzipxSY (id ++ "_unzipxSY") . scanldSY (id ++ "_scanldSY") srV initState
    where initState = V.copy n s0
          srV = $(newProcFun [d| srV :: Pos s => FSVec s a -> a -> FSVec s a
                                 srV v a = V.shiftr v a |])

innerProd :: (Fractional a, ProcType a, Nat s, Typeable s) =>
             ProcId -> FSVec s a -> FSVec s (Signal a) -> Signal a
innerProd id h = zipWithxSY id (ipV `defArgVal` h)
   where ipV = $(newProcFun 
                  -- We could make the inner product in one traverse 
                  -- but FSVecs don't allow recursive calls
                  -- (they don't allow to check the constraints statically)
                  -- Thus, we traverse the vector twice
                  [d| ipV :: (Nat s, Num a) => FSVec s a -> FSVec s a -> a
                      ipV v1 v2 = 
                          V.foldl (+) 0 $ V.zipWith (*) v1 v2 |])



