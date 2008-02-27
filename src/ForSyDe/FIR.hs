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

import ForSyDe.Signal
import ForSyDe.Process
import ForSyDe.Vector

import Data.Typeable
import Language.Haskell.TH.Syntax

-- | All kinds of FIR-filters can now be modeled by means of 'fir'. The only argument needed is the list of coefficients, which is given as a vector of any size. To illustrate this, an 8-th order band pass filter is modeled as follows. 
--
-- > bp = fir (vector [0.06318761339784, 0.08131651217682, 0.09562326700432, 
-- >                   0.10478344432968, 0.10793629404886, 0.10478344432968, 
-- >                   0.09562326700432, 0.08131651217682, 0.06318761339784 ])
-- 

fir :: (Fractional b, Lift b, Typeable b) => Vector b -> Signal b -> Signal b
fir h = innerProd h . sipo k 0.0
    where k = lengthV h

sipo :: (Fractional a, Lift a, Typeable a) =>
        Int -> a -> Signal a -> Vector (Signal a)
sipo n s0 = unzipxSY n . scanldSY "siposcanldSY" srV initState
    where initState = copyV n s0
          srV = $(newProcFun [d| srV :: Vector a -> a -> Vector a
                                 srV v a = shiftrV v a |])

innerProd :: (Fractional a, Lift a, Typeable a) =>
             Vector a -> Vector (Signal a) -> Signal a
innerProd h = zipWithxSY "innerProd" (ipV `defArgVal` h)
   where ipV = $(newProcFun 
                  [d| ipV :: Num a => Vector a -> Vector a -> a
                      ipV h x 
                         | nullV h || nullV x  = 0
	                 | otherwise = 
                              (headV h)*(headV x) + ipV (tailV h) (tailV x) |])



