\subsection{Overview}

The \process{Audio Analyzer} analyzes the current bass level and raises a flag when the bass level exceeds a limit. 

\begin{figure}
\centering
\scalebox{0.8}{\mbox{\input{Figures/AudioAnalyzer.pstex_t}}}
\caption{The \textit{Audio Analyzer} subsystem}
\label{fig:AudioAnalyzerSubsystem}
\end{figure}

As illutsrated in Figure \ref{fig:AudioAnalyzerSubsystem} the \process{Audio Analyzer} is divided into four blocks. The input signal is first grouped into samples of size $N$ in the process \process{Group Samples} and then processed with a \process{DFT} in order to get the frequency spectrum of the signal. Then the power spectrum is calculated in \process{Spectrum}. In \process{CheckBass} the lowest frequencies are compared with a threshold value. If they exceed this value, the output \process{Distortion Flag} will have the value \constant{Fail}.

Since \process{Group Samples} needs $N$ cycles for the grouping, it produces $N-1$ absent values $\perp$ for each grouped sample. Thus the following processes \process{DFT}, \process{Spectrum} and \process{Check Bass} are all $\Psi$-extended in order to be able to process the absent value $\Abst$.
\begin{code}
{-# LANGUAGE TemplateHaskell, PatternGuards #-}
-- The PatternGuards are used to hush innapropiate compiler warnings
-- see http://hackage.haskell.org/trac/ghc/ticket/2017
module AudioAnalyzer (audioAnalyzer) where

import ForSyDe
import Data.Complex
import EqualizerTypes



limit = 1.0
nLow = 3

audioAnalyzer :: Integer -> Signal Double -> Signal (AbstExt AnalyzerMsg)
audioAnalyzer pts =  mapSY "checkBass" (psiPF `defArgPF` checkBass) 
		   . mapSY "spectrum" (psiPF `defArgPF` spectrum)  
		   . mapSY "dft"  (psiPF `defArgPF` (dftPF `defArgVal` pts)) 
		   . groupSY "groupSamples" pts 
		   . mapSY "toComplex" toComplex
 where psiPF = $(newProcFun
                   [d| psiPF :: (a -> b) -> AbstExt a -> AbstExt b
                       psiPF a b = psi a b|]) 
       dftPF = $(newProcFun
                   [d|  dftPF :: Integer -> Vector (Complex Double) -> 
                                 Vector (Complex Double)
                        dftPF i v = dft i v |])

spectrum :: ProcFun (Vector (Complex Double) -> Vector Double)
spectrum =  
 $(newProcFun 
   [d| spectrum :: Vector (Complex Double) -> Vector Double
       spectrum v = (mapV log10 . selectLow nLow . mapV power . selectHalf . 
                     dropV 1) v
         where
            log10 x | x < 0.00001 = -5
                    | otherwise   = log x / log 10
            selectLow n xs = takeV n xs
            selectHalf xs  = takeV half xs
	       where half = floor ((lengthV xs) / 2)  
            power x = (magnitude x) ^ 2 |])


checkBass :: ProcFun (Vector Double -> AnalyzerMsg)
checkBass = 
 $(newProcFun
   [d| checkBass :: Vector Double -> AnalyzerMsg
       checkBass v = (checkLimit limit . sumV) v
        where
         checkLimit limit x | x > limit  = Fail
                            | otherwise  = Pass
         sumV vs = foldlV (+) 0.0 vs |])

toComplex :: ProcFun (Double -> Complex Double)
toComplex = 
 $(newProcFun
   [d| toComplex :: Double -> Complex Double
       toComplex x = x :+ 0 |])

\end{code}
