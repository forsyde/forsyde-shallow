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
module AudioAnalyzer (audioAnalyzer) where

import ForSyDe.Shallow
import Data.Complex
import EqualizerTypes

input = 0.1 :- 0.2 :- input

limit :: Double
limit = 1.0

nLow :: Int
nLow = 3

audioAnalyzer :: Int -> Signal Double -> Signal (AbstExt AnalyzerMsg)
audioAnalyzer pts =  mapSY (psi checkBass) -- Check Bass 
		   . mapSY (psi spectrum)  -- Spectrum
		   . mapSY (psi (dft pts)) -- DFT
		   . groupSY pts	   -- Group Samples 
		   . mapSY toComplex

spectrum :: (RealFloat a) => Vector (Complex a) -> Vector a
spectrum =  mapV log10 . selectLow nLow . mapV power . selectHalf . dropV 1
  where
    log10 x        = log x / log 10
    selectLow n xs = takeV n xs
    selectHalf xs  = takeV half xs
	       where half = floor (fromIntegral (lengthV xs) / 2)  
    power x = (magnitude x) ^ 2

checkBass :: Vector Double -> AnalyzerMsg
checkBass = checkLimit limit . sumV
  where
    checkLimit limit x | x > limit  = Fail
	               | otherwise  = Pass
    sumV vs = foldlV (+) 0.0 vs


toComplex x = x :+ 0
\end{code}
