\subsection{Overview}
Figure \ref{fig:AudioFilter} shows the structure of the \process{AudioFilter}. The task of this subsystem is to amplify different frequencies of the audio signal independently according to the assigned levels. The audio signal is splitted into three identical signals, one for each frequency region. The signals are filtered and then amplified according to the assigned amplification level. As the equalizer in this design only has a bass and treble control, the middle frequencies are not amplified. The output signal from the \process{Audio Filter} is the addition of the three filtered and amplified signals.

\begin{figure}[h]
\centering
\input{Figures/AudioFilter.pstex_t}
\caption{Subsystems of the \emph{Audio Filter}}
\label{fig:AudioFilter}
\end{figure}

We model this structure as a network of blocks directly from Figure \ref{fig:AudioFilter}. It consists of three filters, two amplifiers and an adder. These blocks are modeled in the process layer. The \process{Audio Filter} has the filter coefficients for the low pass, band pass and high pass filter as parameters.
%
\begin{code}
module AudioFilter where

import ForSyDe.Shallow
import ForSyDe.Shallow.FIR

audioFilter :: Floating a => Vector a -> Vector a -> Vector a 
	    -> Signal a -> Signal a -> Signal a -> Signal a
audioFilter lpCoeff bpCoeff hpCoeff bass treble audioIn = audioOut
   where audioOut      = zipWith3SY add3 bassPath middlePath treblePath
	 bassPath      = ((amplify bass) . lowPass) audioIn
	 middlePath    = bandPass audioIn
	 treblePath    = ((amplify treble) . highPass) audioIn
	 lowPass       = firSY lpCoeff
	 bandPass      = firSY bpCoeff
	 highPass      = firSY hpCoeff
	 amplify       = zipWithSY scale
	 add3 x y z    = x + y + z
	 scale x y     = y * (base ** x)  
	 base	       = 1.1
\end{code}

