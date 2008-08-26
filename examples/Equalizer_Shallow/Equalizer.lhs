\subsection{Overview}

The main task of the equalizer system is to adjust the audio signal according to the \process{Button Control}, that works as a user interface. In addition, the bass level must not
exceed a predefined threshold to avoid damage to the speakers.

This specification can be naturally decomposed into four functions shown in
Figure \ref{fig:Equalizer-Level1}. The subsystems \process{Button Control} and \process{Distortion Control}, are control dominated (grey shaded), while the \process{Audio Filter} and the \process{Audio Analyzer} are data flow dominated subsystems.  

\begin{figure}[h]
\centering
\input{Figures/Equalizer-Level1.pstex_t}
\caption{Subsystems of the \process{Equalizer}}
\label{fig:Equalizer-Level1}
\end{figure}

The \process{Button Control} subsystem monitors the button inputs and the override
signal from the subsystem \process{Distortion Control} and adjusts the current
bass and treble levels. This information is passed to the subsystem
\process{Audio Filter}, which receives the audio input, and filters and
amplifies the audio signal according to the current bass and treble
levels. This signal, the output signal of the equalizer, is analyzed by the 
\process{Audio Analyzer} subsystem, which determines, whether the bass exceeds a
predefined threshold. The result of this analysis is passed to the subsystem \process{Distortion Control}, which decides, if a minor or major violation is encountered and issues the
necessary commands to the \process{Button Control} subsystem.  

The frequency characteristics of the \process{Equalizer} is adjusted by the coefficients for the three FIR-filters in the \process{AudioFilter}. 
%
\begin{code}
module Equalizer(equalizer) where

import ForSyDe.Shallow

import ButtonControl
import DistortionControl
import AudioAnalyzer
import AudioFilter
\end{code}
%
The structure of the equalizer is expressed as a network of blocks:
%
\begin{code}
equalizer lpCoeff bpCoeff hpCoeff dftPts 
	  bassUp bassDn trebleUp trebleDn input = (bass, treble) --output 
  where
    (bass, treble)  = buttonControl overrides bassUp bassDn 
	                            trebleUp trebleDn
    output          = audioFilter lpCoeff bpCoeff hpCoeff bass 
				  treble input
    distFlag        = audioAnalyzer dftPts output
    overrides       = distortionControl delayedDistFlag
    delayedDistFlag = delaySY Abst distFlag
\end{code}

Since the equalizer contains a feedback loop, the signal \process{DistFlag} is delayed one event cycle using the initial value \Abst.

