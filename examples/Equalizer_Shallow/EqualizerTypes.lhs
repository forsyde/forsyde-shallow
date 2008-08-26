\subsection{Overview}

This module is a collection of data types that are used in the equalizer model.

\begin{code}
module EqualizerTypes where

data AnalyzerMsg = Pass
 		 | Fail deriving(Show, Read, Eq)

data OverrideMsg = Lock
                 | CutBass
	         | Release deriving(Show, Read, Eq)

data Sensor = Active deriving(Show, Read, Eq)

data Button = BassDn
	    | BassUp
	    | TrebleDn
	    | TrebleUp deriving (Show, Read, Eq)
\end{code}
