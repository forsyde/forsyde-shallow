The block \process{Distortion Control} is directly developed from the SDL-specification, that has been used for the MASCOT-model \cite{BjJa2000}. The specification is shown in Figure \ref{fig:SDL-Distortion Control}.

\begin{figure}[h]
\centering
\includegraphics[scale=0.5]{Figures/DistortionControl.eps}
\caption{SDL-description of \emph{Distortion Control}}
\label{fig:SDL-Distortion Control}
\end{figure}

The \process{Distortion Control} is a single FSM, which is modeled by means of the skeleton \process{mealySY}. The global state is not only expressed by the explicit states - \constant{Passed}, \constant{Failed} and \constant{Locked} -, but also by means of the variable \variable{cnt}. The state machine has two possible input values, \constant{Pass} and \constant{Fail}, and three output values, \constant{Lock}, \constant{Release} and \constant{CutBass}. 

The \process{mealySY} creates a process that can be interpreted as a Mealy-machine. It takes two functions, \function{nxtSt} to calculate the next state and \function{out} to calculate the output. The state is represented by a pair of the explicit state and the variable \variable{cnt}. The initial state is the same as in the SDL-model, given by the tuple \constant{(Passed, 0)}. The \function{nxtSt} function uses pattern matching. Whenever an input value matches a pattern of the \function{nxtSt} function the corresponding right hand side is evaluated, giving the next state. An event with an absent value leaves the state unchanged. The output function is modeled in a similar way. The output is absent, when no output message is indicated in the SDL-model.
%
\begin{code}
module DistortionControl (distortionControl) where

import ForSyDe.Shallow
import EqualizerTypes

data State =  Passed
            | Failed
            | Locked

distortionControl ::  Signal (AbstExt AnalyzerMsg) 
		     -> Signal (AbstExt OverrideMsg)

distortionControl distortion 
   = mealySY nxtSt out (Passed, 0) distortion

lim = 3

--     State       Input       NextState       
nxtSt (state, cnt) (Abst)      = (state,cnt)
nxtSt (Passed,cnt) (Prst Pass) = (Passed,cnt)
nxtSt (Passed,_  ) (Prst Fail) = (Failed,lim)
nxtSt (Failed,cnt) (Prst Pass) = (Locked,cnt)
nxtSt (Failed,cnt) (Prst Fail) = (Failed,cnt)
nxtSt (Locked,_  ) (Prst Fail) = (Failed,lim)
nxtSt (Locked,cnt) (Prst Pass) = (newSt,newCnt)
   where newSt = if (newCnt == 0) then Passed
	                          else Locked
         newCnt = cnt - 1

--   State         Input       Output
out (Passed,_)   (Prst Pass) = Abst
out (Passed,_)   (Prst Fail) = Prst Lock
out (Failed,_)   (Prst Pass) = Abst
out (Failed,_)   (Prst Fail) = Prst CutBass
out (Locked,_)   (Prst Fail) = Abst 
out (Locked,cnt) (Prst Pass) = 
   if (cnt == 1) then Prst Release
		  else Abst
out _             Abst        = Abst
\end{code}







