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
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module DistortionControl (distortionControl) where

import ForSyDe
import EqualizerTypes
import Language.Haskell.TH.Lift
import Data.Typeable

data State =  Passed
            | Failed
            | Locked
 deriving (Typeable, Eq)
$(deriveLift1 ''State)

distortionControl ::  Signal (AbstExt AnalyzerMsg) 
		     -> Signal (AbstExt OverrideMsg)

distortionControl distortion 
   = mealySY "distControl" nxtSt out (Passed, 0) distortion


--     State       Input       NextState
nxtSt :: ProcFun ((State, Int) -> AbstExt AnalyzerMsg -> (State, Int))       
nxtSt = 
 $(newProcFun 
       [d| nxtSt :: (State, Int) -> AbstExt AnalyzerMsg -> (State, Int)
           nxtSt state input = 
                   if isAbsent input then
                       state
                    else if (fst state == Passed) &&
                            (unsafeFromAbstExt input) == Pass then
                       (Passed, snd state)
                    else if (fst state == Passed) && 
                            (unsafeFromAbstExt input) == Fail then
                       (Failed, lim)
                    else if (fst state == Failed) &&
                            (unsafeFromAbstExt input) == Pass then
                       (Locked, snd state)
                    else if (fst state == Failed) &&
                            (unsafeFromAbstExt input) == Fail then
                       (Failed, snd state)
                    else -- nxtSt (Locked,_  ) (Prst Fail)
                       (newSt, newCnt)

               where lim = 3
                     newSt = if (newCnt == 0) then Passed
	                                      else Locked
                     newCnt = (snd state) - 1  |])

--   State         Input       Output
out :: ProcFun ((State, Int) -> AbstExt AnalyzerMsg -> AbstExt OverrideMsg)
out = $(newProcFun
 [d| out :: (State, Int) -> AbstExt AnalyzerMsg -> AbstExt OverrideMsg
     out state input = if isAbsent input then
                         Abst
                       else if (fst state == Passed) &&
                            (unsafeFromAbstExt input) == Pass then
                         Abst
                       else if (fst state == Passed) &&
                            (unsafeFromAbstExt input) == Fail then
                         Prst Lock
                       else if (fst state == Failed) &&
                            (unsafeFromAbstExt input) == Pass then
                         Abst
                       else if (fst state == Failed) &&
                            (unsafeFromAbstExt input) == Fail then
                         Prst CutBass
                       else if (fst state == Locked) &&
                            (unsafeFromAbstExt input) == Fail then
                         Abst
                       else -- (fst state == Locked) &&
                            -- (unsafeFromAbstExt input) == Pass then
                               if ((snd state) == 1) then Prst Release
		                                     else Abst |])
                       


\end{code}







