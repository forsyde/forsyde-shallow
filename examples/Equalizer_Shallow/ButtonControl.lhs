\subsection{Overview}

The subsystem \process{Button Control} works as a user interface in the equalizer system. It receives the four input signals \signal{BassDn}, \signal{BassUp}, \signal{TrebleDn}, \signal{TrebleUp} and the override signal \process{Override} from the \process{Distortion Control} and calculates the new bass and treble values for the output signals \signal{Bass} and \signal{Treble}. The subsytem contains the main processes \process{Button Interface} and \process{Level Control}. The process \process{Level Control} outputs a new value, if either the signal \signal{Button} or the signal \signal{Overr} is present, otherwise the output value is absent. The process \process{Hold Level} is modeled by means of \process{holdSY (0.0, 0.0)} that outputs the last present value, if the input value is absent. The process \process{unzipSY} transforms a signal of tuples (the current bass and treble level) into a tuple of signals (a bass and a treble signal).

\begin{figure}
\centering
\scalebox{0.8}{\mbox{\input{Figures/ButtonControl.pstex_t}}}
\caption{The Subsystem \process{Button Control}}
\end{figure}

\begin{code}
module ButtonControl (buttonControl) where 

import ForSyDe.Shallow
import EqualizerTypes
--import Combinators

data State   = Operating 
	     | Locked deriving(Eq, Show)
type Level   = Double
type Bass    = Level
type Treble  = Level

buttonControl :: Signal (AbstExt OverrideMsg) -> Signal (AbstExt Sensor) 
 	      -> Signal (AbstExt Sensor) -> Signal (AbstExt Sensor) 
 	      -> Signal (AbstExt Sensor) -> (Signal Bass,Signal Treble)
buttonControl overrides bassDn bassUp trebleDn trebleUp 
    = (bass, treble) 
      where (bass, treble) = unzipSY levels
            levels = holdSY (0.0, 0.0) $ levelControl button overrides 
 	    button = buttonInterface bassDn bassUp trebleDn trebleUp
\end{code}

\subsection{The Process \process{Button Interface}}

The \process{Button Interface} monitors the four input buttons \signal{BassDn}, \signal{BassUp}, \signal{TrebleDn}, \signal{TrebleUp} and indicates if a button is pressed. If two or more buttons are pressed the conflict is resolved by the priority order of the buttons. 

\begin{code}
buttonInterface :: Signal (AbstExt Sensor) -> Signal (AbstExt Sensor) 
		-> Signal (AbstExt Sensor) -> Signal (AbstExt Sensor) 
		-> Signal (AbstExt Button)
buttonInterface bassUp bassDn trebleUp trebleDn 
   = zipWith4SY f bassUp bassDn trebleUp trebleDn
     where f (Prst Active) _ _ _ = Prst BassUp
	   f _ (Prst Active) _ _ = Prst BassDn
	   f _ _ (Prst Active) _ = Prst TrebleUp
	   f _ _ _ (Prst Active) = Prst TrebleDn
	   f _ _ _ _		 = Abst
\end{code}

\subsection{The Process \process{Level Control}}

The process has a local state that consists of a mode and the current values for the bass and treble levels (Figure \ref{fig:FSM_LevelControl}). The \process{Level Control} has two modes, in the mode \constant{Operating} the bass and treble values are stepwise changed in 0.2 steps. However, there exists maximum and minimum values which are -5.0 and +5.0. The process enters the mode \constant{Locked} when the \constant{Override} input has the value \constant{Lock}. In this mode an additional increase of the bass level is prohibitet and even decreased by 1.0 in case the \constant{Override} signal has the value \constant{CutBass}. The subsystem returns to the \constant{Operating} mode on the override value \constant{Release}. The output of the process is an absent extended signal of tuples with the current bass and treble levels.   

\begin{figure}
\resizebox{\columnwidth}{!}{\mbox{\input{Figures/FSM_LevelControl.pstex_t}}}
\caption{The State Diagram of the Process \process{Level Control}}
\label{fig:FSM_LevelControl}
\end{figure}
\begin{code}
levelControl :: Signal (AbstExt Button) -> Signal (AbstExt OverrideMsg) 
 	     -> Signal (AbstExt (Bass,Treble))
levelControl button overrides 
  = mealy2SY nextState output (initState, initLevel) button overrides

nextState :: (State,(Double,Double)) -> AbstExt Button 
	  -> AbstExt OverrideMsg -> (State,(Double,Double))
nextState (state, (bass, treble)) button override
  = (newState, (newBass, newTreble)) where
     newState = if state == Operating then
		   if override == Prst Lock then
		      Locked
		   else
		      Operating
	        else
		   if override == Prst Release then
		      Operating
		   else 
		      Locked

     newBass = if state == Locked then
		  if override == Prst CutBass then
		     decreaseLevel bass cutStep
		  else
	             if button == Prst BassDn then
			decreaseLevel bass step
		     else
			bass
	       else -- state = Operating
		  if button == Prst BassDn then
		     decreaseLevel bass step
		  else 
		     if button == Prst BassUp then
			increaseLevel bass step
		     else
			bass
		    
     newTreble = if button == Prst TrebleDn then
		    decreaseLevel treble step
		 else 
		    if button == Prst TrebleUp then
		       increaseLevel treble step
		    else 
		       treble

output :: (a, (Bass, Treble)) -> AbstExt Button -> AbstExt OverrideMsg 
 	  -> AbstExt (Bass,Treble)
output _           Abst Abst = Abst
output (_, levels) _    _    = Prst levels	          
\end{code}

The process uses the following initial values.

\begin{code}
initState =  Operating
initLevel =  (0.0, 0.0)
maxLevel  =  5.0
minLevel  = -5.0
step      =  0.2
cutStep   =  1.0
\end{code}

The process uses the following auxiliary functions.

\begin{code}
decreaseLevel :: Level -> Level -> Level
decreaseLevel level step = if reducedLevel >= minLevel then
			      reducedLevel
		           else
			      minLevel
			   where reducedLevel = level - step

increaseLevel :: Level -> Level -> Level
increaseLevel level step = if increasedLevel <= maxLevel then
			      increasedLevel
		           else
			      maxLevel
			   where increasedLevel = level + step
\end{code}
