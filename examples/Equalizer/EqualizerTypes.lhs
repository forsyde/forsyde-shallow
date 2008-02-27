\subsection{Overview}

This module is a collection of data types that are used in the equalizer model.

\begin{code}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module EqualizerTypes where

import Data.Typeable
import Language.Haskell.TH.Lift
import Data.Complex

data AnalyzerMsg = Pass
 		 | Fail deriving(Show, Read, Eq, Typeable)
$(deriveLift1 ''AnalyzerMsg)

data OverrideMsg = Lock
                 | CutBass
	         | Release deriving(Show, Read, Eq, Typeable)
$(deriveLift1 ''OverrideMsg)

data Sensor = Active deriving(Show, Read, Eq, Typeable)
$(deriveLift1 ''Sensor)

data Button = BassDn
	    | BassUp
	    | TrebleDn
	    | TrebleUp deriving (Show, Read, Eq, Typeable)
$(deriveLift1 ''Button)

$(deriveLift1 ''Complex)

\end{code}
