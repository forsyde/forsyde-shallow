\begin{code}
module TestDistortionControl where

import DistortionControl
import EqualizerTypes
import ForSyDe.Shallow

flag = signal
       [Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Fail,
	Abst, Abst, Abst, Prst Fail,
	Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
	Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass,
	Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Pass,
	Abst, Abst, Abst, Prst Fail,
        Abst, Abst, Abst, Prst Pass,
        Abst, Abst, Abst, Prst Pass]        

testDistortionControl = distortionControl flag
\end{code}
