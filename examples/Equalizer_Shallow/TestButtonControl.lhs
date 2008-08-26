\begin{code}
module TestButtonControl where

import ButtonControl
import EqualizerTypes
import ForSyDe.Shallow




bassUp = signal [Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst,
		 Prst Active, Prst Active, Abst, Abst]

bassDn = signal [Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst,
		 Abst, Abst, Prst Active, Abst]

trebleUp = signal [Abst, Abst, Abst, Abst,
		 Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst, Abst,
		 Abst, Abst, Abst, Prst Active]

trebleDn = signal [Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst, Prst Active,
		 Abst, Abst, Abst,  Prst Active,
		 Abst, Abst, Abst, Prst Active]

overrides = signal [Abst, Abst, Abst, Abst,
		    Prst Lock, Abst, Abst, Abst,
		    Abst, Prst CutBass, Abst, Abst,
		    Prst Release,  Abst, Abst, Abst]

testButtonControl = buttonControl overrides bassUp bassDn trebleUp trebleDn
--testButtonInterface = buttonInterface  bassUp bassDn trebleUp trebleDn 
\end{code}