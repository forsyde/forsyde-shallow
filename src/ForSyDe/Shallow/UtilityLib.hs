{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

{-|

Module      :  ForSyDe
Copyright   :  (c) SAM/KTH 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  forsyde@kth.se
Stability   :  experimental
Portability :  portable 

The ForSyDeUtilityLib is a container including all libraries that are related to the ForSyDe project and either extend the ForSyDe MoC libraries or add additional functionality to ForSyDe.

* "DFT"

* "Memory"

* "Queue"

* "BitVector"

* "FilterLib"

* "Gaussian"		

* "PolyArith"		

* "StochasticLib"
-}

-- #ignore-exports

module ForSyDe.Shallow.UtilityLib(  
                      module ForSyDe.Shallow.DFT,            
                      module ForSyDe.Shallow.Memory,
                      module ForSyDe.Shallow.Queue
	              --module ForSyDe.Shallow.BitVector,
                      --module ForSyDe.Shallow.FilterLib,
		      --module ForSyDe.Shallow.Gaussian,
		      --module ForSyDe.Shallow.PolyArith,
		      --module ForSyDe.Shallow.StochasticLib
		    ) where

import ForSyDe.Shallow.DFT          
import ForSyDe.Shallow.Memory
import ForSyDe.Shallow.Queue
--import ForSyDe.Shallow.BitVector
--import ForSyDe.Shallow.FilterLib
--import ForSyDe.Shallow.Gaussian
--import ForSyDe.Shallow.PolyArith
--import ForSyDe.Shallow.StochasticLib


