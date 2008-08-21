{-|

Module      :  ForSyDe
Copyright   :  (c) SAM/KTH 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  forsyde@kth.se
Stability   :  experimental
Portability :  portable 

The CoreLib is the base for all MoC libraries and is a container that includes the following libraries: 

* "Signal"

* "Vector"
 
* "AbsentExt" 
-}

-- #ignore-exports

module ForSyDe.Shallow.CoreLib(  
			module ForSyDe.Shallow.Signal,
			module ForSyDe.Shallow.Vector,
			module ForSyDe.Shallow.AbsentExt
		    ) where

import ForSyDe.Shallow.Vector
import ForSyDe.Shallow.Signal
import ForSyDe.Shallow.AbsentExt








