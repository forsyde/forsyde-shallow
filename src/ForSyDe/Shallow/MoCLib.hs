{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

{-|

Module      :  ForSyDe
Copyright   :  (c) SAM/KTH 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  forsyde@kth.se
Stability   :  experimental
Portability :  portable 

The ForSyDeMoCLib is a container including all MoC libraries and their domain interfaces. It consists of the following libraries:

* The library for the synchroonous MoC: "SynchronousLib"
 
* The library for the untimed MoC: "UntimedLib"
 
* The library for the continuous time MoC: "CTLib"

* The library for the discrete event MoC:

* The library for the domain interfaces:
-}

-- #ignore-exports

module ForSyDe.Shallow.MoCLib(  
	              module ForSyDe.Shallow.SynchronousLib
                    , module ForSyDe.Shallow.CTLib
		    , module ForSyDe.Shallow.UntimedLib
		    ) where

import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.CTLib
import ForSyDe.Shallow.UntimedLib

