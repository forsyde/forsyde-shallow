{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings

{-|

Module      :  ForSyDe.Shallow
Copyright   :  (c) SAM/KTH 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  forsyde@kth.se
Stability   :  experimental
Portability :  portable 

The ForSyDe is a collection of libraries and modules.
It consistes of three main libraries:

* "CoreLib" contains the basic definitions and functions such as 
            events and signals.

* "MoCLib" defines the models of computations included in ForSyDe.

* "UtilityLib" provides several additional modules that are usefule
               and convenient in practice. Their status is typically 
               experimental.

And then there are a number of modules to be worked on and taskes to be 
address:

* "ForSyDeToDo"

-}

-- #ignore-exports


module ForSyDe.Shallow(module ForSyDe.Shallow.CoreLib
              , module ForSyDe.Shallow.MoCLib
              , module ForSyDe.Shallow.UtilityLib
	      ) where

import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.MoCLib
import ForSyDe.Shallow.UtilityLib
