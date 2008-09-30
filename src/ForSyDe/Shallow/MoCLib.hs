-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoCLib
-- Copyright   :  (c) SAM/KTH 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- The ForSyDeMoCLib is a container including all MoC libraries and
-- their domain interfaces. It consists of the following libraries:
--
-- * The library for the synchronous MoC: "SynchronousLib". In this
--   version the synchronous library is divided into three libraries:
--   SynchronousLib, SynchronousProcessLib and StochasticLib.
-- 
-- * The library for the untimed MoC: "ForSyDe.Shallow.UntimedLib"
--  
-- * The library for the continuous time MoC: "ForSyDe.Shallow.CTLib"
--
-- * The library for the domain interfaces: "ForSyDe.Shallow.DomainInterfaces"
-----------------------------------------------------------------------------
module ForSyDe.Shallow.MoCLib(  
	              module ForSyDe.Shallow.SynchronousLib,
                      module ForSyDe.Shallow.SynchronousProcessLib,
                      module ForSyDe.Shallow.StochasticLib,
                      module ForSyDe.Shallow.CTLib,
		      module ForSyDe.Shallow.UntimedLib,
                      module ForSyDe.Shallow.DomainInterfaces
		    ) where

import ForSyDe.Shallow.StochasticLib
import ForSyDe.Shallow.SynchronousLib
import ForSyDe.Shallow.CTLib
import ForSyDe.Shallow.UntimedLib
import ForSyDe.Shallow.DomainInterfaces
import ForSyDe.Shallow.SynchronousProcessLib
