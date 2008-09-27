-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_devt@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Shallow-embedded implementation of ForSyDe (see 'ForSyDe.Shallow.Signal'). 
--
-- Unlike systems built using the deep-embedded Signal type of ForSyDe
-- (see 'ForSyDe.Signal'), systems built with 'ForSyDe.Shallow.Signal'
-- can make use of new and experimental features such as multiple,
-- heterogeneous MoCs (Models of Computation) other than the Synchronous
-- MoC (the only Model of Computation currently supported by deep-embdded
-- ForSyDe). However, as an important tradeoff, 'ForSyDe.Shallow.Signal'
-- is unaware of the resulting system structure, only allowing simulation 
-- (i.e. a VHDL or GraphML backend is impossible to implement).
-- 
-- The shallow implementation of ForSyDe consists of three main libraries:
-- 
-- * "CoreLib" contains the basic definitions and functions such as events and signals.
--
-- * "MoCLib" defines the models of computations included in ForSyDe.
--
-- * "UtilityLib" provides several additional modules that are usefule
--   and convenient in practice. Their status is typically 
--   experimental.
-----------------------------------------------------------------------------
module ForSyDe.Shallow(module ForSyDe.Shallow.CoreLib
              , module ForSyDe.Shallow.MoCLib
              , module ForSyDe.Shallow.UtilityLib
	      ) where

import ForSyDe.Shallow.CoreLib
import ForSyDe.Shallow.MoCLib
import ForSyDe.Shallow.UtilityLib
