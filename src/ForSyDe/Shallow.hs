-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Shallow-embedded implementation of ForSyDe (see "ForSyDe.Shallow.Signal"). 
--
-- Unlike systems built using the deep-embedded Signal type of ForSyDe
-- (see 'ForSyDe.Signal'), systems built with 'ForSyDe.Shallow.Signal'
-- can make use of new and experimental features such as multiple,
-- heterogeneous MoCs (Models of Computation) other than the Synchronous
-- MoC (the only Model of Computation currently supported by deep-embdded
-- ForSyDe). However, as an important tradeoff, 'ForSyDe.Shallow.Signal'
-- is unaware of the resulting system structure, only allowing simulation 
-- (i.e. a VHDL or GraphML backend is impossible to implement).
-----------------------------------------------------------------------------
module ForSyDe.Shallow(
  -- | This module contains the basic definitions and functions such
  -- as events and signals.
  module ForSyDe.Shallow.Core,

  -- | This module defines the models of computations included in
  -- ForSyDe.
  module ForSyDe.Shallow.MoC
  ) where

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.MoC
-- import ForSyDe.Shallow.Utility
