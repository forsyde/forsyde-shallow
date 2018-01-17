-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Shallow-embedded implementation of ForSyDe (see "ForSyDe.Shallow.Core.Signal"). 
--
-- Unlike systems built using the deep-embedded Signal type of ForSyDe
-- (see @forsyde-deep@), systems built with
-- 'ForSyDe.Shallow.Core.Signal' can make use of new and experimental
-- features such as multiple, heterogeneous MoCs (Models of
-- Computation) other than the Synchronous MoC (the only Model of
-- Computation currently supported by deep-embdded ForSyDe). However,
-- as an important tradeoff, 'ForSyDe.Shallow.Core.Signal' is unaware
-- of the resulting system structure, only allowing simulation (i.e. a
-- VHDL or GraphML backend is impossible to implement).
-----------------------------------------------------------------------------
module ForSyDe.Shallow(
  -- | The "ForSyDe.Shallow.Core" module contains the basic
  --   definitions and functions such as events and signals.
  module ForSyDe.Shallow.Core,

  -- | The "ForSyDe.Shallow.MoC" module defines the models of
  --   computations included in ForSyDe.
  module ForSyDe.Shallow.MoC,

  -- | The "ForSyDe.Shallow.Utility" module provides several
  --   additional modules that are useful and convenient in
  --   practice.
  module ForSyDe.Shallow.Utility
  ) where

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.MoC
import ForSyDe.Shallow.Utility
