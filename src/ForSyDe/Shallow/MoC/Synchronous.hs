{-# OPTIONS_HADDOCK not-home #-}
----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.Synchronous
-- Copyright   :  (c) SAM/KTH 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable 
-- 
-- The synchronuous library defines process constructors and processes
-- for the synchronous computational model. A process constructor is a
-- higher order function which together with combinational function(s)
-- and values as arguments constructs a process.
----------------------------------------------------------------------

module ForSyDe.Shallow.MoC.Synchronous (
  -- * The Core Library
  module ForSyDe.Shallow.MoC.Synchronous.Lib,

  -- * Add-ons
  -- ** Utility Processes
  -- | Collection of process constructors commonly used in designs.
  module ForSyDe.Shallow.MoC.Synchronous.Process,

  -- ** Stochastic Processes
  -- | Library of stochastic process constructors
  module ForSyDe.Shallow.MoC.Synchronous.Stochastic
  ) where

import ForSyDe.Shallow.MoC.Synchronous.Lib
import ForSyDe.Shallow.MoC.Synchronous.Process
import ForSyDe.Shallow.MoC.Synchronous.Stochastic
