-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrapper module exporting all the backends
-- 
-----------------------------------------------------------------------------
module ForSyDe.Backend 
 (module ForSyDe.Backend.Simulate,
  module ForSyDe.Backend.VHDL,
  module ForSyDe.Backend.GraphML) where

import ForSyDe.Backend.Simulate
import ForSyDe.Backend.VHDL
import ForSyDe.Backend.GraphML



