-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
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



