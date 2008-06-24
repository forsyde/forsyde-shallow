-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Config
-- Copyright   :  (c) The ForSyDe team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Configuration values of ForSyDe
--
-----------------------------------------------------------------------------
module ForSyDe.Config (maxTupleSize, module Paths_ForSyDe) where

import Paths_ForSyDe 

-- | Maximum tuple size
maxTupleSize :: Int
-- 62: this is the GHC-hardcoded value as of version 6.8.2
maxTupleSize = 62