-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Config
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
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
--     in the future there will be a way to avoid hardcoding this value.
--     See: http://hackage.haskell.org/trac/ghc/ticket/2364
maxTupleSize = 62
