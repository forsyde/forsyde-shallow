-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Ids
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ForSyDe  identifier types
--
-----------------------------------------------------------------------------
module ForSyDe.Ids where

-- | A process identifier
--   FIXME: make it an ADT
type ProcId = String

-- | A Port identifier
-- FIXME: make it an ADT
type PortId = String

-- | A System identifier
-- FIXME: make it an ADT
type SysId = String