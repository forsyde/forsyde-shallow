-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides publicly usable functions to build a system definition
-- and instantiate it.
-- 
-----------------------------------------------------------------------------
module ForSyDe.System  
(module ForSyDe.System.SysDef,
 module ForSyDe.System.SysFun,
 module ForSyDe.System.Instantiate)
where

import ForSyDe.System.SysDef (SysDef, newSysDef, newSysDefTH, newSysDefTHName)
import ForSyDe.System.SysFun (SysFun)
import ForSyDe.System.SysFun.Instances ()
import ForSyDe.System.Instantiate (instantiate)
