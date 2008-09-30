-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides publicly usable functions to build a system definition
-- and instantiate it.
-- 
-----------------------------------------------------------------------------
module ForSyDe.System  
(SysDef, newSysDef, newSysDefTH, newSysDefTHName,
 SysFun, SysFunToSimFun, SysFunToIOSimFun,
 instantiate)
where

import ForSyDe.System.SysDef (SysDef, newSysDef, newSysDefTH, newSysDefTHName)
import ForSyDe.System.SysFun (SysFun, SysFunToSimFun, SysFunToIOSimFun)
import ForSyDe.System.SysFun.Instances ()
import ForSyDe.System.Instantiate (instantiate)
