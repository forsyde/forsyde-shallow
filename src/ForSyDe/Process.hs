-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Publicly usable functions to create primitive processes. (Reexports 
--  "ForSyDe.Process.SynchProc")
-- 
-----------------------------------------------------------------------------
module ForSyDe.Process 
 (ProcFun, newProcFun, defArgVal, defArgPF,
  ProcType,
  module ForSyDe.Process.SynchProc) where

import ForSyDe.Process.ProcFun (ProcFun, newProcFun, defArgVal, defArgPF)

import ForSyDe.Process.SynchProc 
import ForSyDe.Process.ProcType (ProcType)
import ForSyDe.Process.ProcType.Instances ()
