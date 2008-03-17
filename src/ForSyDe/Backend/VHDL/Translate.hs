-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Traverse
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions to translate elements of the intermediate system 
-- representation to elements of the VHDL AST.
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Translate where

import ForSyDe.Backend.VHDL.AST
import qualified ForSyDe.Backend.VHDL.AST as VHDL
import ForSyDe.Backend.VHDL.Constants

import ForSyDe.Ids
import ForSyDe.Signal
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysDef
import qualified Language.Haskell.TH as TH

import Data.Typeable
import Data.Char (toLower)

-- | Translate a System Definition to an Entity, explicitly returning
--   the VHDL identifiers of its output signals.
transSysDef2Ent :: SysDefVal -> EProne EntityDec
transSysDef2Ent sysDefVal = do
 -- FIXME: check if the entity name and its signals are in the same namespace
 -- then we could use mkVHDLId instead.
 entId <- transSysId2VHDL (sid sysDefVal)
 inDecs  <- mapM (transPort2SigDec In)  (iIface sysDefVal) 
 outDecs <- mapM (transPort2SigDec Out) (oIface sysDefVal)
 return $ EntityDec entId (inDecs ++ outDecs) 
 

-- | Translate a Port to a VHDL Interface signal declaration
transPort2SigDec :: Mode -> (PortId, TypeRep) -> EProne IfaceSigDec
transPort2SigDec m (pid, trep) = do           
 sid <- transPortId2VHDL pid
 tm  <- transSignalTR2TM trep
 return $ IfaceSigDec sid m tm

-- | Translate a system identifier to a VHDL identifier
transSysId2VHDL :: SysId -> EProne VHDLId
transSysId2VHDL = transPortId2VHDL

-- | Translate a process identifier to a VHDL identifier
transProcId2VHDL :: ProcId -> EProne VHDLId
transProcId2VHDL = transPortId2VHDL

-- | translate a port identifier to a VHDL Identifier
transPortId2VHDL :: PortId -> EProne VHDLId
transPortId2VHDL str = do let strL = map toLower str
                          when (elem strL reservedStrs)
                               (throwError (ReservedId str)) 
                          mkVHDLId str
                 

-- | translate a signal 'TypeRep' to a VHDL 'TypeMark'
transSignalTR2TM :: TypeRep -> EProne TypeMark
transSignalTR2TM rep
 | rep == intSignalTR  = return std_logicTM
 | rep == boolSignalTR = return int32TM
 | otherwise = throwError (UnsupportedType rep)
 where intSignalTR  = typeOf (undefined :: Signal Int)
       boolSignalTR = typeOf (undefined :: Signal Bool) 

-- | Translate a Haskell expression to a VHDl expression
transExp2VHDL :: TH.Exp -> EProne VHDL.Expr
transExp2VHDL = undefined