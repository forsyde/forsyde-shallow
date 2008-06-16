{-# LANGUAGE TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.SysDef
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides the System Definition type ('SysDef') together with
-- a Template Haskell constructor to build it. 
--
-----------------------------------------------------------------------------
module ForSyDe.System.SysDef 
  (SysDef(..),
   PrimSysDef(..),
   SysDefVal(..), 
   newSysDef,
   newSysDefTH,
   newSysDefTHName,
   Iface) where

import ForSyDe.Ids
import ForSyDe.Signal
import {-# SOURCE #-} ForSyDe.Netlist 
import ForSyDe.OSharing
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysFun (checkSysFType, SysFun(..))

import Data.Maybe (isJust, fromJust)
import Control.Monad (when, replicateM)
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.LiftInstances ()





-- | Interface, describes the input or output ports of the system.
--   Each entry contains the name of the port and its type.
type Iface = [(PortId, TypeRep)]


-- | We add a phantom parameter to indicate the type of the system 
newtype SysDef a = SysDef {unSysDef :: PrimSysDef}

-- | The Primitive System Definition.
--   Instead of just providing the value, a reference is provided
--   to allow sharing between instances.
newtype PrimSysDef = PrimSysDef {unPrimSysDef :: URef SysDefVal}

-- | The System Definition value
data SysDefVal = SysDefVal 
     {sid     :: SysId,      -- ^ Identifier of the System 
      netlist :: Netlist [], -- ^ System netlist
      iIface  :: Iface,      -- ^ Input  interface
      oIface  :: Iface,      -- ^ Output interface 
      loc     :: Maybe Loc}  -- ^ Location of the call to newSysDef
                             --   which created this System definition
                             --   (used for later error reporting)
                             --   It will initialized or not depending
                             --   on the newSysDef* function used


-- FIXME FIXME FIXME: The frontend doesn't currently check for duplicates in 
--                    process identifiers. newSysDef* should take care of that.



-- | 'SysDef' constructor
--
--   Builds a system definition out of a system function describing the system 
--   and its port identifers.   
newSysDef :: SysFun f => f -- ^ system function 
                      -> SysId    -- ^ System identifier 
                      -> [PortId] -- ^ Input interface port identifiers 
                      -> [PortId] -- ^ Output interface port identifiers 
                      -> SysDef f
newSysDef f sysId inIds outIds = either currError id eProneResult
 where currError = uError "newSysDef"
       eProneResult = newSysDefEProne f Nothing sysId inIds outIds

-- |'SysDef' constructor, Template Haskell version
--  FIXME: currently broken, do not use.
--
--   Builds a system definition out of a system function, a system identifiers 
--   and its port identifers.
--
--  For example @$(newSysDefTH mySysFun "mysys" ["in1] ["out1"])@ creates a
--  system definition from system funcion @mySysFun@ which should have 
--  one input and output signals.
--
--  The advantage of 'newSysDefTH' over 'newSysDef' is that it 
--  reports errors (e.g duplicated port and process identifiers) earlier, 
--  at host-language (Haskell) compile-time. 
--
--  In addition, due to the use of Template Haskell, 'newSysDefTH' is
--  aware of the source location at which it was called, making
--  further error reports friendlier to the user.
newSysDefTH :: SysFun f => f -- ^ system function 
                        -> SysId    -- ^ System identifier 
                        -> [PortId] -- ^ Input interface port identifiers 
                        -> [PortId] -- ^ Output interface port identifiers 
                        -> ExpQ
newSysDefTH f sysId inIds outIds = 
 case eProneResult of
   Left err -> currError err
   -- unfortunately SysDef can't easily be an instance of Lift 
   -- due to the unsafe, unmutable references used in observable sharing 
   -- Right sysDef -> [| sysDef |]
   Right _ -> intError "newSysDefTH" (Other "Unimplemented")
-- FIXME: Fix this function or remove (updating the documentation of
--        newSysDef* in the latter case).
{-
   Right _ -> do
    loc <- currentModule
    [| let iIds = inIds
           oIds = outIds
           (nlist, inTypes, outTypes) = applySysFun f iIds
       in SysDef $ PrimSysDef $ newURef $ SysDefVal sysId
                                          (Netlist nlist)
                                          (zip iIds inTypes)
                                          (zip oIds outTypes)
                                          (Just loc) |]
-}
 where currError = qError "newSysDefTH"
       eProneResult = newSysDefEProne f Nothing sysId inIds outIds


-- | 'SysDef' constructor, Template Haskell 'Name' version
--
--   Builds a 'SysDef' out of the name of a system function
--   and its port identifers.
--
--   The system will later be identified by the basename 
--   (i.e. unqualified name) of the function.
--
--  For example @$(newSysDefTHName 'mySysFun ["in1] ["out1"])@ creates a
--  system definition from system funcion @mySysFun@ which has one input and
--  output signals.
-- 
--   The advantage of 'newSysDefTHName' over 'newSysDefTH' is that it 
--   doesn't suffer from the Template Haskell bug <http://hackage.haskell.org/trac/ghc/ticket/1800>, or in other words, it allows to declare the system 
--   defintion and system function in the same module.
--
--   However, since it doesn't have acces to the system function itself,
--   it can only give early error reports related to incorrect port identifiers
--   (process identifier duplicate errors will be reported at runtime).
newSysDefTHName :: Name     -- ^ Name of the system function 
         -> [PortId] -- ^ Input interface port identifiers 
         -> [PortId] -- ^ Output interface port identifiers
         -> ExpQ 
newSysDefTHName sysFName inIds outIds =  do
           sysFInfo <- reify sysFName
           -- Check that a function name was provided
           sysFType <- case sysFInfo of
                        VarI _ t _  _ -> return t
                        _             -> currError  (NonVarName sysFName)
           -- Check that the function complies with the expected type
           -- and extract the port types
           ((inTypes,inN),(outTypes, outN)) <- recover
                          (currError $ IncomSysF sysFName sysFType)
                          (checkSysFType sysFType)
           -- Check the ports
           let portCheck = checkSysDefPorts (show sysFName)
                                            (inIds, inN) 
                                            (outIds, outN)
           when (isJust portCheck) (currError (fromJust portCheck))
           -- Build the system definition
           errInfo <- currentModule
           let
            -- Input arguments passed to the  system function
            -- in order to get the netlist
            inArgs = [ [| Signal $ newInPort $(litE $ stringL id) |] 
                       | id <- inIds ]
            -- The system definition without type signature for the
            -- phantom parameter 
            untypedSysDef =
            -- The huge let part of this quasiquote is not
            -- really necesary but generates clearer code
             [|let 
               -- Generate the system netlist
               toList = $(signalTup2List outN)
               outNlSignals = toList $ $(appsE $ varE sysFName : inArgs)
               -- Rest of the system defintion
               inIface   = $(genIface inIds inTypes)
               outIface  = $(genIface outIds outTypes)
               errorInfo = errInfo
               in  SysDef $ PrimSysDef $ newURef $ 
                         SysDefVal (nameBase sysFName)
                                   (Netlist outNlSignals)
                                   inIface 
                                   outIface  
                                   (Just errorInfo) |] 
           -- We are done, we simply specify the concrete type of the SysDef
           sigE untypedSysDef (return $ ConT ''SysDef `AppT` sysFType)
 where currError  = qError "newSysDef"


        

----------------------------
-- Internal Helper Functions
----------------------------

-- | Error prone version of 'newSysDef'
newSysDefEProne :: SysFun f => f -- ^ system function 
                -> Maybe Loc -- ^ Location where the originating 
                             -- call took place (if available)
                -> SysId     -- ^ System function 
                -> [PortId]  -- ^ Input interface port identifiers 
                -> [PortId]  -- ^ Output interface port identifiers 
                -> EProne (SysDef f)
newSysDefEProne f mLoc sysId inIds outIds 
  -- check the ports for problems
  | isJust portCheck = throwError (fromJust portCheck)
  | otherwise = return (SysDef $ PrimSysDef $ newURef $ SysDefVal sysId
                                                        (Netlist nlist)
                                                        (zip inIds  inTypes)
                                                        (zip outIds outTypes)
                                                        mLoc)
 where (nlist, inTypes, outTypes) = applySysFun f inIds
       inN = length inIds
       outN = length outIds
       portCheck = checkSysDefPorts sysId (inIds, inN) (outIds, outN) 

-- | Check that the system definition ports match certain lengths and
--   don't containt duplicates
checkSysDefPorts :: SysId -- ^ System currently being checked
                 -> ([PortId], Int) -- ^ input ports and expected length
                 -> ([PortId], Int) -- ^ output ports and expected length
                 -> Maybe ForSyDeErr
checkSysDefPorts sysId (inIds, inN) (outIds, outN)  
  | inN  /= inIdsL = Just $ InIfaceLength (sysId, inN) (inIds, inIdsL)
  | outN /= outIdsL = Just $ OutIfaceLength (sysId, outN) (outIds, outIdsL)
  | isJust (maybeDup) = Just $ MultPortId  (fromJust maybeDup)
  | otherwise = Nothing
 where inIdsL  = length inIds
       outIdsL = length outIds
       maybeDup = findDup (inIds ++ outIds)

-- | Generate a lambda expression to transform a tuple of N 'Signal's into a 
-- a list of 'NlSignal's
signalTup2List :: Int  -- ^ size of the tuple
              ->  ExpQ
signalTup2List n = do -- Generate N signal variable paterns and
                      -- variable expressions refering to the same names
                      names <- replicateM n (newName "i")
                      let tupPat  = tupP  [conP 'Signal [varP n] | n <- names]
                          listExp = listE [varE n                | n <- names]
                      lamE [tupPat] listExp


-- | Find a duplicate in a list
findDup :: Eq a => [a] -> Maybe a
findDup []  = Nothing 
findDup [_] = Nothing
findDup (x:xs)
 | elem x xs = Just x
 | otherwise = findDup xs


-- | Generate a TypeRep expression given a Template Haskell Type
--   note that the use of typeOf cannot lead to errors since all the signal
--   types in a system function are guaranteed to be Typeable by construction
type2TypeRep :: Type -> ExpQ
type2TypeRep t = [| typeOf $(sigE [| undefined |] (return t) ) |]

-- | Generate an interface given its identifiers and Template Haskell Types
genIface :: [PortId] -> [Type] -> ExpQ
genIface [] _  = listE []
genIface _  [] = listE []
genIface (i:ix) (t:tx)  = do
 ListE rest <- genIface ix tx
 tupExp <- tupE [[| i |], type2TypeRep t]
 return (ListE (tupExp:rest)) 

