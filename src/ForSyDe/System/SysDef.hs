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
   PortId,
   IFace) where

import ForSyDe.Signal
import {-# SOURCE #-} ForSyDe.Netlist 
import ForSyDe.OSharing
import ForSyDe.ForSyDeErr
import ForSyDe.System.SysFun (checkSysFType)

import Data.Maybe (isJust, fromJust)
import Control.Monad (when, replicateM)
import Language.Haskell.TH
import Language.Haskell.TH.LiftInstances ()




-- FIXME: it should be Iface not IFace
-- | Interface, describes the input or output ports of the system.
--   Each entry contains the name of the port and its type.
type IFace = [(PortId, Type)]


-- | We add a phantom parameter to indicate the type of the system 
newtype SysDef a = SysDef {unSysDef :: PrimSysDef}

-- | The Primitive System Definition.
--   Instead of just providing the value, a reference is provided
--   to allow sharing between instances.
newtype PrimSysDef = PrimSysDef {unPrimSysDef :: URef SysDefVal}

-- | The System Definition value
data SysDefVal = SysDefVal 
     {id      :: String,                    -- ^ Identifier of the System 
      t       :: Type,                      -- ^ Type of the system
      sysFun  :: [NlSignal] -> [NlSignal],  -- ^ System function
                                            --   in internal untyped form
      iIface  :: IFace,                     -- ^ Input  interface
      oIface  :: IFace,                     -- ^ Output interface 
      errInfo :: String}                    -- ^ Location of the call to 
                                            --   newSysDef which created this 
                                            --   System definition  
                                            --   (used for later error 
                                            --    reporting)


-- | 'Sysdef' constructor
--   Builds a 'SysDef' out of the name of a function describing the system 
--   (system function) and the its port identifers.
--
--   The system will later be identified by the basename 
--   (i.e. unqualified name) of the function.
--   Using different functions with the same basename to define different
--    'SysDef's can later cause name-clashes.
--   
--   Note: the function (f) whose name is provided is required to have the type
--         f :: Signal i1 -> Signal i2 -> ...... -> Signal in -> \-\- inputs
--              (Signal o1, Signal o2, .... , Signal om)         \-\- outputs
--
--         where n <- |N U {0}
--               m <- |N U {0}
--               i1 .. in and o1 .. om are monomorphic
newSysDef :: Name     -- ^ Name of the system function 
         -> [PortId] -- ^ Input interface port identifiers 
         -> [PortId] -- ^ Output interface port identifiers
         -> ExpQ 
newSysDef sysFName inIds outIds =  do
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
           -- Check that the port-id lists have the proper length
           let inIdsL  = length inIds
               outIdsL = length outIds
           when (inN  /= inIdsL) 
             (currError  (InIfaceLength (sysFName,inN) (inIds,inIdsL)))
           when (outN /= outIdsL) 
             (currError  (OutIfaceLength (sysFName,outN) (outIds,outIdsL)))
           -- Check that the port identifiers are unique
           let maybeDup = findDup (inIds ++ outIds)
           when (isJust maybeDup)
             (currError  (MultPortId  (fromJust maybeDup)))
           -- Build the system definition
           let iIface = zip inIds inTypes
               oIface = zip outIds outTypes 
           errInfo <- currentModule
           -- Names used to build the internal untyped version of the 
           -- system function
           names <- replicateM inN (newName "i")
           let
            -- Input patern for the internal system function
            inListPat = listP [varP n | n <- names ]
            -- Input arguments passed to the "real" system function
            inArgs    = [ [| Signal $(varE n) |] | n <- names ]
            -- The system definition without type signature for the
            -- phantom parameter 
            untypedSysDef =
            -- The huge let part of this quasiquote is not
            -- really necesary but generates clearer code
             [|let 
               -- Generate the system function
               toList = $(signalTup2List outN)
               sysFun = $(lam1E 
                            inListPat 
                            (appE [| toList |]
                                  (appsE $ varE sysFName : inArgs)) )  
               -- Rest of the system defintion
               sysType   = sysFType
               inIface   = iIface
               outIface  = oIface
               errorInfo = errInfo
               in  SysDef $ PrimSysDef $ newURef $ SysDefVal (show sysFName) 
                                                   sysType
                                                   sysFun
                                                   inIface 
                                                   outIface  
                                                   errorInfo |] 
           -- We are done, we simply specify the concrete type of the SysDef
           sigE untypedSysDef (return $ ConT ''SysDef `AppT` sysFType)
 where currError  = qError "newSysDef"


----------------------------
-- Internal Helper Functions
----------------------------


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
