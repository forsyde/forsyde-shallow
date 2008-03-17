{-# LANGUAGE TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.Instantiate
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides instantiation capabilities to System Definitions
-- ('SysDef's).through a  Template-Haskell-based function: 'instantiate'.
--
-- Thanks to 'instantiate' ForSyDe models have hierarchy capabilities.
--
-----------------------------------------------------------------------------
module ForSyDe.System.Instantiate (instantiate) where

import ForSyDe.Ids
import ForSyDe.OSharing (readURef)
import ForSyDe.Netlist
import ForSyDe.Signal
import ForSyDe.System.SysDef (SysDef(..), PrimSysDef(..), SysDefVal(..))
import ForSyDe.System.SysFun (checkSysFType)
import ForSyDe.ForSyDeErr

import Language.Haskell.TH
import Control.Monad (replicateM)


-- | Generates an instance of a 'SysDef' in the form of  
--   function out of the name of a 'SysDef' with the same type as its 
--   system function. The resulting function can then be used to plug the 
--   instance to the rest of the system.
instantiate :: ProcId -> Name -> ExpQ
-- Note: we use a Name arg and not a SysDef directly for two reasons:
-- 1) It wouldn't allow to instantiate a SysDef in the same module
--    the SysDef is defined.
--
--    That is due to current lack of Template Haskell support for running
--    functions defined in the same module
--
--    see http://hackage.haskell.org/trac/ghc/ticket/1800
--
-- 2) Without the SysDef Name, in order to generate a SysIns node
--    the whole SysDef would need to be lifted (a SysIns constains a SysDef)
--    and we don't want that:
--     * performance issue
--     * it would require to have a Lift instance of all the netlist types
--     * it's not feasible anyway (URefs cannot be lifted since they are unsafe)
--       
instantiate insId sysDefN = 
           do i <- reify sysDefN
              -- Gather the system function type, checking if
              -- a SysDef variable name was provided
              sysFType <- case i of
                 VarI _ t  _  _ -> 
                   case t  of 
                    ConT name `AppT` syst | name == ''SysDef -> return syst
                    _  -> currError (NonSysDef sysDefN t) 
                 _  -> currError  (NonVarName sysDefN)
              -- Get the number of inputs and outputs of the system
              ((_, nIn),(_, nOut)) <- checkSysFType sysFType
              -- Generate a Signal constructor-pattern for each input 
              inNames <- replicateM nIn (newName "i")     
              let inPats  = [conP 'Signal [varP n] | n <- inNames]
              -- Generate a list with all the input NlSignals
                  inList  = listE [varE n | n <- inNames]
              -- Generate the body of the instantiation function 
                  insFBody  =    
                    [|
                      let 
                      -- The system definition
                      sysDef = $(varE sysDefN) 
                      -- The system definition value
                      sysVal  = (readURef.unPrimSysDef.unSysDef) sysDef
                      -- Put all the inputs in a list
                      allIns = $inList
                      -- Input info of the instance node
                      inputsInfo =  
                          zipWith (\(id,_) s -> (id,s))  (iIface sysVal) allIns
                      -- The instance node
                      insNode = newSysIns insId sysDef inputsInfo
                      -- Generate a Signal for each output
                      outList = map (\(id, _) -> newNodeOutSig 
                                                  insNode (SysInsOut id)) 
                                (oIface sysVal) 
                      toTup = $(nlSignalList2Tup nOut)       
                      in  toTup outList|]
              -- Done, just add the input pattern and the concrete
              -- type signature 
              sigE (lamE inPats insFBody) (return sysFType)    
  where currError = qError "instantiate"


----------------------------
-- Internal Helper Functions
----------------------------    

-- | Generate a lambda expression to transform a list of N 'NlSignal's into a 
-- a tuple of 'Signal's
nlSignalList2Tup :: Int  -- ^ size of the tuple
                 -> ExpQ
nlSignalList2Tup n = 
    do -- Generate a list pattern of n elements
       -- and a tuple including those elements transformed in Signal
       names <- replicateM n (newName "elem")
       let listPat = listP [varP n | n <- names]
           tupExp  = tupE  [conE 'Signal `appE` varE n | n <- names]
       lamE [listPat] tupExp
