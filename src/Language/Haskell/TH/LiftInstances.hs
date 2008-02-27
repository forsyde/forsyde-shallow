{-# LANGUAGE TemplateHaskell #-}  
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.LiftInstances
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides  'Lift' instances for all the AST-types defined
-- in "Language.Haskell.Syntax":
-- 'Guard' 'Strict', 'Callconv', 'Safety','Body', 'Con', 'FunDep', 'Foreign', 
-- 'Lit', 'Pat', 'Match', 'Stmt', 'Range', 'Clause', 'Type', 'Dec', 'Exp'
--
-- Furthermore it provides a 'Lift' instance of 'Ratio' 
-- (essential for some of the other instantiations)
-- 
-----------------------------------------------------------------------------
module Language.Haskell.TH.LiftInstances where

import Language.Haskell.TH.Lift (deriveLift)

import Language.Haskell.TH 
 (Guard,
  Strict,
  Callconv,
  Safety,
  Body, 
  Con, 
  FunDep, 
  Foreign, 
  Lit, 
  Pat, 
  Match, 
  Stmt, 
  Range, 
  Clause, 
  Type, 
  Dec, 
  Exp)
import Control.Monad (mapM)
import Data.Ratio (Ratio)

$(mapM deriveLift 
      [''Ratio,
       ''Guard,
       ''Strict,
       ''Callconv,
       ''Safety,
       ''Body, 
       ''Con, 
       ''FunDep, 
       ''Foreign, 
       ''Lit, 
       ''Pat, 
       ''Match, 
       ''Stmt, 
       ''Range, 
       ''Clause, 
       ''Type, 
       ''Dec, 
       ''Exp])