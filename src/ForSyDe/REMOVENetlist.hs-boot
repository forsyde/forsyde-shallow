{-# OPTIONS_GHC -fglasgow-exts #-}  -- due to Explicitly-kinded quantification 

-- Netlist.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it allows recursive imports in Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details
module ForSyDe.Netlist where

import Language.Haskell.TH (Type)

newtype Netlist container = Netlist (container NlTree)

type PortId = String

type ProcId = String

data  NlNode inputi

data NlProc inputi

data NlEdge node

type NlTag = String

newtype NlTree = NlTree  {rootEdge :: (NlEdge (NlNode NlTree))}

type NlSignal = NlTree

newUndef :: Type -> NlSignal 

newInPort :: PortId -> Type -> NlSignal

newOutPort :: PortId -> NlSignal -> Type -> NlSignal