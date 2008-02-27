-- ForSyDe.SysTem.SysDef.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it allows recursive imports in Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details
module ForSyDe.System.SysDef where

import ForSyDe.OSharing (URef)

import Language.Haskell.TH

-- FIXME: remove this or decide where to put it
type PortId = String

type IFace = [(PortId, Type)]

newtype SysDef a = SysDef {unSysDef :: PrimSysDef}

newtype PrimSysDef = PrimSysDef {unPrimSysDef :: URef SysDefVal}

oIface :: SysDefVal -> IFace

data SysDefVal