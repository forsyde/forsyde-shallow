-- SysDef.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it breaks the recursive import loop with Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details

module ForSyDe.System.SysDef where

import ForSyDe.OSharing
import ForSyDe.Ids
import Data.Typeable (TypeRep)

type Iface = [(PortId, TypeRep)]

newtype SysDef a = SysDef {unSysDef :: PrimSysDef}

newtype PrimSysDef = PrimSysDef {unPrimSysDef :: URef SysDefVal}

data SysDefVal 

oIface :: SysDefVal -> Iface