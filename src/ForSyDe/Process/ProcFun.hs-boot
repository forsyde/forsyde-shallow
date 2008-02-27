-- ForSyDe.Proc.ProcFun.hs-boot: GHC bootstrapping module for Netlist.hs
-- (it allows recursive imports in Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details
module ForSyDe.Process.ProcFun 
 (ProcFun(..), 
  DynProcFun,
  newProcFun) where

import Data.Dynamic
import Language.Haskell.TH

-- | A Process Function 
data ProcFun a = 
   ProcFun {val  :: a,        -- ^ Value of the function
            name :: Name,     -- ^ Function Name 
                                    -- (FIXME: maybe just a String?) 
            typ  :: Type,     -- ^ Function Type
            cls  :: [Clause]} -- ^ Function clauses 


-- | Dynamic process function (an untyped 'ProcFun')
type DynProcFun = ProcFun Dynamic


newProcFun :: Q [Dec] -> ExpQ
