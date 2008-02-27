-- ForSyDe.ForSyDeErr: GHC bootstrapping module for Netlist.hs
-- (it allows recursive imports in Netlist.hs)
-- See "How to compile mutually recursive modules" in GHC's manual for details

module ForSyDe.ForSyDeErr 
 (ForSyDeErr,
  intError,
  evalErr) where

-- | All Errors thrown or displayed in ForSyDe
data ForSyDeErr 


-- | This function helps to avoid defining ForSyDeErr in the bootstrapping file
evalErr :: String -> ForSyDeErr


--  Throws an internal error
intError :: String     -- ^ Function which caused the internal error 
         -> ForSyDeErr -- ^ Error to show
         -> a

