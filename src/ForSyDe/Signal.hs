-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Signal
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
--  
-- Provides the fundamental data structure of ForSyDe models: Signal.
--
-- /This Module is based on Lava2000:/ <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------

module ForSyDe.Signal  where


import {-# SOURCE #-} ForSyDe.Netlist
-- import ForSyDe.OSharing
-- import Language.Haskell.TH.TypeLib

-- | A signal can be seen as wire which carries values of certain type 
--   and which can be connected and processed by the two computational 
--   entities of a ForSyDe system: processes and block instances.
--

--   A Signal is internally represented as an edge of the graph representing
--   the system netlist.  
--   The phantom type parameter ensures type-consistency for the signal
--   processing functions.  
--   FIXME: we don't the end user to know about the internal rep of Signal

newtype Signal a = Signal {unSignal :: NlSignal}

{- FINISH and ... think about where to put signalize -}
-- | Creates a constant signal
{-
constSY :: (Typeable a, Lift a) => a -> Signal a
constSY a = Signal (newNodeOutSig (newURef Const (mkProcVal a)) ConstOut )
   where  outType = signalize 
-}
           



