-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.UtilityLib
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The ForSyDeUtilityLib is a container including all libraries that
-- are related to the ForSyDe shallow-embedded implementation and
-- either extend the ForSyDe MoC libraries or add additional
-- functionality to ForSyDe.
-- 
-- * "ForSyDe.Shallow.DFT"
-- 
-- * "ForSyDe.Shallow.Memory"
-- 
-- * "ForSyDe.Shallow.Queue"
-- 
-- * "ForSyDe.Shallow.BitVector"
--
-- * "ForSyDe.Shallow.FilterLib"
-- 
-- * "ForSyDe.Shallow.Gaussian"		
-- 
-- * "ForSyDe.Shallow.PolyArith"		
--
-- * "ForSyDe.Shallow.StochasticLib"
-----------------------------------------------------------------------------
module ForSyDe.Shallow.UtilityLib(  
                      module ForSyDe.Shallow.DFT,            
                      module ForSyDe.Shallow.Memory,
                      module ForSyDe.Shallow.Queue
                      --module ForSyDe.Shallow.Combinators    
	              --module ForSyDe.Shallow.BitVector
                      --module ForSyDe.Shallow.FilterLib,
		      --module ForSyDe.Shallow.Gaussian,
		      --module ForSyDe.Shallow.PolyArith,
		      --module ForSyDe.Shallow.StochasticLib
		    ) where

import ForSyDe.Shallow.DFT          
import ForSyDe.Shallow.Memory
import ForSyDe.Shallow.Queue
--import ForSyDe.Shallow.Combinators
--import ForSyDe.Shallow.BitVector
--import ForSyDe.Shallow.FilterLib
--import ForSyDe.Shallow.Gaussian
--import ForSyDe.Shallow.PolyArith
--import ForSyDe.Shallow.StochasticLib


