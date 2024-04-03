----------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC
-- Copyright   :  (c) SAM/KTH 2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- The corrent module is a container including all MoC libraries and
-- their domain interfaces.
----------------------------------------------------------------------

module ForSyDe.Shallow.MoC (
  -- | The library for the synchronous MoC
  module ForSyDe.Shallow.MoC.Synchronous,

  -- | The library for the most general form of the untimed MoC
  module ForSyDe.Shallow.MoC.Untimed,

  -- | The library for a specialized untimed MoC
  module ForSyDe.Shallow.MoC.Dataflow,

  -- | The library for the Synchronous Dataflow MoC
  module ForSyDe.Shallow.MoC.SDF,

  -- | The library for the continuous time MoC
  module ForSyDe.Shallow.MoC.CT,

  -- | The library for the domain interfaces
  module ForSyDe.Shallow.MoC.DomainInterface,

  -- | The library for the MoC interfaces
  module ForSyDe.Shallow.MoC.MoCInterface,

  -- | The library for the Cyclo-Static Dataflow MoC
  module ForSyDe.Shallow.MoC.CSDF,

  -- | The library for the Scenario Aware Dataflow MoC
  module ForSyDe.Shallow.MoC.SADF,

  -- | The library for the Discrete Event MoC
  module ForSyDe.Shallow.MoC.DE
  
  ) where

import ForSyDe.Shallow.MoC.Dataflow
import ForSyDe.Shallow.MoC.DomainInterface
import ForSyDe.Shallow.MoC.CT hiding (delayCT, addTime)
import ForSyDe.Shallow.MoC.MoCInterface
import ForSyDe.Shallow.MoC.SDF
import ForSyDe.Shallow.MoC.Synchronous
import ForSyDe.Shallow.MoC.Untimed
import ForSyDe.Shallow.MoC.CSDF
import ForSyDe.Shallow.MoC.SADF
import ForSyDe.Shallow.MoC.DE
