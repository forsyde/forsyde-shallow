-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.SADF
-- Copyright   :  (c) Ricardo Bonna, KTH/ICT/ES, ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Experimental lib. Further test needed
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.SADF (
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delaySADF,
  -- * Kernels
  -- | Based on the process constructors in the SADF-MoC, the
  -- SADF-library provides SADF-kernels with single or multiple inputs
  kernel11SADF, kernel12SADF, kernel13SADF, kernel14SADF, kernel15SADF,
  kernel21SADF, kernel22SADF, kernel23SADF, kernel24SADF, kernel25SADF,
  kernel31SADF, kernel32SADF, kernel33SADF, kernel34SADF, kernel35SADF,
  kernel41SADF, kernel42SADF, kernel43SADF, kernel44SADF, kernel45SADF,
  kernel51SADF, kernel52SADF, kernel53SADF, kernel54SADF, kernel55SADF,
  -- * Detectors
  -- | Based on the process constructors in the SADF-MoC, the
  -- SADF-library provides SADF-detectors with single or multiple inputs
  detector11SADF, detector12SADF, detector13SADF, detector14SADF, detector15SADF,
  detector21SADF, detector22SADF, detector23SADF, detector24SADF, detector25SADF,
  detector31SADF, detector32SADF, detector33SADF, detector34SADF, detector35SADF,
  detector41SADF, detector42SADF, detector43SADF, detector44SADF, detector45SADF,
  detector51SADF, detector52SADF, detector53SADF, detector54SADF, detector55SADF
  ) where

import ForSyDe.Shallow.Core


-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delaynSADF' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delaySADF :: [a] -> Signal a -> Signal a
delaySADF initial_tokens xs = signal initial_tokens +-+ xs


------------------------------------------------------------------------
--
-- SADF KERNELS
--
------------------------------------------------------------------------

-- > Kernels with one output

-- | The process constructor 'kernel11SADF' constructs an SADF kernel with
-- one data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel11SADF :: Signal (Int, Int, [a] -> [b]) -- ^ Control signal
             -> Signal a                      -- ^ Input
             -> Signal b                      -- ^ Output
kernel11SADF = mapSADF

-- | The process constructor 'kernel21SADF' constructs an SADF kernel with
-- two data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel21SADF :: Signal ((Int, Int), Int, [a] -> [b] -> [c])
             -> Signal a -> Signal b
             -> Signal c
kernel21SADF = zipWithSADF

-- | The process constructor 'kernel31SADF' constructs an SADF kernel with
-- three data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel31SADF :: Signal ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])
             -> Signal a -> Signal b -> Signal c
             -> Signal d
kernel31SADF = zipWith3SADF

-- | The process constructor 'kernel41SADF' constructs an SADF kernel with
-- four data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel41SADF :: Signal ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> Signal e
kernel41SADF = zipWith4SADF

-- | The process constructor 'kernel51SADF' constructs an SADF kernel with
-- five data input and one data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel51SADF :: Signal ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> Signal f
kernel51SADF = zipWith5SADF


-- > Kernels with two outputs

-- | The process constructor 'kernel12SADF' constructs an SADF kernel with
-- one data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel12SADF :: Signal (Int, (Int, Int), [a] -> ([b], [c]))
             -> Signal a
             -> (Signal b, Signal c)
kernel12SADF ct xs = unzipSADF (get_prodToken ct) $ mapSADF (inpOut1n ct) xs

-- | The process constructor 'kernel22SADF' constructs an SADF kernel with
-- two data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel22SADF :: Signal ((Int, Int), (Int, Int), [a] -> [b] -> ([c], [d]))
             -> Signal a -> Signal b
             -> (Signal c, Signal d)
kernel22SADF ct xs ys = unzipSADF (get_prodToken ct) $ zipWithSADF (inpOut2n ct) xs ys

-- | The process constructor 'kernel32SADF' constructs an SADF kernel with
-- three data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel32SADF :: Signal ((Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> ([d], [e]))
             -> Signal a -> Signal b -> Signal c
             -> (Signal d, Signal e)
kernel32SADF ct as bs cs
  = unzipSADF (get_prodToken ct) $ zipWith3SADF (inpOut3n ct) as bs cs

-- | The process constructor 'kernel42SADF' constructs an SADF kernel with
-- four data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel42SADF :: Signal ((Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f]))
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f)
kernel42SADF ct as bs cs ds
  = unzipSADF (get_prodToken ct) $ zipWith4SADF (inpOut4n ct) as bs cs ds

-- | The process constructor 'kernel52SADF' constructs an SADF kernel with
-- five data input and two data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel52SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int), [a]
             -> [b] -> [c] -> [d] -> [e] -> ([f], [g]))
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g)
kernel52SADF ct as bs cs ds es
  = unzipSADF (get_prodToken ct) $ zipWith5SADF (inpOut5n ct) as bs cs ds es


-- > Kernels with three outputs

-- | The process constructor 'kernel13SADF' constructs an SADF kernel with
-- one data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel13SADF :: Signal (Int, (Int, Int, Int), [a] -> ([b], [c], [d]))
             -> Signal a
             -> (Signal b, Signal c, Signal d)
kernel13SADF ct xs = unzip3SADF (get_prodToken ct) $ mapSADF (inpOut1n ct) xs

-- | The process constructor 'kernel23SADF' constructs an SADF kernel with
-- two data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel23SADF :: Signal ((Int, Int), (Int, Int, Int), [a] -> [b] -> ([c], [d], [e]))
             -> Signal a -> Signal b
             -> (Signal c, Signal d, Signal e)
kernel23SADF ct xs ys = unzip3SADF (get_prodToken ct) $ zipWithSADF (inpOut2n ct) xs ys

-- | The process constructor 'kernel33SADF' constructs an SADF kernel with
-- three data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel33SADF :: Signal ((Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f]))
             -> Signal a -> Signal b -> Signal c
             -> (Signal d, Signal e, Signal f)
kernel33SADF ct as bs cs
  = unzip3SADF (get_prodToken ct) $ zipWith3SADF (inpOut3n ct) as bs cs

-- | The process constructor 'kernel43SADF' constructs an SADF kernel with
-- four data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel43SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> ([e], [f], [g]))
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g)
kernel43SADF ct as bs cs ds
  = unzip3SADF (get_prodToken ct) $ zipWith4SADF (inpOut4n ct) as bs cs ds

-- | The process constructor 'kernel53SADF' constructs an SADF kernel with
-- five data input and three data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel53SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h]))
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h)
kernel53SADF ct as bs cs ds es
  = unzip3SADF (get_prodToken ct) $ zipWith5SADF (inpOut5n ct) as bs cs ds es


-- > Kernels with four outputs

-- | The process constructor 'kernel14SADF' constructs an SADF kernel with
-- one data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel14SADF :: Signal (Int, (Int, Int, Int, Int), [a] -> ([b], [c], [d], [e]))
             -> Signal a
             -> (Signal b, Signal c, Signal d, Signal e)
kernel14SADF ct xs = unzip4SADF (get_prodToken ct) $ mapSADF (inpOut1n ct) xs

-- | The process constructor 'kernel24SADF' constructs an SADF kernel with
-- two data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel24SADF :: Signal ((Int, Int), (Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f]))
             -> Signal a -> Signal b
             -> (Signal c, Signal d, Signal e, Signal f)
kernel24SADF ct xs ys = unzip4SADF (get_prodToken ct) $ zipWithSADF (inpOut2n ct) xs ys

-- | The process constructor 'kernel34SADF' constructs an SADF kernel with
-- three data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel34SADF :: Signal ((Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> ([d], [e], [f], [g]))
             -> Signal a -> Signal b -> Signal c
             -> (Signal d, Signal e, Signal f, Signal g)
kernel34SADF ct as bs cs
  = unzip4SADF (get_prodToken ct) $ zipWith3SADF (inpOut3n ct) as bs cs

-- | The process constructor 'kernel44SADF' constructs an SADF kernel with
-- four data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel44SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h]))
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g, Signal h)
kernel44SADF ct as bs cs ds
  = unzip4SADF (get_prodToken ct) $ zipWith4SADF (inpOut4n ct) as bs cs ds

-- | The process constructor 'kernel54SADF' constructs an SADF kernel with
-- five data input and four data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel54SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i]))
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h, Signal i)
kernel54SADF ct as bs cs ds es
  = unzip4SADF (get_prodToken ct) $ zipWith5SADF (inpOut5n ct) as bs cs ds es


-- > Kernels with five outputs

-- | The process constructor 'kernel15SADF' constructs an SADF kernel with
-- one data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel15SADF :: Signal (Int, (Int, Int, Int, Int, Int), [a] -> ([b], [c], [d], [e], [f]))
             -> Signal a
             -> (Signal b, Signal c, Signal d, Signal e, Signal f)
kernel15SADF ct xs = unzip5SADF (get_prodToken ct) $ mapSADF (inpOut1n ct) xs

-- | The process constructor 'kernel25SADF' constructs an SADF kernel with
-- two data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel25SADF :: Signal ((Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f], [g]))
             -> Signal a -> Signal b
             -> (Signal c, Signal d, Signal e, Signal f, Signal g)
kernel25SADF ct xs ys = unzip5SADF (get_prodToken ct) $ zipWithSADF (inpOut2n ct) xs ys

-- | The process constructor 'kernel35SADF' constructs an SADF kernel with
-- three data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel35SADF :: Signal ((Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> ([d], [e], [f], [g], [h]))
             -> Signal a -> Signal b -> Signal c
             -> (Signal d, Signal e, Signal f, Signal g, Signal h)
kernel35SADF ct as bs cs
  = unzip5SADF (get_prodToken ct) $ zipWith3SADF (inpOut3n ct) as bs cs

-- | The process constructor 'kernel45SADF' constructs an SADF kernel with
-- four data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel45SADF :: Signal ((Int, Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h], [i]))
             -> Signal a -> Signal b -> Signal c -> Signal d
             -> (Signal e, Signal f, Signal g, Signal h, Signal i)
kernel45SADF ct as bs cs ds
  = unzip5SADF (get_prodToken ct) $ zipWith4SADF (inpOut4n ct) as bs cs ds

-- | The process constructor 'kernel55SADF' constructs an SADF kernel with
-- five data input and five data output signals. The scenario (token rates and
-- function) is determined by the control signal.
kernel55SADF :: Signal ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int),
             [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i], [j]))
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
             -> (Signal f, Signal g, Signal h, Signal i, Signal j)
kernel55SADF ct as bs cs ds es
  = unzip5SADF (get_prodToken ct) $ zipWith5SADF (inpOut5n ct) as bs cs ds es


------------------------------------------------------------------------
--
-- SADF DETECTORS
--
------------------------------------------------------------------------

-- > Detectors with one output

-- | The process constructor 'detector11SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with
-- a single data input and a single control output signals.
detector11SADF :: Int                 -- ^ consumption rates (@c@)
               -> (s -> [a] -> s)     -- ^ next state function (@f@)
               -> (s -> (Int, [y]))   -- ^ scenario selection (@g@)
               -> s                   -- ^ initial state (@s0@)
               -> Signal a            -- ^ Input
               -> Signal y            -- ^ Output
detector11SADF c f g s0 as = outputFSM g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector21SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with two data input and a
-- single control output signals.
detector21SADF :: (Int, Int)
               -> (s -> [a] -> [b] -> s)
               -> (s -> (Int, [y]))
               -> s
               -> Signal a -> Signal b
               -> Signal y
detector21SADF c f g s0 as bs = outputFSM g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector31SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with three data input and a
-- single control output signals.
detector31SADF :: (Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> s)
               -> (s -> (Int, [y]))
               -> s
               -> Signal a -> Signal b -> Signal c
               -> Signal y
detector31SADF c f g s0 as bs cs = outputFSM g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector41SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with four data input and a
-- single control output signals.
detector41SADF :: (Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> s)
               -> (s -> (Int, [y]))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> Signal y
detector41SADF c f g s0 as bs cs ds = outputFSM g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector51SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with five data input and a
-- single control output signals.
detector51SADF :: (Int, Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
               -> (s -> (Int, [y]))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> Signal y
detector51SADF c f g s0 as bs cs ds es = outputFSM g next_state
  where next_state = nextStateFSM5 c f current_state as bs cs ds es
        current_state = delaySADF [s0] next_state


-- > Detectors with two output

-- | The process constructor 'detector12SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with a single data input and two
-- control output signals.
detector12SADF :: Int
               -> (s -> [a] -> s)
               -> (s -> ((Int, Int), ([y1], [y2])))
               -> s
               -> Signal a
               -> (Signal y1, Signal y2)
detector12SADF c f g s0 as = outputFSM2 g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector22SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with two data input and two
-- control output signals.
detector22SADF :: (Int, Int)
               -> (s -> [a] -> [b] -> s)
               -> (s -> ((Int, Int), ([y1], [y2])))
               -> s
               -> Signal a -> Signal b
               -> (Signal y1, Signal y2)
detector22SADF c f g s0 as bs = outputFSM2 g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector32SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with three data input and two
-- control output signals.
detector32SADF :: (Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> s)
               -> (s -> ((Int, Int), ([y1], [y2])))
               -> s
               -> Signal a -> Signal b -> Signal c
               -> (Signal y1, Signal y2)
detector32SADF c f g s0 as bs cs = outputFSM2 g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector42SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with four data input and two
-- control output signals.
detector42SADF :: (Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> s)
               -> (s -> ((Int, Int), ([y1], [y2])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal y1, Signal y2)
detector42SADF c f g s0 as bs cs ds = outputFSM2 g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector52SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with five data input and two
-- control output signals.
detector52SADF :: (Int, Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
               -> (s -> ((Int, Int), ([y1], [y2])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal y1, Signal y2)
detector52SADF c f g s0 as bs cs ds es = outputFSM2 g next_state
  where next_state = nextStateFSM5 c f current_state as bs cs ds es
        current_state = delaySADF [s0] next_state


-- > Detectors with three output

-- | The process constructor 'detector13SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with a single data input and three
-- control output signals.
detector13SADF :: Int
               -> (s -> [a] -> s)
               -> (s -> ((Int, Int, Int), ([y1], [y2], [y3])))
               -> s
               -> Signal a
               -> (Signal y1, Signal y2, Signal y3)
detector13SADF c f g s0 as = outputFSM3 g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector23SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with two data input and three
-- control output signals.
detector23SADF :: (Int, Int)
               -> (s -> [a] -> [b] -> s)
               -> (s -> ((Int, Int, Int), ([y1], [y2], [y3])))
               -> s
               -> Signal a -> Signal b
               -> (Signal y1, Signal y2, Signal y3)
detector23SADF c f g s0 as bs = outputFSM3 g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector33SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with three data input and three
-- control output signals.
detector33SADF :: (Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> s)
               -> (s -> ((Int, Int, Int), ([y1], [y2], [y3])))
               -> s
               -> Signal a -> Signal b -> Signal c
               -> (Signal y1, Signal y2, Signal y3)
detector33SADF c f g s0 as bs cs = outputFSM3 g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector43SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with four data input and three
-- control output signals.
detector43SADF :: (Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> s)
               -> (s -> ((Int, Int, Int), ([y1], [y2], [y3])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal y1, Signal y2, Signal y3)
detector43SADF c f g s0 as bs cs ds = outputFSM3 g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector53SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with five data input and three
-- control output signals.
detector53SADF :: (Int, Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
               -> (s -> ((Int, Int, Int), ([y1], [y2], [y3])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal y1, Signal y2, Signal y3)
detector53SADF c f g s0 as bs cs ds es = outputFSM3 g next_state
  where next_state = nextStateFSM5 c f current_state as bs cs ds es
        current_state = delaySADF [s0] next_state


-- > Detectors with four output

-- | The process constructor 'detector14SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with a single data input and four
-- control output signals.
detector14SADF :: Int
               -> (s -> [a] -> s)
               -> (s -> ((Int, Int, Int, Int), ([y1], [y2], [y3], [y4])))
               -> s
               -> Signal a
               -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector14SADF c f g s0 as = outputFSM4 g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector24SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with two data input and four
-- control output signals.
detector24SADF :: (Int, Int)
               -> (s -> [a] -> [b] -> s)
               -> (s -> ((Int, Int, Int, Int), ([y1], [y2], [y3], [y4])))
               -> s
               -> Signal a -> Signal b
               -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector24SADF c f g s0 as bs = outputFSM4 g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector34SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with three data input and four
-- control output signals.
detector34SADF :: (Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> s)
               -> (s -> ((Int, Int, Int, Int), ([y1], [y2], [y3], [y4])))
               -> s
               -> Signal a -> Signal b -> Signal c
               -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector34SADF c f g s0 as bs cs = outputFSM4 g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector44SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with four data input and four
-- control output signals.
detector44SADF :: (Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> s)
               -> (s -> ((Int, Int, Int, Int), ([y1], [y2], [y3], [y4])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector44SADF c f g s0 as bs cs ds = outputFSM4 g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector54SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with five data input and four
-- control output signals.
detector54SADF :: (Int, Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
               -> (s -> ((Int, Int, Int, Int), ([y1], [y2], [y3], [y4])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal y1, Signal y2, Signal y3, Signal y4)
detector54SADF c f g s0 as bs cs ds es = outputFSM4 g next_state
  where next_state = nextStateFSM5 c f current_state as bs cs ds es
        current_state = delaySADF [s0] next_state


-- > Detectors with five output

-- | The process constructor 'detector15SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with a single data input and five
-- control output signals.
detector15SADF :: Int
               -> (s -> [a] -> s)
               -> (s -> ((Int, Int, Int, Int, Int), ([y1], [y2], [y3], [y4], [y5])))
               -> s
               -> Signal a
               -> (Signal y1, Signal y2, Signal y3, Signal y4, Signal y5)
detector15SADF c f g s0 as = outputFSM5 g next_state
  where next_state = nextStateFSM c f current_state as
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector25SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with two data input and five
-- control output signals.
detector25SADF :: (Int, Int)
               -> (s -> [a] -> [b] -> s)
               -> (s -> ((Int, Int, Int, Int, Int), ([y1], [y2], [y3], [y4], [y5])))
               -> s
               -> Signal a -> Signal b
               -> (Signal y1, Signal y2, Signal y3, Signal y4, Signal y5)
detector25SADF c f g s0 as bs = outputFSM5 g next_state
  where next_state = nextStateFSM2 c f current_state as bs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector35SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with three data input and five
-- control output signals.
detector35SADF :: (Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> s)
               -> (s -> ((Int, Int, Int, Int, Int), ([y1], [y2], [y3], [y4], [y5])))
               -> s
               -> Signal a -> Signal b -> Signal c
               -> (Signal y1, Signal y2, Signal y3, Signal y4, Signal y5)
detector35SADF c f g s0 as bs cs = outputFSM5 g next_state
  where next_state = nextStateFSM3 c f current_state as bs cs
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector45SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with four data input and five
-- control output signals.
detector45SADF :: (Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> s)
               -> (s -> ((Int, Int, Int, Int, Int), ([y1], [y2], [y3], [y4], [y5])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal y1, Signal y2, Signal y3, Signal y4, Signal y5)
detector45SADF c f g s0 as bs cs ds = outputFSM5 g next_state
  where next_state = nextStateFSM4 c f current_state as bs cs ds
        current_state = delaySADF [s0] next_state

-- | The process constructor 'detector55SADF' takes the consumption token rate
-- (@c@), the state transition function (@f@), the scenario selection (@g@) and
-- the initial state (@s0@), and constructs an SADF detector with five data input and five
-- control output signals.
detector55SADF :: (Int, Int, Int, Int, Int)
               -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
               -> (s -> ((Int, Int, Int, Int, Int), ([y1], [y2], [y3], [y4], [y5])))
               -> s
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal y1, Signal y2, Signal y3, Signal y4, Signal y5)
detector55SADF c f g s0 as bs cs ds es = outputFSM5 g next_state
  where next_state = nextStateFSM5 c f current_state as bs cs ds es
        current_state = delaySADF [s0] next_state


------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS (not exported)
------------------------------------------------------------------------

-- | The process constructor 'mapSADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes an input signal and results
-- in an output signal
mapSADF :: Signal (Int, Int, [a] -> [b]) -> Signal a -> Signal b
mapSADF NullS _ = NullS
mapSADF ct xs
  | c < 0 = error "mapSADF: Number of consumed tokens must be a non-negative integer"
  | not $ sufficient_tokens c xs  = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ mapSADF (tailS ct) (dropS c xs)
                else
                  error "mapSADF: Function does not produce correct number of tokens"
  where (c, p, f) = headS ct
        consumed_tokens = fromSignal $ takeS c xs
        produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithSADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes two input signals and
-- results in an output signal
zipWithSADF :: Signal ((Int, Int), Int, [a] -> [b] -> [c])
            -> Signal a -> Signal b -> Signal c
zipWithSADF NullS _ _ = NullS
zipWithSADF ct as bs
  | c1 < 0 || c2 < 0  = error "zipWithSADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWithSADF (tailS ct) (dropS c1 as) (dropS c2 bs)
                else
                  error "zipWithSADF: Function does not produce correct number of tokens"
  where ((c1,c2), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs


-- | The process constructor 'zipWith3SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes three input signals and
-- results in an output signal
zipWith3SADF :: Signal ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])
             -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3SADF NullS _ _ _ = NullS
zipWith3SADF ct as bs cs
  | c1 < 0 || c2 < 0 || c3 < 0
    = error "zipWith3SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith3SADF (tailS ct) (dropS c1 as)
                                                        (dropS c2 bs) (dropS c3 cs)
                else
                  error "zipWith3SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs


-- | The process constructor 'zipWith4SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes four input signals and
-- results in an output signal
zipWith4SADF :: Signal ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
zipWith4SADF NullS _ _ _ _ = NullS
zipWith4SADF ct as bs cs ds
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0
    = error "zipWith4SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith4SADF (tailS ct) (dropS c1 as)
                                              (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
                else
                  error "zipWith4SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3, c4), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        produced_tokens = f consumed_tokens_as consumed_tokens_bs
                            consumed_tokens_cs consumed_tokens_ds


-- | The process constructor 'zipWith5SADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes five input signals and
-- results in an output signal
zipWith5SADF :: Signal ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
             -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f
zipWith5SADF NullS _ _ _ _ _ = NullS
zipWith5SADF ct as bs cs ds es
  | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0 || c5 < 0
    = error "zipWith5SADF: Number of consumed tokens must be a non-negative integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds)
    || (not $ sufficient_tokens c5 es) = NullS
  | otherwise = if length produced_tokens == p then
                  signal produced_tokens +-+ zipWith5SADF (tailS ct) (dropS c1 as)
                                              (dropS c2 bs) (dropS c3 cs) (dropS c4 ds) (dropS c5 es)
                else
                  error "zipWith5SADF: Function does not produce correct number of tokens"
  where ((c1, c2, c3, c4, c5), p, f) = headS ct
        consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        consumed_tokens_es = fromSignal $ takeS c5 es
        produced_tokens = f consumed_tokens_as consumed_tokens_bs
                            consumed_tokens_cs consumed_tokens_ds consumed_tokens_es


------------------------------------------------------------------------
-- unzipSADF Processes (not exported)
------------------------------------------------------------------------

unzipSADF :: [(Int, Int)] -> Signal ([a], [b]) -> (Signal a, Signal b)
unzipSADF [] _ = (NullS, NullS)
unzipSADF _ NullS = (NullS, NullS)
unzipSADF ((p1, p2) : ps) ((s1, s2) :- ss)
  | length s1 /= p1 || length s2 /= p2 = error "unzipSADF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2)
  where (sr1, sr2) = unzipSADF ps ss


unzip3SADF :: [(Int, Int, Int)] -> Signal ([a], [b], [c])
           -> (Signal a, Signal b, Signal c)
unzip3SADF [] _ = (NullS, NullS, NullS)
unzip3SADF _ NullS = (NullS, NullS, NullS)
unzip3SADF ((p1, p2, p3) : ps) ((s1, s2, s3) :- ss)
  | length s1 /= p1 || length s2 /= p2
    || length s3 /= p3 = error "unzip3SADF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3)
  where (sr1, sr2, sr3) = unzip3SADF ps ss


unzip4SADF :: [(Int, Int, Int, Int)] -> Signal ([a], [b], [c], [d])
           -> (Signal a, Signal b, Signal c, Signal d)
unzip4SADF [] _ = (NullS, NullS, NullS, NullS)
unzip4SADF _ NullS = (NullS, NullS, NullS, NullS)
unzip4SADF ((p1, p2, p3, p4) : ps) ((s1, s2, s3, s4) :- ss)
  | length s1 /= p1 || length s2 /= p2
    || length s3 /= p3 || length s4 /= p4 = error "unzip4SADF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3, signal s4 +-+ sr4)
  where (sr1, sr2, sr3, sr4) = unzip4SADF ps ss


unzip5SADF :: [(Int, Int, Int, Int, Int)] -> Signal ([a], [b], [c], [d], [e])
           -> (Signal a, Signal b, Signal c, Signal d, Signal e)
unzip5SADF [] _ = (NullS, NullS, NullS, NullS, NullS)
unzip5SADF _ NullS = (NullS, NullS, NullS, NullS, NullS)
unzip5SADF ((p1, p2, p3, p4, p5) : ps) ((s1, s2, s3, s4, s5) :- ss)
  | length s1 /= p1 || length s2 /= p2
    || length s3 /= p3 || length s4 /= p4
    || length s5 /= p5 = error "unzip5SADF: Process does not produce correct number of tokens"
  | otherwise = (signal s1 +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3,
                 signal s4 +-+ sr4, signal s5 +-+ sr5)
  where (sr1, sr2, sr3, sr4, sr5) = unzip5SADF ps ss

------------------------------------------------------------------------
--
-- Helper functions (not exported!)
--
------------------------------------------------------------------------

sufficient_tokens :: (Num a, Eq a, Ord a) => a -> Signal t -> Bool
sufficient_tokens 0 _     = True
sufficient_tokens _ NullS = False
sufficient_tokens n (_:-xs)
 = if n < 0 then
     error "sufficient_tokens: n must not be negative"
   else
     sufficient_tokens (n-1) xs


get_prodToken :: Signal (a,b,c) -> [b]
get_prodToken NullS = []
get_prodToken ((_, x, _):-xs) = x : get_prodToken xs


inpOut1n :: Signal (it, ot, [a] -> y) -> Signal (it, Int, [a] -> [y])
inpOut1n NullS = NullS
inpOut1n ((it, _, f):-xs) = (it, 1, \a -> [f a]) :- inpOut1n xs

inpOut2n :: Signal (it, ot, [a] -> [b] -> y) -> Signal (it, Int, [a] -> [b] -> [y])
inpOut2n NullS = NullS
inpOut2n ((it, _, f):-xs) = (it, 1, \a b -> [f a b]) :- inpOut2n xs

inpOut3n :: Signal (it, ot, [a] -> [b] -> [c] -> y)
         -> Signal (it, Int, [a] -> [b] -> [c] -> [y])
inpOut3n NullS = NullS
inpOut3n ((it, _, f):-xs) = (it, 1, \a b c -> [f a b c]) :- inpOut3n xs

inpOut4n :: Signal (it, ot, [a] -> [b] -> [c] -> [d] -> y)
         -> Signal (it, Int, [a] -> [b] -> [c] -> [d] -> [y])
inpOut4n NullS = NullS
inpOut4n ((it, _, f):-xs) = (it, 1, \a b c d -> [f a b c d]) :- inpOut4n xs

inpOut5n :: Signal (it, ot, [a] -> [b] -> [c] -> [d] -> [e] -> y)
         -> Signal (it, Int, [a] -> [b] -> [c] -> [d] -> [e] -> [y])
inpOut5n NullS = NullS
inpOut5n ((it, _, f):-xs) = (it, 1, \a b c d e -> [f a b c d e]) :- inpOut5n xs

---------------------------------------------------------
-- Helper functios to the detector's FSM  (not exported)
---------------------------------------------------------

nextStateFSM :: Int -> (s -> [a] -> s)
             -> Signal s -> Signal a -> Signal s
nextStateFSM _ _ NullS _ = NullS
nextStateFSM _ _ _ NullS = NullS
nextStateFSM c f ss as
  | c <= 0 = error "nextStateFSM: Number of consumed tokens must be positive integer"
  | not $ sufficient_tokens c as = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM c f (tailS ss) (dropS c as)
  where consumed_tokens_as = fromSignal $ takeS c as
        current_state = headS ss
        next_state = f current_state consumed_tokens_as


nextStateFSM2 :: (Int, Int) -> (s -> [a] -> [b] -> s)
              -> Signal s -> Signal a -> Signal b -> Signal s
nextStateFSM2 _ _ NullS _ _ = NullS
nextStateFSM2 _ _ _ NullS _ = NullS
nextStateFSM2 _ _ _ _ NullS = NullS
nextStateFSM2 (c1, c2) f ss as bs
  | c1 <= 0 || c2 <= 0 = error "nextStateFSM2: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM2 (c1, c2) f (tailS ss) (dropS c1 as) (dropS c2 bs)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        current_state = headS ss
        next_state = f current_state consumed_tokens_as consumed_tokens_bs


nextStateFSM3 :: (Int, Int, Int) -> (s -> [a] -> [b] -> [c] -> s)
              -> Signal s -> Signal a -> Signal b -> Signal c -> Signal s
nextStateFSM3 _ _ NullS _ _ _ = NullS
nextStateFSM3 _ _ _ NullS _ _ = NullS
nextStateFSM3 _ _ _ _ NullS _ = NullS
nextStateFSM3 _ _ _ _ _ NullS = NullS
nextStateFSM3 (c1, c2, c3) f ss as bs cs
  | c1 <= 0 || c2 <= 0 || c3 <= 0
    = error "nextStateFSM3: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM3 (c1, c2, c3) f (tailS ss)
                                        (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        current_state = headS ss
        next_state = f current_state consumed_tokens_as
                       consumed_tokens_bs consumed_tokens_cs


nextStateFSM4 :: (Int, Int, Int, Int) -> (s -> [a] -> [b] -> [c] -> [d] -> s)
              -> Signal s -> Signal a -> Signal b -> Signal c -> Signal d -> Signal s
nextStateFSM4 _ _ NullS _ _ _ _ = NullS
nextStateFSM4 _ _ _ NullS _ _ _ = NullS
nextStateFSM4 _ _ _ _ NullS _ _ = NullS
nextStateFSM4 _ _ _ _ _ NullS _ = NullS
nextStateFSM4 _ _ _ _ _ _ NullS = NullS
nextStateFSM4 (c1, c2, c3, c4) f ss as bs cs ds
  | c1 <= 0 || c2 <= 0 || c3 <= 0 || c4 <= 0
    = error "nextStateFSM4: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM4 (c1, c2, c3, c4) f (tailS ss)
                                        (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        current_state = headS ss
        next_state = f current_state consumed_tokens_as
                       consumed_tokens_bs consumed_tokens_cs consumed_tokens_ds


nextStateFSM5 :: (Int, Int, Int, Int, Int) -> (s -> [a] -> [b] -> [c] -> [d] -> [e] -> s)
              -> Signal s -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal s
nextStateFSM5 _ _ NullS _ _ _ _ _ = NullS
nextStateFSM5 _ _ _ NullS _ _ _ _ = NullS
nextStateFSM5 _ _ _ _ NullS _ _ _ = NullS
nextStateFSM5 _ _ _ _ _ NullS _ _ = NullS
nextStateFSM5 _ _ _ _ _ _ NullS _ = NullS
nextStateFSM5 _ _ _ _ _ _ _ NullS = NullS
nextStateFSM5 (c1, c2, c3, c4, c5) f ss as bs cs ds es
  | c1 <= 0 || c2 <= 0 || c3 <= 0 || c4 <= 0 || c5 <= 0
    = error "nextStateFSM4: Number of consumed tokens must be positive integer"
  | (not $ sufficient_tokens c1 as)
    || (not $ sufficient_tokens c2 bs)
    || (not $ sufficient_tokens c3 cs)
    || (not $ sufficient_tokens c4 ds)
    || (not $ sufficient_tokens c5 es) = NullS
  | otherwise = signal [next_state] +-+ nextStateFSM5 (c1, c2, c3, c4, c5) f (tailS ss)
                                        (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
                                        (dropS c4 ds) (dropS c5 es)
  where consumed_tokens_as = fromSignal $ takeS c1 as
        consumed_tokens_bs = fromSignal $ takeS c2 bs
        consumed_tokens_cs = fromSignal $ takeS c3 cs
        consumed_tokens_ds = fromSignal $ takeS c4 ds
        consumed_tokens_es = fromSignal $ takeS c5 es
        current_state = headS ss
        next_state = f current_state consumed_tokens_as
                       consumed_tokens_bs consumed_tokens_cs
                       consumed_tokens_ds consumed_tokens_es


outputFSM :: (s -> (Int, [a])) -> Signal s -> Signal a
outputFSM _ NullS = NullS
outputFSM g (s:-ss)
  | length y1 /= p = error "outputFSM: Incorrect number of produced tokens."
  | otherwise = signal y1 +-+ outputFSM g ss
  where (p, y1) = g s


outputFSM2 :: (s -> ((Int, Int), ([a], [b]))) -> Signal s -> (Signal a, Signal b)
outputFSM2 _ NullS = (NullS, NullS)
outputFSM2 g (s:-ss)
  | length y1 /= p1 || length y2 /= p2 = error "outputFSM2: Incorrect number of produced tokens."
  | otherwise = (signal y1 +-+ yr1, signal y2 +-+ yr2)
  where ((p1, p2), (y1, y2)) = g s
        (yr1, yr2) = outputFSM2 g ss


outputFSM3 :: (s -> ((Int, Int, Int), ([a], [b], [c])))
           -> Signal s -> (Signal a, Signal b, Signal c)
outputFSM3 _ NullS = (NullS, NullS, NullS)
outputFSM3 g (s:-ss)
  | length y1 /= p1
    || length y2 /= p2
    || length y3 /= p3 = error "outputFSM3: Incorrect number of produced tokens."
  | otherwise = (signal y1 +-+ yr1, signal y2 +-+ yr2, signal y3 +-+ yr3)
  where ((p1, p2, p3), (y1, y2, y3)) = g s
        (yr1, yr2, yr3) = outputFSM3 g ss


outputFSM4 :: (s -> ((Int, Int, Int, Int), ([a], [b], [c], [d])))
           -> Signal s -> (Signal a, Signal b, Signal c, Signal d)
outputFSM4 _ NullS = (NullS, NullS, NullS, NullS)
outputFSM4 g (s:-ss)
  | length y1 /= p1
    || length y2 /= p2
    || length y3 /= p3
    || length y4 /= p4 = error "outputFSM4: Incorrect number of produced tokens."
  | otherwise = (signal y1 +-+ yr1, signal y2 +-+ yr2, signal y3 +-+ yr3, signal y4 +-+ yr4)
  where ((p1, p2, p3, p4), (y1, y2, y3, y4)) = g s
        (yr1, yr2, yr3, yr4) = outputFSM4 g ss


outputFSM5 :: (s -> ((Int, Int, Int, Int, Int), ([a], [b], [c], [d], [e])))
           -> Signal s -> (Signal a, Signal b, Signal c, Signal d, Signal e)
outputFSM5 _ NullS = (NullS, NullS, NullS, NullS, NullS)
outputFSM5 g (s:-ss)
  | length y1 /= p1
    || length y2 /= p2 || length y3 /= p3
    || length y4 /= p4 || length y5 /= p5 = error "outputFSM5: Incorrect number of produced tokens."
  | otherwise = (signal y1 +-+ yr1, signal y2 +-+ yr2, signal y3 +-+ yr3,
                 signal y4 +-+ yr4, signal y5 +-+ yr5)
  where ((p1, p2, p3, p4, p5),(y1, y2, y3, y4, y5)) = g s
        (yr1, yr2, yr3, yr4, yr5) = outputFSM5 g ss


------------------------------------------------------------------------
--
-- Test of Library (not exported)
--
------------------------------------------------------------------------

{-

---------------------------------------------------------
-- test1: kernel22SADF test
---------------------------------------------------------

test1 :: Signal ((Int, Int), (Int, Int), [a] -> [b] -> ([c], [d]))
      -> Signal a -> Signal b -> (Signal c, Signal d)
test1 = kernel22SADF

ct = signal [((1,1), (1,1), \[a] [b] -> ([2*a], [2*b])),
             ((2,2), (1,1), \[a,b] [c,d] -> ([a+b], [c+d])),
             ((1,2), (2,1), \[a] [b,c] -> ([b,c], [a]))]

x = signal [1..20]
y = signal [21 .. 40]

test1out = test1 ct x y

---------------------------------------------------------
-- test2: Anti Wind-up system
---------------------------------------------------------

-- State transition function for the detector
f :: (Num a, Ord a) => Int -> [a] -> [a] -> Int
f 1 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0) then 2 else 1
f 2 [y] [v] = if (y > 100 && v > 0 || y < (-100) && v < 0) then 2 else 1

-- Output function for the detector
g :: Num a => Int -> ((Int, Int), ([((Int, Int), Int, [a] -> [a] -> [a])], [(Int, Int, [a] -> [a])]))
g 1 = ((1,1), ([((1,1), 1, \[a] [b] -> [a+b])], [(1, 1, \[a] -> [a])]))
g 2 = ((1,1), ([((0,1), 1, \_ [b] -> [b])], [(1, 0, \[a] -> [])]))

-- Detector
detector :: (Num a, Ord a) => Signal a -> Signal a
         -> (Signal ((Int, Int), Int, [a] -> [a] -> [a]), Signal (Int, Int, [a] -> [a]))
detector = detector22SADF (1,1) f g 1

syst :: (Num a, Ord a) => Signal a -> Signal a
syst input = output
  where output = integrator c1 s1 s3
        s3 = delaySADF [0] output
        s1 = kernel11SADF c2 input
        (c1, c2) = detector s3 input
        integrator = kernel21SADF


---------------------------------------------------------
-- test3: Register Bank (3 registers)
---------------------------------------------------------

-- Scenarios list
scenarios :: Int -> ((Int, Int, Int, Int), (Int, Int, Int, Int),
             [a] -> [a] -> [a] -> [a] -> ([a], [a], [a], [a]))
scenarios 0 = ((0,0,0,0), (0,0,0,0), \_ _ _ _ -> ([], [], [], []))
scenarios 1 = ((0,1,0,0), (1,1,0,0), \_ [r1] _ _ -> ([r1], [r1], [], []))
scenarios 2 = ((0,0,1,0), (1,0,1,0), \_ _ [r2] _ -> ([r2], [], [r2], []))
scenarios 3 = ((0,0,0,1), (1,0,0,1), \_ _ _ [r3] -> ([r3], [], [], [r3]))
scenarios 4 = ((1,1,0,0), (0,1,0,0), \[r1] _ _ _ -> ([], [r1], [], []))
scenarios 5 = ((1,0,1,0), (0,0,1,0), \[r2] _ _ _ -> ([], [], [r2], []))
scenarios 6 = ((1,0,0,1), (0,0,0,1), \[r3] _ _ _ -> ([], [], [], [r3]))
scenarios _ = error "scenarios: outside the state list"

switchState :: Int -> [String] -> Int
switchState _ ["sc0"] = 0     -- No operation (kernel inactive)
switchState _ ["lr1"] = 1     -- Load r1
switchState _ ["lr2"] = 2     -- Load r2
switchState _ ["lr3"] = 3     -- Load r3
switchState _ ["sr1"] = 4     -- Store r1
switchState _ ["sr2"] = 5     -- Store r2
switchState _ ["sr3"] = 6     -- Store r3
switchState _ _ = error "switchState: Input not recognized"

regDetector = detector11SADF 1 switchState (\e -> (1, [scenarios e])) 0
regKernel = kernel44SADF

registerBank inputControl inputData = output
  where ct = regDetector inputControl
        (output, r1, r2, r3) = regKernel ct inputData r1' r2' r3'
        r1' = delaySADF [0] r1
        r2' = delaySADF [0] r2
        r3' = delaySADF [0] r3

cInput = signal ["lr1","lr2","lr3","sr1","sr2","sr3","sc0","lr1","lr2","lr3","lr1","lr2","lr3"]
dInput = signal [1..10]

regOutput = registerBank cInput dInput
-- Expected output {0,0,0,1,2,3,1,2,3}

-}
