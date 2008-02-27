{-# OPTIONS_GHC -fth #-} -- The use of -fth is due to splicing mkHDFun
module Plus1 where
-- A simple addaing system 

import HD
import SynchronousLib (mapSY)

-- hdPlus1 input port
plus1In :: InPort
plus1In = mkInPort 1 [("plus1Input", Int)]

-- hdPlus1 output port
plus1Out :: OutPort
plus1Out = mkOutPort 1 [("plus1Output", Int)]


hdPlus1 :: HDSignal Int -> HDSignal Int 
hdPlus1 = mapSY doPlus1
 where doPlus1 = $(mkHDFun [d| doPlus1 :: Int -> Int
                               doPlus1 a = a + 1          |])


plus1Circ :: InPort -> OutPort
plus1Circ ip = supplySig  plus1Sig "plus1Output" plus1Out
  where plus1Sig = plugSig "plus1Input" ip hdPlus1

plus1Block :: Block
plus1Block = mkBlock "plus1_block" plus1In plus1Circ