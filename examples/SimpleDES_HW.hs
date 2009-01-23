-- The module implements a simplified version of the DES
-- Encryption-Decryption Algorithm. The implementation is based on
-- Stallings: Cryptography and Network Security 4/e, Pearson Education,
-- 2006, Appendix C (available from http://www.box.net/shared/06vnp1hiu3)

module SimpleDES_HW where

import ForSyDe
import Data.Bits
import Data.Param.FSVec
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases

-- Calculation of subkey 1 and subkey 2
p10 :: FSVec D10 Bit -> FSVec D10 Bit
p10 key =    key!d2 +> key!d4 +> key!d1 +> key!d6 +> key!d5 
          +> key!d9 +> key!d0 +> key!d8 +> key!d7 +> key!d5 +> empty
                 
splitKey :: FSVec D10 Bit -> (FSVec D5 Bit, FSVec D5 Bit) 
splitKey key = (Data.Param.FSVec.take d5 key, Data.Param.FSVec.drop d5 key)


p8 :: FSVec D10 Bit -> FSVec D8 Bit
p8 key =    key!d5 +> key!d2 +> key!d6 +> key!d3 
         +> key!d7 +> key!d4 +> key!d9 +> key!d8 +> empty


subkey1 key = p8 (rotatedKey1 Data.Param.FSVec.++ rotatedKey2)
             where
               rotatedKey1 = rotr key1
               rotatedKey2 = rotr key2
               (key1, key2) = splitKey (p10 key)

subkey2 key = p8 (rotatedKey1 Data.Param.FSVec.++ rotatedKey2)
             where
               rotatedKey1 = (rotl . rotl) key1
               rotatedKey2 = (rotl . rotl) key2
               (key1, key2) = splitKey (p10 key)
                 
myKey = $(vectorTH [H :: Bit,L,H,L,L,L,L,L,H,L])

-- S-DES Encryption


-- Hardware Component 'ipSys' (Initial Permutation)

ip :: FSVec D8 Bit -> FSVec D8 Bit
ip block =    block!d1 +> block!d5 +> block!d2 +> block!d0
           +> block!d3 +> block!d7 +> block!d4 +> block!d6 +> empty

ipFun :: ProcFun (FSVec D8 Bit -> FSVec D8 Bit)
ipFun = $(newProcFun [d| ip :: FSVec D8 Bit -> FSVec D8 Bit
                         ip block =    block!d1 +> block!d5 
                                    +> block!d2 +> block!d0
                                    +> block!d3 +> block!d7 
                                    +> block!d4 +> block!d6 
                                    +> empty |])

ipProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
ipProc = mapSY "ipProc" ipFun

ipSys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit))
ipSys = newSysDef ipProc "ip" ["in"] ["out"]

ipVHDL = writeVHDL ipSys


-- Hardware Component 'ip_barSys' (Final Permutation)

ip_bar :: FSVec D8 Bit -> FSVec D8 Bit
ip_bar block =    block!d3 +> block!d0 +> block!d2 +> block!d4
               +> block!d6 +> block!d1 +> block!d7 +> block!d5 +> empty

ip_barFun :: ProcFun (FSVec D8 Bit -> FSVec D8 Bit)
ip_barFun = $(newProcFun [d| ip_bar :: FSVec D8 Bit -> FSVec D8 Bit
                             ip_bar block =   block!d3 +> block!d0 
                                           +> block!d2 +> block!d4
                                           +> block!d6 +> block!d1 
                                           +> block!d7 +> block!d5 
                                           +> empty |])

ip_barProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
ip_barProc = mapSY "ip_bar_Proc" ip_barFun

ip_barSys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit))
ip_barSys = newSysDef ip_barProc "ip_bar" ["in"] ["out"]

ip_barVHDL = writeVHDL ip_barSys



-- Hardware Component 'f'  

-- Expansion/Permutation

f_mapping :: FSVec D4 Bit -> FSVec D8 Bit
f_mapping nibble =   nibble!d3 +> nibble!d0 +> nibble!d1 +> nibble!d2
                  +> nibble!d1 +> nibble!d2 +> nibble!d3 +> nibble!d0 +> empty

expPermFun :: ProcFun (FSVec D4 Bit -> FSVec D8 Bit)
expPermFun = $(newProcFun [d| f_mapping :: FSVec D4 Bit -> FSVec D8 Bit
                              f_mapping nibble =   nibble!d3 +>
                                                   nibble!d0 
                                                +> nibble!d1 +>
                                                   nibble!d2
                                                +> nibble!d1 +> 
                                                   nibble!d2 
                                                +> nibble!d3 +> 
                                                   nibble!d0 +>
                                                   empty |])

expPermProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit)
expPermProc = mapSY "expPermProc" expPermFun

expPermSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit))
expPermSys = newSysDef expPermProc "expPerm" ["in"] ["out"] 

-- xor with subkey

f_xor :: FSVec D8 Bit -> FSVec D4 Bit -> FSVec D8 Bit
f_xor key nibble = Data.Param.FSVec.zipWith xor key (f_mapping nibble)

xorSubkeyFun :: ProcFun (FSVec D8 Bit -> FSVec D8 Bit -> FSVec D8 Bit)
xorSubkeyFun = $(newProcFun [d| f_xor :: FSVec D8 Bit -> FSVec D8 Bit  
                                      -> FSVec D8 Bit
                                f_xor key input 
                                      -- = Data.Param.FSVec.zipWith xor key input
                                      =   (xor (key!d0) (input!d0)) 
                                       +> (xor (key!d1) (input!d1)) 
                                       +> (xor (key!d2) (input!d2)) 
                                       +> (xor (key!d3) (input!d3)) 
                                       +> (xor (key!d4) (input!d4)) 
                                       +> (xor (key!d5) (input!d5)) 
                                       +> (xor (key!d6) (input!d6)) 
                                       +> (xor (key!d7) (input!d7)) 
                                       +> empty

                                           |])

xorSubkeyProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit) 
              -> Signal (FSVec D8 Bit) 
xorSubkeyProc = zipWithSY "xorSubKeyProc" xorSubkeyFun

xorSubkeySys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit) 
                        -> Signal (FSVec D8 Bit))
xorSubkeySys = newSysDef xorSubkeyProc "xorSubKey" ["key", "input"]
               ["out"]

-- splitByte
 
splitBlock :: FSVec D8 Bit -> (FSVec D4 Bit, FSVec D4 Bit)
splitBlock block = (Data.Param.FSVec.take d4 block, 
                    Data.Param.FSVec.drop d4 block)

splitByteFun :: ProcFun (FSVec D8 Bit -> (FSVec D4 Bit, FSVec D4 Bit))
splitByteFun = $(newProcFun [d| splitByte :: FSVec D8 Bit 
                                          -> (FSVec D4 Bit, FSVec D4
                                                    Bit) 
                                splitByte block = (Data.Param.FSVec.take
                                                       d4 block,
                                                   Data.Param.FSVec.drop
                                                       d4 block) 
                              |])

splitByteProc :: Signal (FSVec D8 Bit) 
              -> (Signal (FSVec D4 Bit, FSVec D4 Bit))
splitByteProc = mapSY "splitByteProc" splitByteFun

splitByteSys :: SysDef (Signal (FSVec D8 Bit) 
                        -> (Signal (FSVec D4 Bit, FSVec D4 Bit)))
splitByteSys = newSysDef splitByteProc "splitByte" ["byte"]
               ["pair_nibble"]
splitByteVHDL = writeVHDL splitByteSys

-- Indexes for Matrix S0 and S1

rowS0 :: FSVec D8 Bit -> FSVec D2 Bit
rowS0 pmatrix = pmatrix!d0 +> pmatrix!d3 +> empty

rowS0Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
rowS0Fun = $(newProcFun [d| rowS0 :: FSVec D8 Bit -> FSVec D2 Bit
                            rowS0 pmatrix = pmatrix!d0 +> pmatrix!d3 +>
                                            empty 
                                                |])

rowS0Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
rowS0Proc = mapSY "rowS0Proc" rowS0Fun

rowS0Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
rowS0Sys = newSysDef rowS0Proc "rowS0" ["pmatrix"] ["rowS0"]


colS0 :: FSVec D8 Bit -> FSVec D2 Bit
colS0 pmatrix = pmatrix!d1 +> pmatrix!d2 +> empty

colS0Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
colS0Fun = $(newProcFun [d| colS0 :: FSVec D8 Bit -> FSVec D2 Bit
                            colS0 pmatrix = pmatrix!d1 +> pmatrix!d2 +> empty
                                                |])

colS0Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
colS0Proc = mapSY "colS0Proc" colS0Fun

colS0Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
colS0Sys = newSysDef colS0Proc "colS0" ["pmatrix"] ["colS0"]
 

rowS1 :: FSVec D8 Bit -> FSVec D2 Bit
rowS1 pmatrix = pmatrix!d4 +> pmatrix!d7 +> empty

rowS1Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
rowS1Fun = $(newProcFun [d| rowS1 :: FSVec D8 Bit -> FSVec D2 Bit
                            rowS1 pmatrix = pmatrix!d4 +> pmatrix!d7 +>
                                            empty 
                                                |])

rowS1Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
rowS1Proc = mapSY "rowS1Proc" rowS1Fun

rowS1Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
rowS1Sys = newSysDef rowS1Proc "rowS1" ["pmatrix"] ["rowS1"]



colS1 :: FSVec D8 Bit -> FSVec D2 Bit
colS1 pmatrix = pmatrix!d5 +> pmatrix!d6 +> empty

colS1Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
colS1Fun = $(newProcFun [d| colS1 :: FSVec D8 Bit -> FSVec D2 Bit
                            colS1 pmatrix =  pmatrix!d5 +> pmatrix!d6 +> empty
                                                |])

colS1Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
colS1Proc = mapSY "colS1Proc" colS1Fun

colS1Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
colS1Sys = newSysDef colS1Proc "colS1" ["pmatrix"] ["colS1"]


-- Output S0

outputS0Fun :: ProcFun (FSVec D2 Bit -> FSVec D2 Bit -> FSVec D2 Bit)
outputS0Fun 
   = $(newProcFun [d| accessS0 :: FSVec D2 Bit -> FSVec D2 Bit 
                               -> FSVec D2 Bit
                      accessS0 row col 
                          = -- Row 0 = 1, 0, 3, 2
                            if row == (L +> L +> empty) then
                              if col == (L +> L +> empty) then 
                                (L +> H +> empty)
                              else 
                                if col == (L +> H +> empty) then 
                                  (L +> L +> empty)
                                else
                                  if col == (H +> L +> empty) then 
                                    (H +> H +> empty)
                                  else
                                    (H +> L +> empty)
                            else
                              -- Row 1 = 3, 2, 1, 0  
                              if row == (L +> L +> empty) then
                                if col == (L +> L +> empty) then 
                                  (H +> H +> empty)
                                else 
                                  if col == (L +> H +> empty) then 
                                    (H +> L +> empty)
                                  else
                                    if col == (H +> L +> empty) then 
                                      (L +> H +> empty)
                                    else
                                      (L +> L +> empty)
                              else
                                -- Row 2 = 0, 2, 1, 3
                                if row == (L +> L +> empty) then
                                  if col == (L +> L +> empty) then 
                                    (L +> L +> empty)
                                  else 
                                    if col == (L +> H +> empty) then 
                                      (H +> L +> empty)
                                    else
                                      if col == (H +> L +> empty) then 
                                        (L +> H +> empty)
                                      else
                                        (H +> H +> empty)
                                else
                                  -- Row 3 = 3, 1, 3, 2
                                  if col == (L +> L +> empty) then 
                                    (H +> H +> empty)
                                  else 
                                    if col == (L +> H +> empty) then 
                                      (L +> H +> empty)
                                    else
                                      if col == (H +> L +> empty) then 
                                        (H +> H +> empty)
                                      else
                                        (H +> L +> empty)
                                            |])

outputS0Proc :: Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
             -> Signal (FSVec D2 Bit)
outputS0Proc = zipWithSY "outputS0Proc" outputS0Fun

outputS0Sys :: SysDef ( Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
                        -> Signal (FSVec D2 Bit))
outputS0Sys = newSysDef outputS0Proc "outputS0" ["row", "col"] ["out"]

outputS0VHDL = writeVHDL outputS0Sys


-- Output S1

outputS1Fun :: ProcFun (FSVec D2 Bit -> FSVec D2 Bit -> FSVec D2 Bit)
outputS1Fun 
   = $(newProcFun [d| accessS1 :: FSVec D2 Bit -> FSVec D2 Bit 
                               -> FSVec D2 Bit
                      accessS1 row col 
                          = -- Row 0 = 0, 1, 2, 3
                            if row == (L +> L +> empty) then
                              if col == (L +> L +> empty) then 
                                (L +> L +> empty)
                              else 
                                if col == (L +> H +> empty) then 
                                  (L +> H +> empty)
                                else
                                  if col == (H +> L +> empty) then 
                                    (H +> L +> empty)
                                  else
                                    (H +> H +> empty)
                            else
                              -- Row 1 = 2, 0, 1, 3  
                              if row == (L +> L +> empty) then
                                if col == (L +> L +> empty) then 
                                  (H +> L +> empty)
                                else 
                                  if col == (L +> H +> empty) then 
                                    (L +> L +> empty)
                                  else
                                    if col == (H +> L +> empty) then 
                                      (L +> H +> empty)
                                    else
                                      (H +> H +> empty)
                              else
                                -- Row 2 = 3, 0, 1, 0
                                if row == (L +> L +> empty) then
                                  if col == (L +> L +> empty) then 
                                    (H +> H +> empty)
                                  else 
                                    if col == (L +> H +> empty) then 
                                      (L +> L +> empty)
                                    else
                                      if col == (H +> L +> empty) then 
                                        (L +> H +> empty)
                                      else
                                        (L +> L +> empty)
                                else
                                  -- Row 3 = 2, 1, 0, 3
                                  if col == (L +> L +> empty) then 
                                    (H +> L +> empty)
                                  else 
                                    if col == (L +> H +> empty) then 
                                      (L +> H +> empty)
                                    else
                                      if col == (H +> L +> empty) then 
                                        (L +> L +> empty)
                                      else
                                        (H +> H +> empty)
                                            |])

outputS1Proc :: Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
             -> Signal (FSVec D2 Bit)
outputS1Proc = zipWithSY "outputS1Proc" outputS1Fun

outputS1Sys :: SysDef ( Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
                        -> Signal (FSVec D2 Bit))
outputS1Sys = newSysDef outputS1Proc "outputS1" ["row", "col"] ["out"]

outputS1VHDL = writeVHDL outputS1Sys


-- Permute output of S0 and S1
p4 :: FSVec D4 Bit -> FSVec D4 Bit
p4 byte = byte!d1 +> byte!d3 +> byte!d2 +> byte!d0 +> empty

p4Fun :: ProcFun (FSVec D2 Bit -> FSVec D2 Bit -> FSVec D4 Bit)
p4Fun = $(newProcFun [d| p4 :: FSVec D2 Bit -> FSVec D2 Bit -> FSVec D4 Bit
                         p4 outS0 outS1 
                            =    outS0!d1 +> outS1!d1 
                              +> outS1!d0 +> outS0!d0
                              +> empty
                                  |])

p4Proc :: Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit)
p4Proc = zipWithSY "p4Proc" p4Fun

p4Sys :: SysDef (Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) -> Signal (FSVec D4 Bit))
p4Sys = newSysDef p4Proc "p4" ["S0", "S1"] ["out"]


-- Concat Nibbles
xorNibblesFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> FSVec D4 Bit)
xorNibblesFun = $(newProcFun [d| xorNibbles :: FSVec D4 Bit -> FSVec D4 Bit 
                                         -> FSVec D4 Bit
                                 xorNibbles left right 
--                                   = Data.Param.FSVec.zipWith xor left right
                                     =    (xor (left!d0) (right!d0)) 
                                       +> (xor (left!d1) (right!d1))
                                       +> (xor (left!d2) (right!d2))
                                       +> (xor (left!d3) (right!d3))
                                       +> empty
                                   |])

xorNibblesProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit)
xorNibblesProc = zipWithSY "xorNibblesProc" xorNibblesFun      

xorNibblesSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit))
xorNibblesSys = newSysDef xorNibblesProc "xorNibbles" ["n1", "n2"] ["out"]


-- Switch
switch :: FSVec D8 Bit -> FSVec D8 Bit
switch nibble =  Data.Param.FSVec.drop d4 nibble 
                 Data.Param.FSVec.++ 
                 Data.Param.FSVec.take d4 nibble

switchFun :: ProcFun (FSVec D8 Bit -> FSVec D8 Bit)
switchFun = $(newProcFun [d| switch :: FSVec D8 Bit -> FSVec D8 Bit
                             switch nibble =  Data.Param.FSVec.drop d4 nibble 
                                              Data.Param.FSVec.++ 
                                              Data.Param.FSVec.take d4 nibble
                                                  |])


switchProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
switchProc = mapSY "switchProc" switchFun

switchSys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit))
switchSys = newSysDef switchProc "switch" ["in"] ["out"]






fProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit) -> Signal (FSVec D4 Bit)
fProc nibble subkey = out
               where 
                  out = (instantiate "p4Sys" p4Sys) s0 s1
                  s0 = (instantiate "outputS0Sys" outputS0Sys) rS0 cS0
                  s1 = (instantiate "outputS1Sys" outputS1Sys) rS1 cS1
                  rS0 = (instantiate "rowS0" rowS0Sys) xorsubkey
                  rS1 = (instantiate "rowS1" rowS1Sys) xorsubkey
                  cS0 = (instantiate "colS0" colS0Sys) xorsubkey          
                  cS1 = (instantiate "colS1" colS1Sys) xorsubkey
                  xorsubkey = (instantiate "xorsubkeySys" xorSubkeySys) subkey expperm
                  expperm = (instantiate "expPermSys" expPermSys) nibble

fSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit) -> Signal (FSVec D4 Bit)) 
fSys = newSysDef fProc "fSys" ["nibble", "subkey"] ["out"]

outputS0Fun' :: ProcFun (FSVec D2 Bit -> FSVec D2 Bit -> FSVec D2 Bit)
outputS0Fun' 
   = $(newProcFun [d| accessS0 :: FSVec D2 Bit -> FSVec D2 Bit 
                               -> FSVec D2 Bit
                      accessS0 row col 
                          = if 
                              row == (L +> L +> empty)
                            then
                              (L +> L +> empty)
                            else
                              (s0!d0)!d0
                            where
                                s0 :: FSVec D4 (FSVec D4 (FSVec D2 Bit))
                                s0 =    -- Row 0 = 1, 0, 3, 2
                                       ((L +> H +> empty) +> (L +> L +> empty) 
                                         +> (H +> H +> empty) +> (H +> L +> empty) 
                                         +> empty)
                                     +> 
                                        -- Row 1 = 3, 2, 1, 0
                                        ((H +> H +> empty) +> (H +> L +> empty) 
                                         +> (L +> H +> empty) +> (L +> L +> empty) 
                                         +> empty)
                                     +> 
                                        -- Row 2 = 0, 2, 1, 3
                                        ((L +> L +> empty) +> (H +> L +> empty) 
                                         +> (L +> H +> empty) +> (H +> H +> empty) 
                                         +> empty) 
                                     +> 
                                        -- Row 3 = 3, 1, 3, 2
                                        ((H +> H +> empty) +> (L +> H +> empty) 
                                         +> (H +> H +> empty) +> (H +> L +> empty) 
                                         +> empty)
                                     +>
                                        empty
                                             |])

outputS0Proc' :: Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
             -> Signal (FSVec D2 Bit)
outputS0Proc' = zipWithSY "outputS0Proc" outputS0Fun'

outputS0Sys' :: SysDef ( Signal (FSVec D2 Bit) -> Signal (FSVec D2 Bit) 
                        -> Signal (FSVec D2 Bit))
outputS0Sys' = newSysDef outputS0Proc' "outputS0" ["row", "col"] ["out"]

outputS0VHDL' = writeVHDL outputS0Sys'


access :: FSVec D4 (FSVec D4 (FSVec D2 Bit)) -> 
          FSVec D2 Bit -> FSVec D2 Bit -> FSVec D2 Bit
access matrix row col = if row == (L +> L +> empty) then
                           if col == (L +> L +> empty) then
                              (matrix!d0)!d0
                           else
                              if col == (L +> H +> empty) then
                                 (matrix!d0)!d1
                              else
                                 if col == (H +> L +> empty) then
                                    (matrix!d0)!d2
                                 else
                                    (matrix!d0)!d3
                        else 
                           if row == (L +> H +> empty) then
                              if col == (L +> L +> empty) then
                                 (matrix!d1)!d0
                              else
                                 if col == (L +> H +> empty) then
                                    (matrix!d1)!d1
                                 else
                                    if col == (H +> L +> empty) then
                                       (matrix!d1)!d2
                                    else
                                       (matrix!d1)!d3
                            else
                               if row == (H +> L +> empty) then
                                  if col == (L +> L +> empty) then
                                     (matrix!d2)!d0
                                  else
                                     if col == (L +> H +> empty) then
                                        (matrix!d2)!d1
                                     else
                                        if col == (H +> L +> empty) then
                                           (matrix!d2)!d2
                                        else
                                           (matrix!d2)!d3
                               else
                                  if col == (L +> L +> empty) then
                                     (matrix!d3)!d0
                                  else
                                     if col == (L +> H +> empty) then
                                        (matrix!d3)!d1
                                     else
                                        if col == (H +> L +> empty) then
                                           (matrix!d3)!d2
                                        else
                                           (matrix!d3)!d3




s0 :: FSVec D4 (FSVec D4 (FSVec D2 Bit))
s0 =    -- Row 0 = 1, 0, 3, 2
        ((L +> H +> empty) +> (L +> L +> empty) +> 
         (H +> H +> empty) +> (H +> L +> empty) +> empty)
     +> 
        -- Row 1 = 3, 2, 1, 0
        ((H +> H +> empty) +> (H +> L +> empty) +> 
         (L +> H +> empty) +> (L +> L +> empty) +> empty)
     +> 
        -- Row 2 = 0, 2, 1, 3
        ((L +> L +> empty) +> (H +> L +> empty) +> 
         (L +> H +> empty) +> (H +> H +> empty) +> empty)
     +> 
        -- Row 3 = 3, 1, 3, 2
        ((H +> H +> empty) +> (L +> H +> empty) +> 
         (H +> H +> empty) +> (H +> L +> empty) +> empty)
     +>
        empty
  
s1 :: FSVec D4 (FSVec D4 (FSVec D2 Bit))
s1 =    -- Row 0 = 0, 1, 2, 3
        ((L +> L +> empty) +> (L +> H +> empty) +> 
         (H +> L +> empty) +> (H +> H +> empty) +> empty)
     +> 
        -- Row 1 = 2, 0, 1, 3
        ((H +> L +> empty) +> (L +> L +> empty) +> 
         (L +> H +> empty) +> (H +> H +> empty) +> empty)
     +> 
        -- Row 2 = 3, 0, 1, 0
        ((H +> H +> empty) +> (L +> L +> empty) +> 
         (L +> H +> empty) +> (L +> L +> empty) +> empty)
     +> 
        -- Row 3 = 2, 1, 0, 3
        ((H +> L +> empty) +> (L +> H +> empty) +> 
         (L +> L +> empty) +> (H +> H +> empty) +> empty)
     +>
        empty





outputS0 :: FSVec D8 Bit -> FSVec D2 Bit
outputS0 pmatrix = access s0 row col
                          where 
                             row = rowS0 pmatrix
                             col = colS0 pmatrix

outputS1 :: FSVec D8 Bit -> FSVec D2 Bit
outputS1 pmatrix = access s1 row col
                          where 
                             row = rowS1 pmatrix
                             col = colS1 pmatrix

output :: FSVec D8 Bit -> FSVec D4 Bit
output pmatrix = p4 $ outS0 Data.Param.FSVec.++ outS1
                 where outS0 = outputS0 pmatrix
                       outS1 = outputS1 pmatrix

f_k :: FSVec D8 Bit -> FSVec D8 Bit -> FSVec D8 Bit
f_k subkey input = left Data.Param.FSVec.++ right 
                   where right = Data.Param.FSVec.drop d4 input
                         left' = Data.Param.FSVec.take d4 input 
                         left = Data.Param.FSVec.zipWith xor left' (output pmatrix)
                         pmatrix = f_xor subkey right



encrypt key plaintext = id 
                        $ ip_bar 
                        $ f_k (subkey_2) 
                        $ switch 
                        $ f_k (subkey_1) 
                        $ ip 
                        plaintext
                        where 
                           subkey_1 = subkey1 key 
                           subkey_2 = subkey2 key

decrypt key ciphertext = id
                         $ ip_bar 
                         $ f_k (subkey_1) 
                         $ switch 
                         $ f_k (subkey_2) 
                         $ ip 
                         ciphertext
                         where 
                            subkey_1 = subkey1 key 
                            subkey_2 = subkey2 key

-- Test Data 
plain1 = L +> L +> H +> L +> 
         L +> H +> L +> L +> empty

plain2 = H +> L +> H +> L +> 
         H +> H +> H +> H +> empty

enc1 = encrypt myKey plain1
dec1 = decrypt myKey enc1

enc2 = encrypt myKey plain2
dec2 = decrypt myKey enc2



