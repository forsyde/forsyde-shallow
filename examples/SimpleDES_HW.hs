-- The module implements a simplified version of the DES
-- Encryption-Decryption Algorithm. The implementation is based on
-- Stallings: Cryptography and Network Security 4/e, Pearson Education,
-- 2006, Appendix C (available from http://www.box.net/shared/06vnp1hiu3)

module SimpleDES_HW where

import ForSyDe
import ForSyDe.Bit
import Data.Bits
import Data.Int
import Data.Param.FSVec 
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Aliases
import qualified Data.Param.FSVec as FSVec
-- System Definition: subkeysSys
--
-- Generation of Subkey 1 (8 bit) and Subkey 2 (8 bit) from an initial
-- Key (10 bit)

subkeysFun :: ProcFun (FSVec D10 Bit -> (FSVec D8 Bit, FSVec D8 Bit))
subkeysFun
   = $(newProcFun [d| subkeys :: FSVec D10 Bit -> (FSVec D8 Bit, FSVec D8 Bit) 
                      subkeys key = (subkey1, subkey2)
                         where 
                           -- Subkey 1
                           subkey2 :: FSVec D8 Bit                             
                           subkey2 = p8 ls3Left ls3Right
                           
                           -- Subkey 1          
                           subkey1 :: FSVec D8 Bit                              
                           subkey1 = p8 ls1Left ls1Right
                           
                           -- Shifted Values - Akward implementation            
                           ls3Right :: FSVec D5 Bit          
                           ls3Right = shiftOneLeft ls2Right          
                           ls3Left :: FSVec D5 Bit
                           ls3Left = shiftOneLeft ls2Left   
                           ls2Right :: FSVec D5 Bit          
                           ls2Right = shiftOneLeft ls1Right          
                           ls2Left :: FSVec D5 Bit
                           ls2Left = shiftOneLeft ls1Left          
                           ls1Right :: FSVec D5 Bit          
                           ls1Right = shiftOneLeft p10Right          
                           ls1Left :: FSVec D5 Bit
                           ls1Left = shiftOneLeft p10Left

                           -- Outputs of P10          
                           p10Left :: FSVec D5 Bit          
                           p10Left = fst5 p10Out
                           p10Right :: FSVec D5 Bit          
                           p10Right = snd5 p10Out          
                           p10Out :: (FSVec D5 Bit, FSVec D5 Bit)
                           p10Out = p10 key 

                           -- P8: Permutation and reduction to 8 Bits
                           p8 :: FSVec D5 Bit -> FSVec D5 Bit
                              -> FSVec D8 Bit  
                           p8 vec1 vec2 =   vec2!d0 +> vec1!d2 +> vec2!d1 
                                         +> vec1!d3 +> vec2!d2 +> vec1!d4 
                                         +> vec2!d4 +> vec2!d3 +> empty

                           -- Shifts one bit to the left (which means
                           -- from d1 to d0, which in fact is 'rotr') 
                           shiftOneLeft :: FSVec D5 Bit -> FSVec D5 Bit
                           shiftOneLeft vec = rotr vec 

                           -- P10: Permutation
                           p10 :: FSVec D10 Bit 
                               -> (FSVec D5 Bit, FSVec D5 Bit)
                           p10 key = (   key!d2 +> key!d4 +> key!d1 
                                      +> key!d6 +> key!d5 +> empty,
                                         key!d9 +> key!d0 +> key!d8 
                                      +> key!d7 +> key!d5 +> empty )
                           fst5 :: (FSVec D5 Bit, FSVec D5 Bit) -> FSVec D5 Bit
                           fst5 (a, b) = a
                           snd5 :: (FSVec D5 Bit, FSVec D5 Bit) -> FSVec D5 Bit
                           snd5 (a, b) = b                           
                    |])


subkeysProc :: Signal (FSVec D10 Bit) -> (Signal (FSVec D8 Bit), Signal (FSVec D8 Bit))
subkeysProc = unzipSY "unzipKeys" . mapSY "subKeys" subkeysFun
    
subkeysSys :: SysDef (Signal (FSVec D10 Bit) -> (Signal (FSVec D8 Bit), Signal (FSVec D8 Bit)))
subkeysSys = newSysDef subkeysProc "genSubkeys" ["key"] ["subkey1", "subkey2"]
           


-- System Definition: ipSys
--
-- Initial permutation of plaintext

ipFun :: ProcFun (FSVec D8 Bit -> (FSVec D4 Bit, FSVec D4 Bit))
ipFun = $(newProcFun [d| ip :: FSVec D8 Bit -> (FSVec D4 Bit, FSVec D4 Bit)
                         ip block = (   block!d1 +> block!d5 
                                     +> block!d2 +> block!d0
                                     +> empty,
                                        block!d3 +> block!d7 
                                     +> block!d4 +> block!d6 
                                     +> empty) |])

ipProc :: Signal (FSVec D8 Bit) 
       -> (Signal (FSVec D4 Bit), Signal (FSVec D4 Bit))
ipProc = unzipSY "unzipSY" . mapSY "ipProc" ipFun

ipSys :: SysDef (Signal (FSVec D8 Bit) 
                 -> (Signal (FSVec D4 Bit), Signal (FSVec D4 Bit)))
ipSys = newSysDef ipProc "ip" ["in"] ["ipLeft", "ipRight"]

ipVHDL = writeVHDL ipSys



-- System Definition: ipBarSys
-- 
-- Inverse of initial permutation

ipBarFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> FSVec D8 Bit)
ipBarFun = $(newProcFun [d| ipBar :: FSVec D4 Bit -> FSVec D4 Bit -> FSVec D8 Bit
                            ipBar left right 
                                =    left!d3 +> left!d0 
                                  +> left!d2 +> right!d0
                                  +> right!d2 +> left!d1 
                                  +> right!d3 +> right!d1 
                                  +> empty |])

ipBarProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit)
ipBarProc = zipWithSY "ipBar_Proc" ipBarFun

ipBarSys :: SysDef ( Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) -> Signal (FSVec D8 Bit))
ipBarSys = newSysDef ipBarProc "ipBar" ["left", "right"] ["out"]

ipBarVHDL = writeVHDL ipBarSys



-- System Definition: f
--
-- 'f' consists of several blocks that achieve a combination of
-- permutation and substitution

-- System Definition: expPermSys
-- 
-- A 4-bit input is extended to 8 bit and permutated

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



-- System Definition: xorSybkeySys
--
-- xor operation of subkey and 8-bit value
 
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

{-
xorSubkeyProc' :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit) 
               -> (Signal (FSVec D4 Bit, FSVec D4 Bit))
xorSubkeyProc' keySignal permSignal = mapSY "splitByteProc" splitByteFun (zipWithSY "xorSubKeyProc" xorSubkeyFun keySignal permSignal) 

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
-}

-- System Definition: rowS0Sys
--
-- Gives indexes for row for S0 matrix

rowS0Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
rowS0Fun = $(newProcFun [d| rowS0 :: FSVec D8 Bit -> FSVec D2 Bit
                            rowS0 pmatrix = pmatrix!d0 +> pmatrix!d3 +>
                                            empty 
                                                |])

rowS0Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
rowS0Proc = mapSY "rowS0Proc" rowS0Fun

rowS0Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
rowS0Sys = newSysDef rowS0Proc "rowS0" ["pmatrix"] ["rowS0"]

-- System Definition: colS0Sys
--
-- Gives indexes for column for S0 matrix

colS0Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
colS0Fun = $(newProcFun [d| colS0 :: FSVec D8 Bit -> FSVec D2 Bit
                            colS0 pmatrix = pmatrix!d1 +> pmatrix!d2 +> empty
                                                |])

colS0Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
colS0Proc = mapSY "colS0Proc" colS0Fun

colS0Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
colS0Sys = newSysDef colS0Proc "colS0" ["pmatrix"] ["colS0"]
 

-- System Definition: rowS1Sys
--
-- Gives indexes for column for S1 matrix

rowS1Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
rowS1Fun = $(newProcFun [d| rowS1 :: FSVec D8 Bit -> FSVec D2 Bit
                            rowS1 pmatrix = pmatrix!d4 +> pmatrix!d7 +>
                                            empty 
                                                |])

rowS1Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
rowS1Proc = mapSY "rowS1Proc" rowS1Fun

rowS1Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
rowS1Sys = newSysDef rowS1Proc "rowS1" ["pmatrix"] ["rowS1"]


-- System Definition: colS1Sys
--
-- Gives indexes for column for S1 matrix

colS1Fun :: ProcFun (FSVec D8 Bit -> FSVec D2 Bit)
colS1Fun = $(newProcFun [d| colS1 :: FSVec D8 Bit -> FSVec D2 Bit
                            colS1 pmatrix =  pmatrix!d5 +> pmatrix!d6 +> empty
                                                |])

colS1Proc :: Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit)
colS1Proc = mapSY "colS1Proc" colS1Fun

colS1Sys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D2 Bit))
colS1Sys = newSysDef colS1Proc "colS1" ["pmatrix"] ["colS1"]


-- System Definition: outputS0Sys
--
-- Gives values for matrix S0 based on row and column index

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
                              if row == (L +> H +> empty) then
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
                                if row == (H +> L +> empty) then
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


-- System Definition: outputS1Sys
--
-- Gives values for matrix S1 based on row and column index

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
                              if row == (L +> H +> empty) then
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
                                if row == (H +> L +> empty) then
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


-- System Definition: p4Sys
--
-- Permute output of S0 and S1

p4Fun :: ProcFun (FSVec D2 Bit -> FSVec D2 Bit 
                  -> FSVec D4 Bit)
p4Fun = $(newProcFun 
            [d| p4 :: FSVec D2 Bit -> FSVec D2 Bit 
                   -> FSVec D4 Bit
                p4 outS0 outS1 
                   =    outS0!d1 +> outS1!d1 
                     +> outS1!d0 +> outS0!d0
                     +> empty |])

p4Proc :: Signal (FSVec D2 Bit) 
       -> Signal (FSVec D2 Bit) 
       -> Signal (FSVec D4 Bit)
p4Proc = zipWithSY "p4Proc" p4Fun

p4Sys :: SysDef (Signal (FSVec D2 Bit) 
      -> Signal (FSVec D2 Bit) 
      -> Signal (FSVec D4 Bit))
p4Sys = newSysDef p4Proc "p4" ["S0", "S1"] ["out"]


-- System Definition: xorNibblesSys
--
-- xor operation on two nibbles

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

-- System Definition: switchSys
--
-- The switch exchanges the left and right nibbles

switchFun :: ProcFun (FSVec D4 Bit -> FSVec D4 Bit -> (FSVec D4 Bit, FSVec D4 Bit))
switchFun = $(newProcFun [d| switch :: FSVec D4 Bit -> FSVec D4 Bit 
                                    -> (FSVec D4 Bit, FSVec D4 Bit)
                             switch left right 
                                 = (right, left)
                            |])

switchProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
           -> (Signal (FSVec D4 Bit), Signal (FSVec D4 Bit))
switchProc left right = unzipSY "unzipSwitch" (zipWithSY "switchProc" switchFun left right)

switchSys :: SysDef (Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
                     -> (Signal (FSVec D4 Bit), Signal (FSVec D4 Bit)))
switchSys = newSysDef switchProc "switch" ["leftIn", "leftRight"] ["leftOut", "rightOut"]


-- System Definition: fSys
-- 
-- fSys changes nibbles using both permutation and substitution based
-- on the value of a subkey

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


-- System Definition: fkSys
--
-- fkSys changes left and right nibble of a byte using permutation and
-- substitution based on the value of a subkey 

fkProc :: Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
       -> Signal (FSVec D8 Bit)
       -> (Signal (FSVec D4 Bit), Signal(FSVec D4 Bit))
fkProc ipLeft ipRight subkey = (outLeft, outRight)
   where
      outLeft = (instantiate "xorNibblesSys" xorNibblesSys) ipLeft fSysOut
      fSysOut = (instantiate "fSys" fSys) ipRight subkey
      outRight = ipRight          

fkSys :: SysDef (   Signal (FSVec D4 Bit) -> Signal (FSVec D4 Bit) 
                 -> Signal (FSVec D8 Bit)
                 -> (Signal (FSVec D4 Bit), Signal(FSVec D4 Bit)))
fkSys = newSysDef fkProc "fkSys" ["ipLeft", "ipRight", "subkey"] 
                                 ["fkSysLeft", "fkSysRight"]


-- System Definition: encryptSys
-- 
-- encryptSys encrypts a plaintext based on two subkeys

encryptProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
            -> Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
encryptProc subkey1 subkey2 plaintext = ciphertext 
   where
      ciphertext = (instantiate "ipBarSys" ipBarSys) fkSys2Left fkSys2Right
      (fkSys2Left, fkSys2Right) 
         = (instantiate "fkSys2" fkSys) switchLeft switchRight subkey2
      (switchLeft, switchRight) 
         = (instantiate "switchSys" switchSys) fk1SysLeft fk1SysRight
      (fk1SysLeft, fk1SysRight) 
         = (instantiate "fkSys1" fkSys) ipSysLeft ipSysRight subkey1
      (ipSysLeft, ipSysRight)
         = (instantiate "ipSys" ipSys) plaintext

encryptSys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
                      -> Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit))
encryptSys = newSysDef encryptProc "encryptSys" ["subkey1", "subkey2", "plain"] ["cipher"]

int2bv8Fun :: ProcFun (Int8 -> FSVec D8 Bit)
int2bv8Fun = $(newProcFun [d| int2bv8 :: Int8 -> FSVec D8 Bit 
                              int2bv8 x = toBitVector8 x |]) 

int2bv8Proc :: Signal Int8 -> Signal (FSVec D8 Bit)
int2bv8Proc x = mapSY "int2bv8" int2bv8Fun x 

int2bv8Sys :: SysDef (Signal Int8 -> Signal (FSVec D8 Bit))
int2bv8Sys = newSysDef int2bv8Proc "int2bv8Sys" ["int"] ["bv8"] 

encryptProc' :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
            -> Signal Int8 -> Signal Int8
encryptProc' subkey1 subkey2 plaintext = ciphertext 
   where
      ciphertext = (instantiate "bv82int" bv82intSys) ciphertext_bv8
      ciphertext_bv8 = (instantiate "ipBarSys" ipBarSys) fkSys2Left fkSys2Right
      (fkSys2Left, fkSys2Right) 
         = (instantiate "fkSys2" fkSys) switchLeft switchRight subkey2
      (switchLeft, switchRight) 
         = (instantiate "switchSys" switchSys) fk1SysLeft fk1SysRight
      (fk1SysLeft, fk1SysRight) 
         = (instantiate "fkSys1" fkSys) ipSysLeft ipSysRight subkey1
      (ipSysLeft, ipSysRight)
         = (instantiate "ipSys" ipSys) plaintext_bv
      plaintext_bv = (instantiate "int2bv8" int2bv8Sys) plaintext

encryptSys' :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
                      -> Signal Int8 -> Signal Int8)
encryptSys' = newSysDef encryptProc' "encryptSys" ["subkey1", "subkey2", "plain"] ["cipher"]



-- System Definition: decryptSys
-- 
-- decryptSys decrypts a plaintext based on two subkeys

decryptProc :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
            -> Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
decryptProc subkey1 subkey2 ciphertext = plaintext
   where
      plaintext = (instantiate "ipBarSys" ipBarSys) fk1SysLeft fk1SysRight
      (fk1SysLeft, fk1SysRight) 
         = (instantiate "fkSys1" fkSys) switchLeft switchRight subkey1
      (switchLeft, switchRight) 
         = (instantiate "switchSys" switchSys) fk2SysLeft fk2SysRight
      (fk2SysLeft, fk2SysRight) 
         = (instantiate "fkSys2" fkSys) ipSysLeft ipSysRight subkey2
      (ipSysLeft, ipSysRight)
         = (instantiate "ipSys" ipSys) ciphertext


decryptSys :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
                      -> Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit))
decryptSys = newSysDef decryptProc "decryptSys" ["subkey1", "subkey2", "cipher"] ["plain"]


bv82intFun :: ProcFun (FSVec D8 Bit -> Int8)
bv82intFun = $(newProcFun [d| bv82int :: FSVec D8 Bit -> Int8 
                              bv82int x = fromBitVector8 x |]) 

bv82intProc :: Signal (FSVec D8 Bit) -> Signal Int8
bv82intProc x = mapSY "bv82int" bv82intFun x 

bv82intSys :: SysDef (Signal (FSVec D8 Bit) -> Signal Int8)
bv82intSys = newSysDef bv82intProc "bv82intSys" ["bv8"] ["sys"]

decryptProc' :: Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
             -> Signal Int8 -> Signal Int8
decryptProc' subkey1 subkey2 ciphertext = plaintext
   where
      plaintext = (instantiate "bv82int" bv82intSys) plaintext_bv8
      plaintext_bv8 = (instantiate "ipBarSys" ipBarSys) fk1SysLeft fk1SysRight
      (fk1SysLeft, fk1SysRight) 
         = (instantiate "fkSys1" fkSys) switchLeft switchRight subkey1
      (switchLeft, switchRight) 
         = (instantiate "switchSys" switchSys) fk2SysLeft fk2SysRight
      (fk2SysLeft, fk2SysRight) 
         = (instantiate "fkSys2" fkSys) ipSysLeft ipSysRight subkey2
      (ipSysLeft, ipSysRight)
         = (instantiate "ipSys" ipSys) ciphertext_bv8
      ciphertext_bv8 = (instantiate "int2bv8" int2bv8Sys) ciphertext

decryptSys' :: SysDef (Signal (FSVec D8 Bit) -> Signal (FSVec D8 Bit)
                      -> Signal (Int8) -> Signal Int8)
decryptSys' = newSysDef decryptProc' "decryptSys" ["subkey1", "subkey2", "cipher"] ["plain"]

-- System Definition: desSys
--
-- desSys combines encryption, decryption and subkey module. It takes
-- a plaintext, a cyphertext and a key as input

desProc :: Signal (FSVec D10 Bit) -> Signal (FSVec D8 Bit) -> Signal
           (FSVec D8 Bit) -> (Signal (FSVec D8 Bit), Signal (FSVec D8 Bit))
desProc key plainIn cipherIn = (cipherOut, plainOut)
   where plainOut = (instantiate "decryptSys" decryptSys) subkey1
                       subkey2 cipherIn
         cipherOut = (instantiate "encryptSys" encryptSys) subkey1
                       subkey2 plainIn
         (subkey1, subkey2) = (instantiate "subkeysSys" subkeysSys) key

desSys :: SysDef (   Signal (FSVec D10 Bit) -> Signal (FSVec D8 Bit) 
                  -> Signal (FSVec D8 Bit) 
                  -> (Signal (FSVec D8 Bit), Signal (FSVec D8 Bit)))
desSys = newSysDef desProc "desSys" ["key", "plainIn", "cipherIn"]
         ["cipherOut", "plainOut"]


-- Output Quartus
compileQuartus :: IO ()
compileQuartus = writeVHDLOps vhdlOps fSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[]
                              }


-- Simulation
s_1 = [(L +> L +> empty), (L +> H +> empty)]
s_2 = [(H +> L +> empty), (H +> H +> empty)]

key1 = [H +> L +> H +> L +> L +> L +> L +> L +> H +> L +> empty] 
subkey_1 = fst $ simulate subkeysSys key1
subkey_2 = snd $ simulate subkeysSys key1

plain = [L +> L +> H +> L +> L +> H +> L +> L +> empty]
ipLeft = fst $ simulate ipSys plain
ipRight = snd $ simulate ipSys plain

enc = simulate encryptSys subkey_1 subkey_2 plain
fk = simulate fkSys ipLeft ipRight subkey_1
ep = simulate expPermSys ipLeft
zxor = simulate xorSubkeySys subkey_1 ep
rS0 = simulate rowS0Sys zxor
rS1 = simulate rowS1Sys zxor
cS0 = simulate colS0Sys zxor
cS1 = simulate colS1Sys zxor
oS0 = simulate outputS0Sys rS0 cS0
oS1 = simulate outputS1Sys rS1 cS1
dec = simulate decryptSys subkey_1 subkey_2 enc


-- Hardware Generation
compileQuartus_subkeysSys :: IO ()
compileQuartus_subkeysSys = writeVHDLOps vhdlOps subkeysSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[]
                              }

-- Modelsim
vhdlSim = writeAndModelsimVHDL Nothing encryptSys subkey_1 subkey_2 plain

-- Hardware Generation
compileQuartus_desSys :: IO ()
compileQuartus_desSys = writeVHDLOps vhdlOps desSys
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("CycloneII",
                                                       Just "EP2C35F672C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[]
                              }
