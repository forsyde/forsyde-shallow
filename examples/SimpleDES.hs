-- The module implements a simplified version of the DES
-- Encryption-Decryption Algorithm. The implementation is based on
-- Stallings: Cryptography and Network Security 4/e, Pearson Education,
-- 2006, Appendix C (available from http://www.box.net/shared/06vnp1hiu3)

module SimpleDES where

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

-- Initial Permutation

ip :: FSVec D8 Bit -> FSVec D8 Bit
ip block =    block!d1 +> block!d5 +> block!d2 +> block!d0
           +> block!d3 +> block!d7 +> block!d4 +> block!d6 +> empty

ip_bar :: FSVec D8 Bit -> FSVec D8 Bit
ip_bar block =    block!d3 +> block!d0 +> block!d2 +> block!d4
               +> block!d6 +> block!d1 +> block!d7 +> block!d5 +> empty

splitBlock :: FSVec D8 Bit -> (FSVec D4 Bit, FSVec D4 Bit)
splitBlock block = (Data.Param.FSVec.take d4 block, 
                    Data.Param.FSVec.drop d4 block)
 
f_mapping :: FSVec D4 Bit -> FSVec D8 Bit
f_mapping nibble =   nibble!d3 +> nibble!d0 +> nibble!d1 +> nibble!d2
          +> nibble!d1 +> nibble!d2 +> nibble!d3 +> nibble!d0 +> empty

f_xor :: FSVec D8 Bit -> FSVec D4 Bit -> FSVec D8 Bit
f_xor key nibble = Data.Param.FSVec.zipWith xor key (f_mapping nibble)

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


p4 :: FSVec D4 Bit -> FSVec D4 Bit
p4 byte = byte!d1 +> byte!d3 +> byte!d2 +> byte!d0 +> empty

switch :: (FSVec D8 Bit) -> FSVec D8 Bit
switch nibble =  Data.Param.FSVec.drop d4 nibble 
                 Data.Param.FSVec.++ 
                 Data.Param.FSVec.take d4 nibble
 