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
import qualified Data.Param.FSVec as FS


-- Calculation of subkey 1 and subkey 2
p10 :: FSVec D10 Bit -> FSVec D10 Bit
p10 key =    key!d2 +> key!d4 +> key!d1 +> key!d6 +> key!d5 
          +> key!d9 +> key!d0 +> key!d8 +> key!d7 +> key!d5 +> empty
                 
splitKey :: FSVec D10 Bit -> (FSVec D5 Bit, FSVec D5 Bit) 
splitKey key = (FS.take d5 key, FS.drop d5 key)


p8 :: FSVec D10 Bit -> FSVec D8 Bit
p8 key =    key!d5 +> key!d2 +> key!d6 +> key!d3 
         +> key!d7 +> key!d4 +> key!d9 +> key!d8 +> empty

shift :: FSVec D5 Bit -> FSVec D5 Bit
shift vec = rotr vec



subkey1 key = p8 (rotatedKey1 FS.++ rotatedKey2)
             where
               rotatedKey1 = SimpleDES.shift key1
               rotatedKey2 = SimpleDES.shift key2
               (key1, key2) = splitKey (p10 key)

subkey2 key = p8 (rotatedKey1 FS.++ rotatedKey2)
             where
               rotatedKey1 = (SimpleDES.shift . SimpleDES.shift . SimpleDES.shift) key1
               rotatedKey2 = (SimpleDES.shift . SimpleDES.shift . SimpleDES.shift) key2
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
splitBlock block = (FS.take d4 block, 
                    FS.drop d4 block)
 
f_mapping :: FSVec D4 Bit -> FSVec D8 Bit
f_mapping nibble =   nibble!d3 +> nibble!d0 +> nibble!d1 +> nibble!d2
                  +> nibble!d1 +> nibble!d2 +> nibble!d3 +> nibble!d0 +> empty

expPerm :: FSVec D4 Bit -> FSVec D8 Bit
expPerm nibble =    nibble!d3 +> nibble!d0 +> nibble!d1 +> nibble!d2
                  +> nibble!d1 +> nibble!d2 +> nibble!d3 +> nibble!d0 +> empty

zipxor :: FSVec D8 Bit -> FSVec D8 Bit -> FSVec D8 Bit
zipxor key input = FS.zipWith xor key input

f_xor :: FSVec D8 Bit -> FSVec D4 Bit -> FSVec D8 Bit
f_xor key nibble = FS.zipWith xor key (f_mapping nibble)


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

rowS0 :: FSVec D8 Bit -> FSVec D2 Bit
rowS0 pmatrix = pmatrix!d0 +> pmatrix!d3 +> empty

rowS1 :: FSVec D8 Bit -> FSVec D2 Bit
rowS1 pmatrix = pmatrix!d4 +> pmatrix!d7 +> empty

colS0 :: FSVec D8 Bit -> FSVec D2 Bit
colS0 pmatrix = pmatrix!d1 +> pmatrix!d2 +> empty

colS1 :: FSVec D8 Bit -> FSVec D2 Bit
colS1 pmatrix = pmatrix!d5 +> pmatrix!d6 +> empty

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
output pmatrix = p4 $ outS0 FS.++ outS1
                 where outS0 = outputS0 pmatrix
                       outS1 = outputS1 pmatrix

s0matrix = outputS0
s1matrix = outputS1

f :: FSVec D8 Bit -> FSVec D4 Bit -> FSVec D4 Bit
f subkey nibble 
    = p4 (outS0 FS.++ outS1)
      where outS0  = s0matrix outXor
            outS1  = s1matrix outXor
            outXor = zipxor subkey outEP
            outEP  = expPerm nibble

f_k' :: FSVec D8 Bit -> FSVec D8 Bit -> FSVec D8 Bit
f_k' subkey input = left FS.++ right 
                    where right = FS.drop d4 input
                          left' = FS.take d4 input 
                          left = FS.zipWith xor left' (output pmatrix)
                          pmatrix = f_xor subkey right

f_k :: FSVec D8 Bit -> FSVec D8 Bit 
    -> FSVec D8 Bit
f_k subkey input 
   = outLeft FS.++ outRight 
     where 
       outLeft = FS.zipWith xor inpLeft fOut
       outRight = inpRight
       fOut = f subkey inpRight
       inpLeft = FS.take d4 input 
       inpRight = FS.drop d4 input

-- switch 

switch :: (FSVec D8 Bit) -> FSVec D8 Bit
switch nibble =  FS.drop d4 nibble 
                 FS.++ 
                 FS.take d4 nibble


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

key1 = H +> L +> H +> L +> L +> L +> L +> L +> H +> L +> empty 
subkey_1 = subkey1 key1
subkey_2 = subkey2 key1

plain = L +> L +> H +> L +> L +> H +> L +> L +> empty
ipLeft = FS.take d4 (ip plain)
ipRight = FS.drop d4 (ip plain)

enc = encrypt key1 plain
fk = f_k subkey_1 $ ip $ plain
ep = expPerm ipLeft
zxor = zipxor subkey_1 (expPerm ipLeft) 
rS0 = rowS0 zxor
rS1 = rowS1 zxor
cS0 = colS0 zxor
cS1 = colS1 zxor
oS0 = outputS0 zxor
oS1 = outputS1 zxor