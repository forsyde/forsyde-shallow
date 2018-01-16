-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Core.BitVector
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- It defines the bit vector operations from\/to integer.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.BitVector(
    -- *Polynomial data type
    BitVector, Parity(..),
    -- *Bit-vector and integer transformations
    intToBitVector, bitVectorToInt,
    -- *Add even\/odd parity bit
    addEvenParityBit, addOddParityBit, addParityBit,
    -- *Remove parity bit
    removeParityBit,
    -- *Check the even\/odd parity of the bit-vector
    isEvenParity, isOddParity,
    -- *Judge the form sanity of the bit-vector
    isBitVector
  )
  where

import ForSyDe.Shallow.Core.Vector

type BitVector = Vector Integer

-- |To judge whether the input bit-vector is in a proper form.
isBitVector :: (Num t, Eq t) =>
      Vector t  -- ^Input bit-vector
   -> Bool  -- ^Output boolean value
isBitVector NullV = True
isBitVector (x:>xs) = (x `elem` [0, 1]) && isBitVector xs

-- |To transform the input integer to a bit-vector with specified number of
-- bits.
intToBitVector :: Int  -- ^Num of the bits
       -> Integer  -- ^The input integer
       -> BitVector -- ^The output bit-vector
intToBitVector bits n | n >= 0 && n < 2^(bits-1) 
       = intToBitVector' bits n
intToBitVector bits n | n < 0 && abs n <= 2^(bits-1) 
       = intToBitVector' bits (n + 2^bits)
intToBitVector _    _ | otherwise = 
      error "intToBitvector : Number out of range!" 

-- |Helper function of 'intToBitVector'.
intToBitVector' :: (Num a, Ord a1, Num a1, Integral t) => 
       t -> a1 -> Vector a
intToBitVector' 0    _ = NullV
intToBitVector' bits n = if n >= 2^(bits-1) then
         1 :> intToBitVector' (bits-1) (n - 2^(bits-1))
       else  
         0 :> intToBitVector' (bits-1) n

-- |To transform the input bit-vecotr to an integer.
bitVectorToInt :: BitVector -> Integer
bitVectorToInt (1:>xv) | isBitVector xv 
       = bitVectorToInt' xv (lengthV xv) - 2 ^ lengthV xv 
bitVectorToInt (0:>xv) | isBitVector xv 
       = bitVectorToInt' xv (lengthV xv)
bitVectorToInt _ = error "bitVectorToInt: Vector is not a BitVector!"


-- |Helper function of 'bitVectorToInt'.
bitVectorToInt' :: (Integral a, Num t) => Vector t -> a -> t
bitVectorToInt' NullV   _   = 0
bitVectorToInt' (x:>xv) bit = x * 2^(bit-1) + bitVectorToInt' xv (bit-1)

data Parity = Even | Odd deriving (Show, Eq)

-- |To add even parity bit on the bit-vector in the tail.
addEvenParityBit :: (Num a, Eq a) => Vector a -> Vector a
addEvenParityBit = addParityBit Even
-- |To add odd parity bit on the bit-vector in the tail.
addOddParityBit :: (Num a, Eq a) => Vector a -> Vector a
addOddParityBit  = addParityBit Odd

addParityBit :: (Num a, Eq a) => Parity -> Vector a -> Vector a
addParityBit p v 
  | isBitVector v = case p of
    Even -> resZero even
    Odd -> resZero (not even)
  | otherwise =  error "addParity: Vector is not a BitVector"  
 where even = evenNumber v 
       resZero b = v <+> unitV (if b then 0 else 1)


-- |To remove the parity bit in the tail.
removeParityBit :: (Num t, Eq t) => Vector t -> Vector t
removeParityBit v 
 | isBitVector v = takeV (lengthV v - 1) v
 | otherwise = error "removeParityBit: Vector is not a BitVector "

-- |To check the even parity of the bit-vector.
isEvenParity :: (Num t, Eq t) => Vector t -> Bool
isEvenParity = isParityCorrect Even

-- |To check the odd parity of the bit-vector.
isOddParity :: (Num t, Eq t) => Vector t -> Bool
isOddParity = isParityCorrect Odd

isParityCorrect :: (Num t, Eq t) => Parity -> Vector t -> Bool 
isParityCorrect Even xv = evenNumber xv
isParityCorrect Odd xv  = not $ evenNumber xv 

evenNumber :: (Num t, Eq t) => Vector t -> Bool
evenNumber NullV   = True
evenNumber (0:>xv) = xor False (evenNumber xv)
evenNumber (1:>xv) = xor True (evenNumber xv)
evenNumber (_:>_) = error "evenNumber: Vector is not a BitVector "
         
xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False 

