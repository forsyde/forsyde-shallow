{--# OPTIONS_GHC -w #--}
-- FIXME: remove warnings


{- |
It defines the bit vector operations from\/to integer.
-}
module ForSyDe.Shallow.BitVector(
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

import ForSyDe.Shallow.Vector

type BitVector = Vector Integer

-- |To judge whether the input bit-vector is in a proper form.
isBitVector :: (Num t) =>
          Vector t  -- ^Input bit-vector
       -> Bool      -- ^Output boolean value
isBitVector NullV = True
isBitVector (x:>xs) = (x == 0 || x == 1) && isBitVector xs

-- |To transform the input integer to a bit-vector with specified number of
-- bits.
intToBitVector :: Int  -- ^Num of the bits
               -> Integer  -- ^The input integer
               -> BitVector -- ^The output bit-vector
intToBitVector bits n | n >= 0 && n < 2^(bits-1) 
			   = intToBitVector' bits n
intToBitVector bits n | n < 0 && abs n <= 2^(bits-1) 
			   = intToBitVector' bits (n + 2^bits)
intToBitVector bits n | otherwise = 
          error "intToBitvector : Number out of range!" 

-- |Helper function of 'intToBitVector'.
intToBitVector' 0    n = NullV
intToBitVector' bits n = if (n >= 2^(bits-1)) then
			     1 :> intToBitVector' (bits-1) (n - 2^(bits-1))
			 else  
			     0 :> intToBitVector' (bits-1) n

-- |To transform the input bit-vecotr to an integer.
bitVectorToInt :: BitVector -> Integer
bitVectorToInt (1:>xv) | isBitVector xv 
               = bitVectorToInt' xv (lengthV xv) - 2^(lengthV xv) 
bitVectorToInt (0:>xv) | isBitVector xv 
               = bitVectorToInt' xv (lengthV xv)
bitVectorToInt xv | not $ isBitVector xv 
               = error "bitVectorToInt: Vector is not a BitVector!"

-- |Helper function of 'bitVectorToInt'.
bitVectorToInt' NullV   bit = 0
bitVectorToInt' (x:>xv) bit = x * 2^(bit-1) + bitVectorToInt' xv (bit-1)

data Parity = Even | Odd deriving (Show, Eq)

-- |To add even parity bit on the bit-vector in the tail.
addEvenParityBit = addParityBit Even
-- |To add odd parity bit on the bit-vector in the tail.
addOddParityBit  = addParityBit Odd

addParityBit Even v | isBitVector v
        = if evenNumber v then
	     v <+> unitV 0
          else
	     v <+> unitV 1
addParityBit Odd  v | isBitVector v
        = if evenNumber v then
	     v <+> unitV 1
          else
	     v <+> unitV 0
addParityBit _    v | not $ isBitVector v
	= error "addParity: Vector is not a BitVector"  

-- |To remove the parity bit in the tail.
removeParityBit v | isBitVector v 
        = takeV (lengthV v - 1) v
removeParityBit v | otherwise 
        = error "removeParityBit: Vector is not a BitVector "

-- |To check the even parity of the bit-vector.
isEvenParity = isParityCorrect Even

-- |To check the odd parity of the bit-vector.
isOddParity = isParityCorrect Odd

isParityCorrect Even xv = evenNumber xv
isParityCorrect Odd xv  = not $ evenNumber xv 

evenNumber NullV   = True
evenNumber (0:>xv) = xor False (evenNumber xv)
evenNumber (1:>xv) = xor True (evenNumber xv)
evenNumber (_:>xv) = error "evenNumber: Vector is not a BitVector "
                     
xor True  False = True
xor False True  = True
xor _     _     = False 

