-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Core.Matrix
-- Copyright   :  (c) ForSyDe Group, KTH 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Matrix' and corresponding
-- functions. It is a shallow interpretation of 2D arrays and is used
-- for quick prototyping of array algorithms and skeletons, although
-- itself is a type synonym for a `Vector` of `Vector`s. Therefore
-- this module is simply a collection of utility functions on 2D
-- vectors used mainly for convenience. For a type-checked fixed-size
-- data type for representing matrices, see the
-- <http://hackage.haskell.org/package/matrix matrix> or
-- <http://hackage.haskell.org/package/repa REPA> packages.
--
-- __OBS:__ The lengths in the API documentation for function arguments
-- are not type-safe, but rather suggestions for usage in designing
-- matrix algorithms or skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.Matrix (
  Matrix (..),
  prettyMat,
  -- * Queries
  nullMat, sizeMat, wellFormedMat,
  -- * Generators
  matrix, fromMatrix, unitMat,
  -- * Functional skeletons
  mapMat, zipWithMat, zipWith3Mat, reduceMat, 
  -- * Selectors
  atMat, groupMat, cropMat, stencilMat,
  -- * Permutators
  rotateMat, reverseMat, transposeMat, replaceMat
  ) where

import ForSyDe.Shallow.Core.Vector
import Data.List (intercalate)

-- | 'Matrix' is simply a type synonym for vector of vectors. This
-- means that /any/ function on 'Vector' works also on 'Matrix'.
type Matrix a = Vector (Vector a)

-- | Prints out to the terminal a matrix in a readable format, where
-- all elements are right-aligned and separated by a custom separator.
--
-- >>> let m = matrix 3 3 [1,2,3,3,100,4,12,32,67]
-- >>> prettyMat "|" m
--  1|  2| 3
--  3|100| 4
-- 12| 32|67
prettyMat :: Show a
          => String   -- ^ separator string
          -> Matrix a -- ^ input matrix
          -> IO ()
prettyMat sep mat = mapM_ putStrLn $ fromVector $ printMat maxWdt strMat
  where
    maxWdt = reduceV (zipWithV max) $ mapMat length strMat
    strMat = mapMat show mat
    printMat w  = mapV (\row -> printRow w row)
    printRow w  = intercalate sep . fromVector . zipWithV align w
    align n str = replicate (n - length str) ' ' ++ str

nullMat :: Matrix a -> Bool
nullMat NullV = True
nullMat (NullV:>NullV) = True
nullMat _ = False

sizeMat :: Matrix a -> (Int,Int)
sizeMat m = (x,y)
  where
    y = lengthV m
    x = (lengthV . headV) (wellFormedMat m)

wellFormedMat :: Matrix a -> Matrix a
wellFormedMat NullV = error "matrix is null"
wellFormedMat m@(x:>NullV) = m
wellFormedMat m@(x:>xs)
  | reduceV (&&) (mapV (\r -> lengthV r == lengthV x) xs) = m
  | otherwise = error "matrix ill-formed: rows are of unequal lengths"

-- | Converts a list into a 'Matrix'.
matrix :: Int      -- ^ number of columns (X dimension)
       -> Int      -- ^ number of rows (Y dimension)
       -> [a]      -- ^ list of pixels
       -> Matrix a -- ^ 'Matrix' of pixels
matrix x y = groupV x . vector . check
  where
    check l | length l == x * y = l
            | otherwise
      = error $ "cannot form matrix (" ++ show x ++ ","
              ++ show y ++ ") from a list with "
              ++ show (length l) ++ " elements"

fromMatrix :: Matrix a -> [a]
fromMatrix = fromVector . concatV

unitMat :: a -> Matrix a
unitMat a = (a:>NullV):>NullV

mapMat = mapV . mapV

zipWithMat f = zipWithV (zipWithV f)

zipWith3Mat f = zipWith3V (\a b c -> zipWith3V f a b c)

reduceMat f = reduceV f . mapV (reduceV f)

atMat x y mat = (mat `atV` y) `atV` x

groupMat x y = groupV y . mapV (groupV x)

-- |
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " m
--  1  2  3  4
-- 11 12 13 14
-- 21 22 23 24
-- 31 32 33 34
-- >>> prettyMat " " $ cropMat 2 3 1 1 m
-- 12 13
-- 22 23
-- 32 33
cropMat :: Int -> Int -> Int -> Int -> Matrix a -> Matrix a
cropMat w h pX pY = mapV (crop w pX) . crop h pY
  where crop size pos = dropV pos . takeV (pos + size) 


-- |
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " $ stencilMat 2 2 m
--   <<1,2>,<11,12>>   <<2,3>,<12,13>>   <<3,4>,<13,14>>
-- <<11,12>,<21,22>> <<12,13>,<22,23>> <<13,14>,<23,24>>
-- <<21,22>,<31,32>> <<22,23>,<32,33>> <<23,24>,<33,34>>
stencilMat :: Int -> Int -> Matrix a -> Matrix (Matrix a)
stencilMat r c = arrange . groupCols . groupRows
  where
    groupRows =         mapV (takeV r) . dropFromEnd r . tailsV
    groupCols = mapMat (mapV (takeV c) . dropFromEnd c . tailsV)
    arrange   = mapV transposeMat
    dropFromEnd n v = takeV (lengthV v - n) v

reverseMat :: Matrix a -> Matrix a
reverseMat = reverseV . mapV reverseV

-- | Parallel communication (data access) pattern which "rotates" a
-- matrix. The rotation is controled with the /x/ and /y/ index
-- arguments as following:
--
-- * @(> 0)@ : rotates the matrix right/down with the corresponding
-- number of positions.
-- 
-- * @(= 0)@ : does not modify the position for that axis.
-- 
-- * @(< 0)@ : rotates the matrix left/up with the corresponding
-- number of positions.
rotateMat :: Int -- ^ index on X axis
          -> Int -- ^ index on Y axis
          -> Vector (Vector a)
          -> Vector (Vector a)
rotateMat x y = rotateV y . mapV (rotateV x)

transposeMat NullV = NullV
transposeMat (NullV:>xss) = transposeMat xss
transposeMat rows = (mapV headV rows) :> transposeMat (mapV tailV rows)

-- |
--
-- >>> let m  = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> let m1 = matrix 2 2 [101,202,303,404]
-- >>> prettyMat " " $ replaceMat 1 1 m1 m
--  1   2   3  4
-- 11 101 202 14
-- 21 303 404 24
-- 31  32  33 34
replaceMat :: Int -> Int -> Matrix a -> Matrix a -> Matrix a
replaceMat x y mask = replace y h (zipWithV (\m o -> replace x w (\_ -> m) o) mask)
  where
    (w,h) = sizeMat mask
    replace start size replaceF vec
      = let begin  = takeV start vec
            middle = replaceF $ dropV start $ takeV (start + size) vec
            end    = dropV (start + size) vec
        in begin <+> middle <+> end
