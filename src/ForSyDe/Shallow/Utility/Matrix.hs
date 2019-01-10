-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Utility.Matrix
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
module ForSyDe.Shallow.Utility.Matrix (
  Matrix, prettyMat,
  -- * Queries
  nullMat, sizeMat, wellFormedMat,
  -- * Generators
  matrix, fromMatrix, unitMat, indexMat,
  -- * Functional skeletons
  mapMat, zipWithMat, zipWith3Mat, reduceMat, dotVecMat, dotMatMat,
  -- * Selectors
  atMat, takeMat, dropMat, cropMat, groupMat, stencilMat,
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

-- | Checks if a matrix is null. @<>@ and @<<>>@ are both null
-- matrices.
nullMat :: Matrix a -> Bool
nullMat NullV = True
nullMat (NullV:>NullV) = True
nullMat _ = False

-- | Returns the X and Y dimensions of matrix and checks if it is well formed.
sizeMat :: Matrix a -> (Int,Int)
sizeMat m = (x,y)
  where
    y = lengthV m
    x = (lengthV . headV) (wellFormedMat m)

-- | Checks if a matrix is well-formed, meaning that all its rows are
-- of equal length. Returns the same matrix in case it is well-formed
-- or throws an exception if it is ill-formed.
wellFormedMat :: Matrix a -> Matrix a
wellFormedMat NullV = error "matrix is null"
wellFormedMat m@(_:>NullV) = m
wellFormedMat m@(x:>xs)
  | reduceV (&&) (mapV (\r -> lengthV r == lengthV x) xs) = m
  | otherwise = error "matrix ill-formed: rows are of unequal lengths"

-- | Converts a list into a 'Matrix'. See example from 'prettyMat'.
matrix :: Int      -- ^ number of columns (X dimension) @= x@
       -> Int      -- ^ number of rows (Y dimension) @= y@
       -> [a]      -- ^ list of values; /length/ = @x * y@
       -> Matrix a -- ^ 'Matrix' of values; /size/ = @(x,y)@
matrix x y = groupV x . vector . check
  where
    check l | length l == x * y = l
            | otherwise
      = error $ "cannot form matrix (" ++ show x ++ ","
              ++ show y ++ ") from a list with "
              ++ show (length l) ++ " elements"

-- | Converts a matrix back to a list.
fromMatrix :: Matrix a -- ^ /size/ = @(x,y)@
           -> [a]      -- ^ /length/ = @x * y@
fromMatrix = fromVector . concatV

-- | Creates a unit (i.e. singleton) matrix, which is a matrix with
-- only one element.
unitMat :: a -> Matrix a -- ^ /size/ = @(1,1)@
unitMat a = (a:>NullV):>NullV

-- | Returns an /infinite matrix/ with (X,Y) index pairs. You need to
-- zip it against another (finite) matrix or to extract a finite
-- subset in order to be useful (see example below).
--
-- >>> prettyMat " " $ takeMat 3 4 indexMat 
-- (0,0) (1,0) (2,0)
-- (0,1) (1,1) (2,1)
-- (0,2) (1,2) (2,2)
-- (0,3) (1,3) (2,3)
indexMat :: Matrix (Int, Int)
indexMat = zipWithMat (,) colix rowix
  where
    colix = vector $ repeat $ vector [0..]
    rowix = transposeMat colix 

-- | Maps a function on every value of a matrix.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
mapMat :: (a -> b)
       -> Matrix a -- ^ /size/ = @(xa,ya)@
       -> Matrix b -- ^ /size/ = @(xa,ya)@
mapMat = mapV . mapV

-- | Applies a binary function pair-wise on each element in two matrices.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
zipWithMat :: (a -> b -> c)
           -> Matrix a -- ^ /size/ = @(xa,ya)@
           -> Matrix b -- ^ /size/ = @(xb,yb)@
           -> Matrix c -- ^ /size/ = @(minimum [xa,xb], minimum [ya,yb])@
zipWithMat f = zipWithV (zipWithV f)

-- | Applies a function 3-tuple-wise on each element in three matrices.
--
-- __OBS:__ this function does not check if the output matrix is well-formed.
zipWith3Mat :: (a -> b -> c -> d)
            -> Matrix a -- ^ /size/ = @(xa,ya)@
            -> Matrix b -- ^ /size/ = @(xb,yb)@
            -> Matrix c -- ^ /size/ = @(xc,yc)@
            -> Matrix d -- ^ /size/ = @(minimum [xa,xb,xc], minimum [ya,yb,yc])@
zipWith3Mat f = zipWith3V (\a b c -> zipWith3V f a b c)

-- | Reduces all the elements of a matrix to one element based on a
-- binary function.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> reduceMat (+) m
-- 108
reduceMat :: (a -> a -> a) -> Matrix a -> a
reduceMat f = reduceV f . mapV (reduceV f)

-- | Pattern implementing the template for a dot operation between a
-- vector and a matrix.
--
-- >>> let mA = matrix 4 4 [1,-1,1,1,  1,-1,-1,-1,  1,1,-1,1,  1,1,1,-1]
-- >>> let y  = vector[1,0,0,0]
-- >>> dotVecMat (+) (*) mA y
-- <1,1,1,1>
dotVecMat :: (a -> a -> a) -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
          -> (b -> a -> a) -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
          -> Matrix b      -- ^ /size/ = @(xa,ya)@
          -> Vector a      -- ^ /length/ = @xa@
          -> Vector a      -- ^ /length/ = @xa@
dotVecMat f g mA y = mapV (\x -> reduceV f $ zipWithV g x y) mA

-- | Pattern implementing the template for a dot operation between two
-- matrices.
--
-- >>> let mA = matrix 4 4 [1,-1,1,1,  1,-1,-1,-1,  1,1,-1,1,  1,1,1,-1]
-- >>> prettyMat " " $ dotMatMat (+) (*) mA mA
-- 2 -2  2  2
-- 2 -2 -2 -2
-- 2  2  2 -2
-- 2  2 -2  2
dotMatMat :: (a -> a -> a) -- ^ kernel function for a row/column reduction, e.g. @(+)@ for dot product
          -> (b -> a -> a) -- ^ binary operation for pair-wise elements, e.g. @(*)@ for dot product
          -> Matrix b      -- ^ /size/ = @(xa,ya)@
          -> Matrix a      -- ^ /size/ = @(ya,xa)@
          -> Matrix a      -- ^ /size/ = @(xa,xa)@
dotMatMat f g m = mapV (dotVecMat f g m) . transposeMat

-- | Returns the element of a matrix at a certain position.
--
-- >>> let m = matrix 3 3 [1,2,3,11,12,13,21,22,23]
-- >>> atMat 2 1 m
-- 13
atMat :: Int       -- ^ X index starting from zero
      -> Int       -- ^ Y index starting from zero
      -> Matrix a
      -> a
atMat x y mat = (mat `atV` y) `atV` x

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " $ takeMat 2 2 m
--  1  2
-- 11 12
takeMat :: Int       -- ^ X index starting from zero
        -> Int       -- ^ Y index starting from zero
        -> Matrix a
        -> Matrix a
takeMat x y = mapV (takeV x) . takeV y

-- | Returns the upper-left part of a matrix until a specific
-- position.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " $ dropMat 2 2 m
-- 23 24
-- 33 34
dropMat :: Int       -- ^ X index starting from zero
        -> Int       -- ^ Y index starting from zero
        -> Matrix a
        -> Matrix a
dropMat x y = mapV (dropV x) . dropV y

-- | Crops a section of a matrix.
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
cropMat :: Int      -- ^ crop width  = @w@
        -> Int      -- ^ crop height = @h@
        -> Int      -- ^ X start position = @x0@
        -> Int      -- ^ Y start position = @y0@
        -> Matrix a -- ^ /size/ = @(xa,ya)@
        -> Matrix a -- ^ /size/ = @(minimum [w,xa-x0], minimum [h,xa-x0])@
cropMat w h pX pY = takeMat w h . dropMat pX pY

-- cropMat w h pX pY = mapV (crop w pX) . crop h pY
--   where crop size pos = dropV pos . takeV (pos + size) 

-- | Groups a matrix into smaller equallly-shaped matrices.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " $ groupMat 2 2 m
--   <<1,2>,<11,12>>   <<3,4>,<13,14>>
-- <<21,22>,<31,32>> <<23,24>,<33,34>>
groupMat :: Int      -- ^ width of groups = @w@
         -> Int      -- ^ height of groups = @h@
         -> Matrix a -- ^ /size/ = @(xa,ya)@
         -> Matrix (Matrix a) -- ^ /size/ = @(xa `div` w,ya `div` h)@
groupMat w h = mapV transposeMat . groupV h . mapV (groupV w)


-- | Returns a stencil of neighboring elements for each possible
-- element in a vector.
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

-- | Reverses the order of elements in a matrix
reverseMat :: Matrix a -> Matrix a
reverseMat = reverseV . mapV reverseV

-- | Pattern which "rotates" a matrix. The rotation is controled with
-- the /x/ and /y/ index arguments as following:
--
-- * @(> 0)@ : rotates the matrix right/down with the corresponding
-- number of positions.
-- 
-- * @(= 0)@ : does not modify the position for that axis.
-- 
-- * @(< 0)@ : rotates the matrix left/up with the corresponding
-- number of positions.
--
-- >>> let m = matrix 4 4 [1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34]
-- >>> prettyMat " " $ rotateMat (-1) 1 m
-- 32 33 34 31
--  2  3  4  1
-- 12 13 14 11
-- 22 23 24 21
rotateMat :: Int -- ^ index on X axis
          -> Int -- ^ index on Y axis
          -> Vector (Vector a)
          -> Vector (Vector a)
rotateMat x y = rotateV y . mapV (rotateV x)

-- | Transposes a matrx.
transposeMat :: Matrix a -- ^ /size/ = @(x,y)@
             -> Matrix a -- ^ /size/ = @(y,x)@
transposeMat NullV = NullV
transposeMat (NullV:>xss) = transposeMat xss
transposeMat rows = (mapV headV rows) :> transposeMat (mapV tailV rows)

-- | Replaces a part of matrix with another (smaller) part, starting
-- from an arbitrary position.
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
