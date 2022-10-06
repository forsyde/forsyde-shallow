-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Core.Vector
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' and the corresponding
-- functions. It is a development of the module defined by
-- <https://ptolemy.berkeley.edu/~johnr/papers/pdf/thesis.pdf Reekie>.
-- The 'Vector' data type is a shallow interpretation of arrays and is
-- used for quick prototyping of array algorithms and skeletons,
-- whereas in fact it is implemented as an infinite list itself. For a
-- type-checked fixed-size data type for representing vectors, see
-- <http://hackage.haskell.org/package/parameterized-data FSVec> or
-- <http://hackage.haskell.org/package/repa REPA>.
--
-- __OBS:__ The lengths in the API documentation for function arguments
-- are not type-safe, but rather suggestions for usage in designing
-- vector algorithms or skeletons.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.Vector (
  Vector (..), (<+>), (<:), 
  -- * Queries
  nullV, lengthV,
  -- * Generators
  vector, fromVector, unitV, 
  iterateV, generateV, copyV,
  -- * Functional skeletons
  mapV, zipWithV, zipWith3V,
  reduceV, pipeV, foldlV, foldrV, 
  scanlV, scanrV, -- meshlV, meshrV,
  -- * Selectors
  atV, headV, tailV, lastV, initV, headsV, tailsV,
  takeV, dropV, selectV, groupV, filterV, stencilV,
  -- * Permutators
  replaceV, zipV, unzipV,
  concatV, reverseV, shiftlV, shiftrV, rotrV, rotlV, rotateV
  ) where

-----------------------------------------------------------------------------
-- CONSTRUCTORS AND INSTANCES
-----------------------------------------------------------------------------

infixr 5 :>
infixl 5 <:
infixr 5 <+>

-- | The data type 'Vector' is modeled similar to a list. It has two data type constructors. 'NullV' constructs the empty vector, while ':>' constructsa vector by adding an value to an existing vector..
--
-- 'Vector' is an instance of the classes 'Read' and 'Show'. This means that the vector
--
-- > 1:>2:>3:>NullV
--
-- is shown as
--
-- > <1,2,3>
data Vector a = NullV
              | a :> (Vector a) deriving (Eq)

instance (Show a) => Show (Vector a) where
  showsPrec p NullV = showParen (p > 9) (showString "<>")
  showsPrec p xs    = showParen (p > 9) (showChar '<' . showVector1 xs)
    where
      showVector1 NullV = showChar '>'            
      showVector1 (y:>NullV) = shows y . showChar '>'
      showVector1 (y:>ys) = shows y . showChar ',' 
                            . showVector1 ys

instance Read a => Read (Vector a) where
   readsPrec _ s = readsVector s

readsVector :: (Read a) => ReadS (Vector a)
readsVector s = [((x:>NullV), rest) | ("<", r2) <- lex s,
            (x, r3)   <- reads r2,
            (">", rest) <- lex r3]
       ++
      [(NullV, r4)    | ("<", r5) <- lex s,
            (">", r4) <- lex r5]
       ++
      [((x:>xs), r6)  | ("<", r7) <- lex s,
            (x, r8)   <- reads r7,
            (",", r9) <- lex r8,
            (xs, r6) <- readsValues r9]

readsValues :: (Read a) => ReadS (Vector a)
readsValues s = [((x:>NullV), r1) | (x, r2)   <- reads s,
              (">", r1) <- lex r2]
      ++
      [((x:>xs), r3)    | (x, r4)   <- reads s,
              (",", r5) <- lex r4,
              (xs, r3)  <- readsValues r5]

-- | The operator '(<:)' appends an element at the end of a vector.
(<:)  :: Vector a  -- ^ /length/ = @la@
      -> a
      -> Vector a  -- ^ /length/ = @la + 1@
xs <: x = xs <+> unitV x     

-- | The operator '<+>' concatenates two vectors.
(<+>) :: Vector a  -- ^ /length/ = @la@
      -> Vector a  -- ^ /length/ = @lb@
      -> Vector a  -- ^ /length/ = @la + lb@
NullV <+> ys   = ys
(x:>xs) <+> ys = x :> (xs <+> ys) 

-----------------------------------------------------------------------------
-- GENERATORS
-----------------------------------------------------------------------------

-- | The function 'vector' converts a list into a vector.
vector        :: [a] -> Vector a
vector []     = NullV
vector (x:xs) = x :> (vector xs)

-- | The function 'fromVector' converts a vector into a list.
fromVector         :: Vector a -> [a]
fromVector NullV   = []
fromVector (x:>xs) = x : fromVector xs

-- | The function 'unitV' creates a vector with one element. 
unitV   :: a -> Vector a  -- ^ /length/ = @1@
unitV x = x :> NullV

-- | The function 'iterateV' generates a vector with a given number of
-- elements starting from an initial element using a supplied function
-- for the generation of elements.
--
-- >>> iterateV 5 (+1) 1
-- <1,2,3,4,5>
iterateV :: (Num a, Eq a)
         => a        -- ^ number of elements = @n@
         -> (b -> b) -- ^ generator function (@last_element -> next_element@)
         -> b        -- ^ initial element
         -> Vector b -- ^ generated vector; /length/ = @n@
iterateV 0 _ _ = NullV
iterateV n f a = a :> iterateV (n-1) f (f a)

-- | The function 'generateV' behaves in the same way as 'iterateV',
-- but starts with the application of the supplied function to the
-- supplied value.
--
-- >>> generateV 5 (+1) 1
-- <2,3,4,5,6>
generateV :: (Num a, Eq a)
         => a        -- ^ number of elements = @n@
         -> (b -> b) -- ^ generator function (@last_element -> next_element@)
         -> b        -- ^ initial element
         -> Vector b -- ^ generated vector; /length/ = @n@
generateV 0 _ _ = NullV
generateV n f a = x :> generateV (n-1) f x 
        where x = f a

-- | The function 'copyV' generates a vector with a given number of
-- copies of the same element.
--
-- >>> copyV 7 5 
-- <5,5,5,5,5,5,5>
copyV     :: (Num a, Eq a)
          => a        -- ^ number of elements = @n@
          -> b        -- ^ element to be copied
          -> Vector b -- ^ /length/ = @n@
copyV k x = iterateV k id x 

-----------------------------------------------------------------------------
-- QUERIES
-----------------------------------------------------------------------------

-- | The function 'nullV' returns 'True' if a vector is empty. 
nullV       :: Vector a -> Bool
nullV NullV = True
nullV _     = False

-- | The function 'lengthV' returns the number of elements in a value. 
lengthV         :: Vector a -> Int
lengthV NullV   = 0
lengthV (_:>xs) = 1 + lengthV xs

-----------------------------------------------------------------------------
-- HIGHER ORDER SKELETONS
-----------------------------------------------------------------------------

-- | The higher-order function 'mapV' applies a function on all elements of a vector.
mapV :: (a -> b)
     -> Vector a  -- ^ /length/ = @la@
     -> Vector b  -- ^ /length/ = @la@
mapV f (x:>xs) = f x :> mapV f xs
mapV _ NullV   = NullV

-- | The higher-order function 'zipWithV' applies a function pairwise on two vectors.
zipWithV :: (a -> b -> c)
         -> Vector a  -- ^ /length/ = @la@
         -> Vector b  -- ^ /length/ = @lb@
         -> Vector c  -- ^ /length/ = @minimum [la,lb]@
zipWithV f (x:>xs) (y:>ys) = f x y :> (zipWithV f xs ys)
zipWithV _ _ _ = NullV

-- | The higher-order function 'zipWithV3' applies a function 3-tuple-wise on three vectors.
zipWith3V :: (a -> b -> c -> d)
          -> Vector a  -- ^ /length/ = @la@
          -> Vector b  -- ^ /length/ = @lb@
          -> Vector c  -- ^ /length/ = @lc@
          -> Vector d  -- ^ /length/ = @minimum [la,lb,lc]@
zipWith3V f (x:>xs) (y:>ys) (z:>zs) = f x y z :> (zipWith3V f xs ys zs)
zipWith3V _ _ _ _ = NullV

-- | The higher-order functions 'foldlV' folds a function from the
-- right to the left over a vector using an initial value.
--
-- >>> foldlV (-) 8 $ vector [4,2,1]   -- is the same as (((8 - 4) - 2) - 1) 
-- 1
foldlV :: (a -> b -> a) -> a -> Vector b -> a 
foldlV _ a NullV   = a
foldlV f a (x:>xs) = foldlV f (f a x) xs

-- | The higher-order functions 'foldrV' folds a function from the
-- left to the right over a vector using an initial value.
--
-- >>> foldrV (-) 8 $ vector [4,2,1]   -- is the same as (4 - (2 - (1 - 8)))
-- -5
foldrV :: (b -> a -> a) -> a -> Vector b -> a
foldrV _ a NullV   = a 
foldrV f a (x:>xs) = f x (foldrV f a xs)

-- | Reduces a vector of elements to a single element based on a
-- binary function.
--
-- >>> reduceV (+) $ vector [1,2,3,4,5]
-- 15
reduceV :: (a -> a -> a) -> Vector a -> a
reduceV _ NullV      = error "Cannot reduce a null vector"
reduceV _ (x:>NullV) = x
reduceV f (x:>xs)    = foldlV f x xs

-- | Pipes an element through a vector of functions.
--
-- >>> vector [(*2), (+1), (/3)] `pipeV` 3      -- is the same as ((*2) . (+1) . (/3)) 3
-- 4.0
pipeV :: Vector (a -> a) -> a -> a
pipeV vf = foldrV (.) id vf

-----------------------------------------------------------------------------
-- SELECTORS
-----------------------------------------------------------------------------

-- | The function 'atV' returns the n-th element in a vector, starting
-- from zero.
--
-- >>> vector [1,2,3,4,5] `atV` 3
-- 4
atV  :: (Integral a) => Vector b -> a -> b
NullV   `atV` _ = error "atV: Vector has not enough elements"
(x:>_)  `atV` 0 = x
(_:>xs) `atV` n = xs `atV` (n-1)

-- | The functions 'headV' returns the first element of a vector.
headV :: Vector a -> a
headV NullV   = error "headV: Vector is empty"
headV (v:>_) = v

-- | The function 'lastV' returns the last element of a vector.
lastV :: Vector a -> a
lastV NullV  = error "lastV: Vector is empty"
lastV (v:>NullV) = v
lastV (_:>vs)    = lastV vs

-- | The functions 'tailV' returns all, but the first element of a vector.
tailV :: Vector a  -- ^ /length/ = @la@
      -> Vector a  -- ^ /length/ = @la-1@
tailV NullV   = error "tailV: Vector is empty"
tailV (_:>vs) = vs

-- | The function 'initV' returns all but the last elements of a vector.
initV :: Vector a  -- ^ /length/ = @la@
      -> Vector a  -- ^ /length/ = @la-1@
initV NullV  = error "initV: Vector is empty"
initV (_:>NullV) = NullV
initV (v:>vs)    = v :> initV vs

-- | The function 'takeV' returns the first @n@ elements of a vector.
-- 
-- >>> takeV 2 $ vector [1,2,3,4,5]
-- <1,2>
takeV :: (Num a, Ord a)
      => a        -- ^ @= n@
      -> Vector b -- ^ /length/ = @la@
      -> Vector b -- ^ /length/ = @minimum [n,la]@
takeV 0 _       = NullV
takeV _ NullV       = NullV
takeV n (v:>vs) | n <= 0    = NullV
                | otherwise = v :> takeV (n-1) vs

-- | The function 'dropV' drops the first @n@ elements of a vector.
--
-- >>> dropV 2 $ vector [1,2,3,4,5]
-- <3,4,5>
dropV :: (Num a, Ord a)
      => a        -- ^ @= n@
      -> Vector b -- ^ /length/ = @la@
      -> Vector b -- ^ /length/ = @maximum [0,la-n]@
dropV 0 vs      = vs
dropV _ NullV       = NullV
dropV n (v:>vs) | n <= 0    = v :> vs
                | otherwise = dropV (n-1) vs

-- | The function 'selectV' selects elements in the vector based on a
-- regular stride.
selectV :: Int      -- ^ the initial element, starting from zero
        -> Int      -- ^ stepsize between elements
        -> Int      -- ^ number of elements @= n@
        -> Vector a -- ^ /length/ = @la@ 
        -> Vector a -- ^ /length/ @= n@
selectV f s n vs
  | n <= 0                 = NullV
  | (f+s*n-1) > lengthV vs = error "selectV: Vector has not enough elements"
  | otherwise              = atV vs f :> selectV (f+s) s (n-1) vs

-- | The function 'groupV' groups a vector into a vector of vectors of
-- size n.
--
-- >>> groupV 3 $ vector [1,2,3,4,5,6,7,8]
-- <<1,2,3>,<4,5,6>>
groupV :: Int               -- ^ @= n@
       -> Vector a          -- ^ /length/ = @la@ 
       -> Vector (Vector a) -- ^ /length/ = @la `div` n@ 
groupV n v 
  | lengthV v < n = NullV
  | otherwise     = selectV 0 1 n v 
                    :> groupV n (selectV n 1 (lengthV v-n) v)


-- | The higher-function 'filterV' takes a predicate function and a
-- vector and creates a new vector with the elements for which the
-- predicate is true.
--
-- >>> filterV odd $ vector [1,2,3,4,5,6,7,8]
-- <1,3,5,7>
--
-- (*) however, the length is __unknown__, because it is dependent on
-- the data contained inside the vector. Try avoiding 'filterV' in
-- designs where the size of the data is crucial.
filterV :: (a -> Bool) -- ^ predicate function
        -> Vector a    -- ^ /length/ = @la@
        -> Vector a    -- ^ /length/ @<= la@ (*)
filterV _ NullV   = NullV
filterV p (v:>vs) = if (p v)
                    then v :> filterV p vs
                    else filterV p vs

-- | Returns a vector containing all the possible prefixes of an input
-- vector.
--
-- >>> let v = vector [1,2,3,4,5,6]
-- >>> headsV v
-- <<1>,<1,2>,<1,2,3>,<1,2,3,4>,<1,2,3,4,5>,<1,2,3,4,5,6>,<1,2,3,4,5,6>>
headsV :: Vector a          -- ^ /length/ = @la@
       -> Vector (Vector a) -- ^ /length/ = @la + 1@
headsV NullV  = error "heads: null vector"
headsV v      = foldrV sel (unitV NullV) $ mapV (unitV . unitV) v
  where sel x y = x <+> mapV (lastV  x <+>) y

-- | Returns a vector containing all the possible suffixes of an input
-- vector.
--
-- >>> let v = vector [1,2,3,4,5,6]
-- >>> tailsV v
-- <<1,2,3,4,5,6>,<2,3,4,5,6>,<3,4,5,6>,<4,5,6>,<5,6>,<6>,<>>
tailsV :: Vector a          -- ^ /length/ = @la@
       -> Vector (Vector a) -- ^ /length/ = @la + 1@
tailsV NullV = NullV
tailsV v    = foldrV sel (unitV NullV) $ mapV (unitV . unitV) v
  where sel x y = mapV (<+> headV y) x <+> y

-- | Returns a stencil of @n@ neighboring elements for each possible
-- element in a vector.
--
-- >>> stencilV 3 $ vector [1..5]
-- <<1,2,3>,<2,3,4>,<3,4,5>>
stencilV :: Int               -- ^ stencil size @= n@
         -> Vector a          -- ^ /length/ = @la@ 
         -> Vector (Vector a) -- ^ /length/ = @la - n + 1@ 
stencilV n v = mapV (takeV n) $ dropFromEnd n $ tailsV v
  where dropFromEnd m = takeV (lengthV v - m + 1)

-----------------------------------------------------------------------------
-- PERMUTATORS
-----------------------------------------------------------------------------

-- |  The function 'replaceV' replaces an element in a vector.
--
-- >>> replaceV (vector [1..5]) 2 100
-- <1,2,100,4,5>
replaceV :: Vector a -- ^ input vector; /length/ = @la@
         -> Int      -- ^ position of the element to be replaced
         -> a        -- ^ new element
         -> Vector a -- ^ altered vector; /length/ = @la@
replaceV vs n x 
    | n <= lengthV vs && n >= 0 = takeV n vs <+> unitV x 
                                  <+> dropV (n+1) vs
    | otherwise                 = vs

-- | The function 'zipV' zips two vectors into a vector of tuples.
zipV   :: Vector a      -- ^ /length/ = @la@ 
       -> Vector b      -- ^ /length/ = @lb@ 
       -> Vector (a, b) -- ^ /length/ = @minimum [la,lb]@ 
zipV (x:>xs) (y:>ys) = (x, y) :> zipV xs ys
zipV _   _   = NullV

-- | The function 'unzipV' unzips a vector of tuples into two vectors.
unzipV :: Vector (a, b)        -- ^ /length/ = @la@
       -> (Vector a, Vector b) -- ^ /length/ = @la@
unzipV NullV           = (NullV, NullV)
unzipV ((x, y) :> xys) = (x:>xs, y:>ys) 
  where (xs, ys) = unzipV xys

-- | The function 'shiftlV' shifts a value from the left into a vector.
--
-- >>> vector [1..5] `shiftlV` 100
-- <100,1,2,3,4>
shiftlV :: Vector a -> a -> Vector a 
shiftlV vs v = v :> initV vs

-- | The function 'shiftrV' shifts a value from the right into a vector. 
--
-- >>> vector [1..5] `shiftrV` 100
-- <2,3,4,5,100>
shiftrV :: Vector a -> a -> Vector a
shiftrV vs v = tailV vs <: v

-- | The function 'rotlV' rotates a vector to the left. Note that this
-- fuctions does not change the size of a vector.
--
-- >>> rotlV $ vector [1..5]
-- <5,1,2,3,4>
rotlV   :: Vector a -> Vector a
rotrV NullV = NullV
rotrV vs    = tailV vs <: headV vs

-- | The function 'rotrV' rotates a vector to the right. Note that
-- this fuction does not change the size of a vector.
--
-- >>> rotrV $ vector [1..5]
-- <2,3,4,5,1>
rotrV   :: Vector a -> Vector a
rotlV NullV = NullV
rotlV vs    = lastV vs :> initV vs

-- | The function 'rotateV' rotates a vector based on an index offset.
--
-- * @(> 0)@ : rotates the vector left with the corresponding number
-- of positions.
--
-- * @(= 0)@ : does not modify the vector.
--
-- * @(< 0)@ : rotates the vector right with the corresponding number
-- of positions.
rotateV :: Int -> Vector a -> Vector a
rotateV n
  | n > 0     = pipeV (copyV (abs n) rotlV)
  | n < 0     = pipeV (copyV (abs n) rotrV)
  | otherwise = id

-- | The function 'concatV' transforms a vector of vectors to a single vector. 
concatV   :: Vector (Vector a) -> Vector a
concatV = foldrV (<+>) NullV

-- | The function 'reverseV' reverses the order of elements in a vector. 
reverseV  :: Vector a -> Vector a
reverseV NullV   = NullV
reverseV (v:>vs) = reverseV vs <: v

-- | Performs the parallel prefix operation on a vector.
--
-- >>> scanlV (+) 0 $ vector [1,1,1,1,1,1]
-- <1,2,3,4,5,6>
scanlV    :: (a -> b -> a)  -- ^ funtion to generate next element
          -> a              -- ^ initial element
          -> Vector b       -- ^ input vector; /length/ = @l@
          -> Vector a       -- ^ output vector; /length/ = @l@ 
scanlV _ _ NullV   = NullV
scanlV f a (x:>xs) = q :> scanlV f q xs 
       where q = f a x
             
-- | Performs the parallel suffix operation on a vector.
--
-- >>> scanrV (+) 0 $ vector [1,1,1,1,1,1]
-- <6,5,4,3,2,1>
scanrV    :: (b -> a -> a)   -- ^ funtion to generate next element
          -> a               -- ^ initial element       
          -> Vector b        -- ^ input vector; /length/ = @l@
          -> Vector a        -- ^ output vector; /length/ = @l@ 
scanrV _ _ NullV  = NullV
scanrV f a (x:>NullV) = f x a :> NullV
scanrV f a (x:>xs)    = f x y :> ys 
          where ys@(y:>_) = scanrV f a xs

{-
-- | The function 'serialV' can be used to construct a serial network of processes.

--|The function \haskell{serialV} and \haskell{parallelV} can be used to construct serial and parallel networks of processes.
\begin{code}
serialV    :: Vector (a -> a) -> a -> a
parallelV  :: Vector (a -> b) -> Vector a -> Vector b
\end{code}

The functions \haskell{scanlV} and \haskell{scanrV} "scan" a function through a vector. The functions take an initial element apply a functions recursively first on the element and then on the result of the function application.
%
\begin{code}
scanlV    :: (a -> b -> a) -> a -> Vector b -> Vector a 
scanrV    :: (b -> a -> a) -> a -> Vector b -> Vector a
\end{code}

\index{scanlV@\haskell{scanlV}}
\index{scanrV@\haskell{scanrV}}

Reekie also proposed the \haskell{meshlV} and \haskell{meshrV} iterators. They are like a combination of \haskell{mapV} and \haskell{scanlV} or \haskell{scanrV}. The argument function supplies a pair of values: the first is input into the next application of this function, and the second is the output value. As an example consider the expression:
%
\begin{code}
f x y = (x+y, x+y)

s1 = vector [1,2,3,4,5]
\end{code}
%
Here \haskell{meshlV} can be used to calculate the running sum. 
%
\begin{verbatim}
Vector> meshlV f 0 s1
(15,<1,3,6,10,15>)
\end{verbatim}

\begin{code}
meshlV    :: (a -> b -> (a, c)) -> a -> Vector b -> (a, Vector c)
meshrV    :: (a -> b -> (c, b)) -> b -> Vector a -> (Vector c, b)
\end{code}

\index{meshlV@\haskell{meshlV}}
\index{meshrV@\haskell{meshrV}}
-}


{-
serialV  fs  x = serialV' (reverseV fs ) x
  where
    serialV' NullV   x = x
    serialV' (f:>fs) x = serialV fs (f x)


parallelV NullV   NullV   = NullV
parallelV _  NullV   
   = error "parallelV: Vectors have not the same size!"
parallelV NullV  _   
   = error "parallelV: Vectors have not the same size!"
parallelV (f:>fs) (x:>xs) = f x :> parallelV fs xs

meshlV _ a NullV   = (a, NullV)
meshlV f a (x:>xs) = (a'', y:>ys) 
       where (a', y)   = f a x
         (a'', ys) = meshlV f a' xs

meshrV _ a NullV    = (NullV, a)
meshrV f a (x:>xs)  = (y:>ys, a'') 
        where (y, a'') = f x a'
          (ys, a') = meshrV f a xs
-}

