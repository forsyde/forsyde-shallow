-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Core.Vector
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the data type 'Vector' and the
-- corresponding functions. It is a development of the module
-- defined by Reekie.  Though the vector is modeled as a list, it
-- should be viewed as an array, i.e. a vector has a fixed
-- size. Unfortunately, it is not possible to have the size of the
-- vector as a parameter of the vector data type, due to restrictions
-- in Haskells type system. Still most operations are defined for
-- vectors with the same size.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.Vector ( 
  Vector (..), vector, fromVector, unitV, nullV, lengthV,
  atV, replaceV, headV, tailV, lastV, initV, takeV, dropV, 
  selectV, groupV, (<+>), (<:), mapV, foldlV, foldrV,
  reduceV, pipeV, zipWithV, filterV, zipV, unzipV, 
  -- scanlV, scanrV, meshlV, meshrV, 
  concatV, reverseV, shiftlV, shiftrV, rotrV, rotlV, rotateV,
  generateV, iterateV, copyV --, serialV, parallelV 
  ) where

infixr 5 :>
infixl 5 <:
infixr 5 <+>

-- | The data type 'Vector' is modeled similar to a list. It has two data type constructors. 'NullV' constructs the empty vector, while ':>' constructsa vector by adding an value to an existing vector. Using the inheritance mechanism of Haskell we have declared 'Vector' as an instance of the classes 'Read' and 'Show'.
--
-- | This means that the vector 1:>2:>3:>NullV is shown as <1,2,3>.
data Vector a = NullV
              | a :> (Vector a) deriving (Eq)

-- | The function 'vector' converts a list into a vector.
vector     :: [a] -> Vector a

-- | The function 'fromVector' converts a vector into a list.
fromVector :: Vector a -> [a]

-- | The function 'unitV' creates a vector with one element. 
unitV :: a -> Vector a

-- | The function 'nullV' returns 'True' if a vector is empty. 
nullV :: Vector a -> Bool

-- | The function 'lengthV' returns the number of elements in a value. 
lengthV :: Vector a -> Int

-- | The function 'atV' returns the n-th element in a vector, starting from zero.
atV  :: (Num a, Eq a) => Vector b -> a -> b

-- |  The function 'replaceV' replaces an element in a vector.
replaceV :: Vector a -> Int -> a -> Vector a

-- | The functions 'headV' returns the first element of a vector.
headV :: Vector a -> a

-- | The function 'lastV' returns the last element of a vector.
lastV :: Vector a -> a

-- | The functions 'tailV' returns all, but the first element of a vector.
tailV :: Vector a -> Vector a 

-- | The function 'initV' returns all but the last elements of a vector.
initV :: Vector a -> Vector a 

-- | The function 'takeV' returns the first n elements of a vector.
takeV :: (Num a, Ord a) => a -> Vector b -> Vector b

-- | The function 'dropV' drops the first n elements of a vector.
dropV :: (Num a, Ord a) => a -> Vector b -> Vector b

-- | The function 'selectV' selects elements in the vector. The first argument gives the initial element, starting from zero, the second argument gives the stepsize between elements and the last argument gives the number of elements. 
selectV :: Int -> Int -> Int -> Vector a -> Vector a

-- | The function 'groupV' groups a vector into a vector of vectors of size n.
groupV :: Int -> Vector a -> Vector (Vector a)

-- | The operator '(<:)' adds an element at the end of a vector.
(<:)  :: Vector a -> a -> Vector a

-- | The operator '(<+>)' concatinates two vectors.
(<+>) :: Vector a -> Vector a -> Vector a


-- | The higher-order function 'mapV' applies a function on all elements of a vector.
mapV :: (a -> b) -> Vector a -> Vector b    

-- | The higher-order function 'zipWithV' applies a function pairwise on to vectors.
zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c

-- | The higher-order functions 'foldlV' folds a function from the right to the left  over a vector using an initial value.
foldlV :: (a -> b -> a) -> a -> Vector b -> a 

-- | The higher-order functions 'foldrV' folds a function from the left to the right over a vector using an initial value.
foldrV :: (b -> a -> a) -> a -> Vector b -> a

-- | Reduces a vector of elements to a single element based on a binary function. 
reduceV :: (a -> a -> a) -> Vector a -> a

-- | Pipes an element through a vector of functions. For example the code
--
-- >>> pipeV [(*2), (+1), (/3)] 3
-- > 2
--
-- is equivalent to
--
-- >>> ((*2) . (+1) . (/3)) 3
-- > 2
pipeV :: Vector (a -> a) -> a -> a

-- | The higher-function 'filterV' takes a predicate function and a vector and creates a new vector with the elements for which the predicate is true. 
filterV :: (a -> Bool) -> Vector a -> Vector a

-- | The function 'zipV' zips two vectors into a vector of tuples.
zipV   :: Vector a -> Vector b -> Vector (a, b)

-- | The function 'unzipV' unzips a vector of tuples into two vectors.
unzipV :: Vector (a, b) -> (Vector a, Vector b)

-- | The function 'shiftlV' shifts a value from the left into a vector. 
shiftlV :: Vector a -> a-> Vector a 

-- | The function 'shiftrV' shifts a value from the right into a vector. 
shiftrV :: Vector a -> a -> Vector a

-- | The function 'rotlV' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
rotlV   :: Vector a -> Vector a

-- | The function 'rotrV' rotates a vector to the right. Note that this fuction does not change the size of a vector.
rotrV   :: Vector a -> Vector a

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

-- | The function 'concatV' transforms a vector of vectors to a single vector. 
concatV   :: Vector (Vector a) -> Vector a

-- | The function 'reverseV' reverses the order of elements in a vector. 
reverseV  :: Vector a -> Vector a

-- | The function 'iterateV' generates a vector with a given number of elements starting from an initial element using a supplied function for the generation of elements. 
--
-- > Vector> iterateV 5 (+1) 1
--
-- > <1,2,3,4,5> :: Vector Integer
iterateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- | The function 'generateV' behaves in the same way, but starts with the application of the supplied function to the supplied value. 
--
-- > Vector> generateV 5 (+1) 1
-- 
-- > <2,3,4,5,6> :: Vector Integer
generateV :: (Num a, Eq a) => a -> (b -> b) -> b -> Vector b

-- | The function 'copyV' generates a vector with a given number of copies of the same element. 
--
-- > Vector> copyV 7 5 
-- 
-- > <5,5,5,5,5,5,5> :: Vector Integer
copyV     :: (Num a, Eq a) => a -> b -> Vector b

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

instance (Show a) => Show (Vector a) where
   showsPrec p NullV = showParen (p > 9) (
             showString "<>")
   showsPrec p xs    = showParen (p > 9) (
             showChar '<' . showVector1 xs)
           where
          showVector1 NullV
             = showChar '>'            
          showVector1 (y:>NullV) 
             = shows y . showChar '>'
          showVector1 (y:>ys)    
             = shows y . showChar ',' 
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

vector []     = NullV
vector (x:xs) = x :> (vector xs)

fromVector NullV   = []
fromVector (x:>xs) = x : fromVector xs

unitV x = x :> NullV

nullV NullV   = True
nullV _     = False

lengthV NullV   = 0
lengthV (_:>xs) = 1 + lengthV xs

replaceV vs n x 
    | n <= lengthV vs && n >= 0 = takeV n vs <+> unitV x 
          <+> dropV (n+1) vs
    | otherwise         =  vs

NullV   `atV` _ = error "atV: Vector has not enough elements"
(x:>_)  `atV` 0 = x
(_:>xs) `atV` n = xs `atV` (n-1)

headV NullV   = error "headV: Vector is empty"
headV (v:>_) = v

tailV NullV   = error "tailV: Vector is empty"
tailV (_:>vs) = vs

lastV NullV  = error "lastV: Vector is empty"
lastV (v:>NullV) = v
lastV (_:>vs)    = lastV vs

initV NullV  = error "initV: Vector is empty"
initV (_:>NullV) = NullV
initV (v:>vs)    = v :> initV vs

takeV 0 _       = NullV
takeV _ NullV       = NullV
takeV n (v:>vs) | n <= 0    = NullV
        | otherwise = v :> takeV (n-1) vs

dropV 0 vs      = vs
dropV _ NullV       = NullV
dropV n (v:>vs) | n <= 0    = v :> vs
        | otherwise = dropV (n-1) vs

selectV f s n vs | n <= 0       
         = NullV
         | (f+s*n-1) > lengthV vs 
        = error "selectV: Vector has not enough elements"
         | otherwise    
        = atV vs f :> selectV (f+s) s (n-1) vs

groupV n v 
  | lengthV v < n = NullV
  | otherwise     = selectV 0 1 n v 
      :> groupV n (selectV n 1 (lengthV v-n) v)

NullV <+> ys   = ys
(x:>xs) <+> ys = x :> (xs <+> ys) 

xs <: x = xs <+> unitV x     

mapV _ NullV   = NullV
mapV f (x:>xs) = f x :> mapV f xs

zipWithV f (x:>xs) (y:>ys) = f x y :> (zipWithV f xs ys)
zipWithV _ _   _   = NullV

foldlV _ a NullV   = a
foldlV f a (x:>xs) = foldlV f (f a x) xs

foldrV _ a NullV   = a 
foldrV f a (x:>xs) = f x (foldrV f a xs)

reduceV _ NullV      = error "Cannot reduce a null vector"
reduceV _ (x:>NullV) = x
reduceV f (x:>xs)    = foldrV f x xs

pipeV = reduceV (.)

filterV _ NullV   = NullV
filterV p (v:>vs) = if (p v) then
         v :> filterV p vs
      else 
         filterV p vs

zipV (x:>xs) (y:>ys) = (x, y) :> zipV xs ys
zipV _   _   = NullV

unzipV NullV       = (NullV, NullV)
unzipV ((x, y) :> xys) = (x:>xs, y:>ys) 
       where (xs, ys) = unzipV xys

shiftlV vs v = v :> initV vs

shiftrV vs v = tailV vs <: v

rotrV NullV = NullV
rotrV vs    = tailV vs <: headV vs

rotlV NullV = NullV
rotlV vs    = lastV vs :> initV vs

rotateV n
  | n > 0     = pipeV (copyV (abs n) rotlV)
  | n < 0     = pipeV (copyV (abs n) rotrV)
  | otherwise = id


concatV = foldrV (<+>) NullV

reverseV NullV   = NullV
reverseV (v:>vs) = reverseV vs <: v

generateV 0 _ _ = NullV
generateV n f a = x :> generateV (n-1) f x 
        where x = f a

iterateV 0 _ _ = NullV
iterateV n f a = a :> iterateV (n-1) f (f a)

copyV k x = iterateV k id x 

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

scanlV _ _ NullV   = NullV
scanlV f a (x:>xs) = q :> scanlV f q xs 
       where q = f a x

scanrV _ _ NullV  = NullV
scanrV f a (x:>NullV) = f x a :> NullV
scanrV f a (x:>xs)    = f x y :> ys 
          where ys@(y:>_) = scanrV f a xs

meshlV _ a NullV   = (a, NullV)
meshlV f a (x:>xs) = (a'', y:>ys) 
       where (a', y)   = f a x
         (a'', ys) = meshlV f a' xs

meshrV _ a NullV    = (NullV, a)
meshrV f a (x:>xs)  = (y:>ys, a'') 
        where (y, a'') = f x a'
          (ys, a') = meshrV f a xs
-}






