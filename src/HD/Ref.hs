-- FIXME, avoid using HD.Dynamic and use Data.Dynamic
module HD.Ref
  ( Ref       -- * -> * ; Eq, Show
  , ref       -- :: a     -> Ref a
  , deref     -- :: Ref a -> a
  
  , Table   -- :: * -> * -> * ; Eq
  , table   -- :: IO (Table a b)
  , extend  -- :: Table a b -> Ref a -> b -> IO ()
  , find    -- :: Table a b -> Ref a -> IO (Maybe b)

  )
 where

import System.IO
  ( fixIO
  )
import System.IO.Unsafe              
  ( unsafePerformIO
  )

import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef
  )

import HD.Dyn


---




{-

Warning! One should regard this module as a portable
extension to the Haskell language. It is not Haskell.

-}



{-

A "Table a b" of Refs contains

* References to a value of type "a" (key of the table)
* Values of type "b" associated to each key

Here is how we implement Tables of Refs:

A Table is nothing but a unique tag, of type TableTag.
TableTag can be anything, as long as it is easy
to create new ones, and we can compare them for
equality. (I chose IORef ()).

So how do we store Refs in a Table? We do not
want the Tables keeping track of their Refs
(which would be disastrous when the table
becomes big, and we would not have any garbage
collection).

Instead, every Ref keeps track of the value it
has in each table it is in. This has the advantage
that we have a constant lookup time (if the number of
Tables we are using is small), and we get garbage
collection of table entries for free.

The disadvantage is that, since the types of the
Tables vary, the Ref has no idea what type of
values it is supposed to store. So we use dynamic
types.

A Ref is implemented as follows: it has two pieces
of information. The first one is an updatable
list of entries for each table it is a member in.
Since it is an updatable list, it is an IORef, which
we also use to compare two Refs. The second part is
just the value the Ref is pointing at (this can never
change anyway).

-}

-----------------------------------------------------------------
-- Ref

-- FIXME: why IORef, it should be STRef
data Ref a
  = Ref (IORef [(TableTag, Dyn)]) a

instance Eq (Ref a) where
  Ref r1 _ == Ref r2 _ = r1 == r2

instance Show a => Show (Ref a) where
  showsPrec _ (Ref _ a) = showChar '{' . shows a . showChar '}'

-- FIXME: the side effect caused can break optimizations in GHC 
--        check what has to be done here and the rest of the code (e.g pragma noinline)
ref :: a -> Ref a
ref a = unsafePerformIO $
  do r <- newIORef []
     return (Ref r a)

deref :: Ref a -> a
deref (Ref _ a) = a

-----------------------------------------------------------------
-- Table 

type TableTag
  = IORef ()

newtype Table a b
  = Table TableTag
 deriving Eq


table :: IO (Table a b)
table = Table `fmap` newIORef ()

find :: Table a b -> Ref a -> IO (Maybe b)
find (Table t) (Ref r _) =
  do list <- readIORef r
     return (fromDyn `fmap` lookup t list)

extend :: Table a b -> Ref a -> b -> IO ()
extend (Table t) (Ref r _) b =
  do list <- readIORef r
     writeIORef r ((t,toDyn b) : filter ((/= t) . fst) list)


-----------------------------------------------------------------
-- the end.



