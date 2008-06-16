-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.OSharing
-- Copyright   :  (c) The ForSyDe team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides support for Observable Sharing: <http://www.cs.chalmers.se/~koen/pubs/entry-asian99-lava.html>
-- 
-- It provides:
--   'URef': Unsafe Unmutable references, using them causes side effects.
--   'URefTable': a table were 'URef's are used as key and can store any value.
-- 
--    
-- /This Module was taken from Lava2000's/ @Ref.hs@ /module/: <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------
module ForSyDe.OSharing
  ( -- Unsafe references
    URef, 
    newURef,       
    readURef,     
    -- Tables of Unsafe References (IO version)
    URefTableIO,   
    newURefTableIO,   
    addEntryIO,  
    queryIO,
    -- Tables of Unsafe References (IO version)
    URefTableST,
    newURefTableST,
    addEntryST,
    queryST,
    -- Memoizating functions
    memoURef,
    memoURefIO,
    memoURefST)
 where

import ForSyDe.OSharing.UDynamic

import System.IO (fixIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.ST


-- FIXME FIXME FIXME: Add pragmas to avoid breaking optimizations in the
-- functions where unsafe* functions are used.

-- | An Unsafe Unmutable Reference to a value of type a 
--
-- An 'URef' is implemented as follows: it has two pieces of information. The
-- first one is an updatable list of entries for each table it is a member in.
-- Since the types of the Tables vary, the URef has no idea what type of
-- values it is supposed to store. So we use dynamic types.
--
-- Since it is an updatable list, it is an IORef, which we also use to compare
-- two 'URef's. The second part is just the value the URef is pointing at (this
-- can never change anyway since references are unmutable).
--
-- FIXME: why IORef? it should maybe be STRef?
--        Besides: Is it guaranteed that the values of an IORef
--        are not changed by the garbage collector?
data URef a
  = URef (IORef [(URefTableTag, UDynamic)]) a

instance Eq (URef a) where
  URef r1 _ == URef r2 _ = r1 == r2

instance Show a => Show (URef a) where
  showsPrec _ (URef _ a) = showChar '{' . shows a . showChar '}'


-- | Create a new Unsafe Unmutable Reference to a Haskell object
--
--   This operation, as explained in the Observable sharing paper, can cuase
--   side-effects, since the value returned (the 'URef') is not determined by
--   the arguments of the function (i.e. different calls to 'newURef' with
--   the same argument return different 'URef's). 
-- 
-- FIXME: the side effect caused can break optimizations in GHC check what has
-- to be done here and the rest of the code (e.g pragma noinline)
newURef :: a -> URef a
newURef a = unsafePerformIO $
  do r <- newIORef []
     return (URef r a)

-- | Read the value pointed by the 'URef'.
readURef :: URef a -> a
readURef (URef _ a) = a

--------------
-- URefTableIO 
--------------

-- | A unique identifier which univocally designates a table 
type URefTableTag
  = IORef ()


-- | A 'URefTable' a b 
-- 
-- * Unsafe References to a value of type "a" (key of the table)
-- * Values of type "b" associated to each key
-- 
-- Here is how we implement Tables of URefs:
--
-- A Table is nothing but a unique tag, of type TableTag.  TableTag can be
-- anything, as long as it is easy to create new ones, and we can compare them
-- for equality. (I chose IORef ()).
--
-- So how do we store URefs in a Table? We do not want the Tables keeping
-- track of their URefs (which would be disastrous when the table becomes big,
-- and we would not have any garbage collection).
--
-- Instead, every URef keeps track of the value it has in each table it is
-- in. This has the advantage that we have a constant lookup time (if the
-- number of Tables we are using is small), and we get garbage collection of
-- table entries for free.
newtype URefTableIO a b
  = URefTableIO URefTableTag
 deriving Eq

-- | Create a new table
newURefTableIO :: IO (URefTableIO a b)
newURefTableIO = URefTableIO `fmap` newIORef ()

-- | Query the value corresponding to an 'URef' 
queryIO :: URefTableIO a b -> URef a -> IO (Maybe b)
queryIO (URefTableIO t) (URef r _) =
  do list <- readIORef r
     return (unsafeFromUDyn `fmap` lookup t list)

-- | Add an ('URef' a, b) pair entry to the table 
addEntryIO ::  URefTableIO a b -- ^ key of the entry  
         -> URef a 
         -> b             -- ^ value of the entry 
         -> IO () 
addEntryIO (URefTableIO t) (URef r _) b =
  do list <- readIORef r
     writeIORef r ((t, unsafeToUDyn b) : filter ((/= t) . fst) list)

--------------
-- URefTableST
--------------

-- | 'ST' version of 'URefTableIO'
newtype URefTableST s a b
  = URefTableST (URefTableIO a b)
 deriving Eq

-- | 'ST' version of 'newURefTableIO'
newURefTableST :: ST s (URefTableST s a b)
newURefTableST = unsafeIOToST (URefTableST `fmap` newURefTableIO)

-- | 'ST' version of 'queryIO'
queryST :: URefTableST s a b -> URef a -> ST s (Maybe b)
queryST (URefTableST tab) r = unsafeIOToST (queryIO tab r)

-- | 'ST' version of 'addEntryIO'
addEntryST :: URefTableST s a b -> URef a -> b -> ST s ()
addEntryST (URefTableST tab) r b = unsafeIOToST (addEntryIO tab r b)

--------------------------------
-- Memoization of URef functions
--------------------------------

-- | Generates a memoizated version of a function taking 'URef' values 
memoURef :: (URef a -> b) -> (URef a -> b)
memoURef f = unsafePerformIO . memoURefIO (return . f)

-- | 'IO' version of 'memoURef'
memoURefIO :: (URef a -> IO b) -> (URef a -> IO b)
memoURefIO f = unsafePerformIO $
  do tab <- newURefTableIO
     let f' r = do mb <- queryIO tab r
                   case mb of
                     Just b  -> do return b
                     Nothing -> fixIO $ \b ->
                                  do addEntryIO tab r b
                                     f r
     return f'

-- | 'ST' version of 'memoURef'
memoURefST :: (URef a -> ST s b) -> (URef a -> ST s b)
memoURefST f = unsafePerformST $
  do tab <- newURefTableST
     let f' r = do mb <- queryST tab r
                   case mb of
                     Just b  -> do return b
                     Nothing -> fixST $ \b ->
                                  do addEntryST tab r b
                                     f r
     return f'
 where unsafePerformST = unsafePerformIO . unsafeSTToIO