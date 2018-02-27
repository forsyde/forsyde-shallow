-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.Core.Signal
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the shallow-embedded 'Signal' datatype and
-- functions operating on it.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Core.Signal(
  Signal (NullS, (:-)), (-:), (+-+), (!-), 
  signal, fromSignal,
  unitS, nullS, headS, tailS, atS, takeS, dropS,
  lengthS, infiniteS, copyS, selectS, writeS, readS, fanS,
  foldrS, allS
  ) where

infixr 5    :-
infixr 5    -:
infixr 5    +-+
infixr 5    !-


-- | A signal is defined as a list of events. An event has a tag and a value. The tag of an event is defined by the position in the list. A signal is defined as an instance of the classes 'Read' and 'Show'. The signal 1 :- 2 :- NullS is represented as \{1,2\}.
data Signal a = NullS
      | a :- Signal a deriving (Eq)

-- | The function 'signal' converts a list into a signal.
signal     :: [a] -> Signal a 

-- | The function 'fromSignal' converts a signal into a list.
fromSignal     :: Signal a -> [a]

-- | The function 'unitS' creates a signal with one value.
unitS      :: a -> Signal a

-- | The function 'nullS' checks if a signal is empty.
nullS      :: Signal a -> Bool

-- | The function 'headS' gives the first value - the head -  of a signal.
headS      :: Signal a -> a

-- | The function 'tailS' gives the rest of the signal - the tail.
tailS      :: Signal a -> Signal a

-- | The function 'atS'  returns the n-th event in a signal. The numbering of events in a signal starts with 0. There is also an operator version of this function, '(!-)'.
atS        :: Int -> Signal a -> a

-- | The function 'takeS' returns the first n values of a signal.
takeS      :: Int -> Signal a -> Signal a

-- | The function 'dropS' drops the first $n$ values from a signal.
dropS      :: Int -> Signal a -> Signal a

-- | The function 'selectS' takes three parameters, an offset, a stepsize and a signal and returns some elements of the signal such as in the following example:
--
-- @
-- Signal> selectS 2 3 (signal[1,2,3,4,5,6,7,8,9,10])
-- {3,6,9} :: Signal Integer
-- @
selectS        :: Int -> Int -> Signal a -> Signal a

-- | The function 'lengthS' returns the length of a 'finite' signal.
lengthS        :: Signal b -> Int

-- | The function 'infiniteS' creates an infinite signal. The first argument 'f' is a function that is applied on the current value. The second argument 'x' gives the first value of the signal.
--
-- > Signal> takeS 5 (infiniteS (*3) 1)
-- > {1,3,9,27,81} :: Signal Integer
--
infiniteS      :: (a -> a) -> a -> Signal a

-- | The function 'writeS' transforms a signal into a string of the following format:
--
-- @ 
-- Signal> writeS (signal[1,2,3,4,5])
-- "1\n2\n3\n4\n5\n" :: [Char]
-- @
writeS     :: Show a => Signal a -> [Char]

-- | The function 'readS' transforms a formatted string into a signal.
--
-- @
-- Signal> readS "1\n2\n3\n4\n5\n" :: Signal Int
-- {1,2,3,4,5} :: Signal Int
-- @
readS      :: Read a => [Char] -> Signal a

-- | The operator '-:' adds at an element to a signal at the tail.
(-:)       :: Signal a -> a -> Signal a

-- | The operator '+-+' concatinates two signals into one signal.  
(+-+)      :: Signal a -> Signal a -> Signal a 


-- | The function 'copyS' creates a signal with n values 'x'.
copyS :: (Num a, Eq a) => a -> b -> Signal b


-- | The combinator 'fanS' takes two processes 'p1' and 'p2' and and generates a process network, where a signal is split and processed by the processes 'p1' and 'p2'.
fanS :: (Signal a -> Signal b) -> (Signal a -> Signal c) 
      -> Signal a -> (Signal b, Signal c)

-- | Folds all events in a signal to one value based on a reduction
-- function.
foldrS :: (t -> p -> p) -> p -> Signal t -> p

-- | Checks if all events in a signal are satisfying a predicate
-- function.
allS :: (a -> Bool) -> Signal a -> Bool

-- Implementation

instance (Show a) => Show (Signal a) where
  showsPrec p NullS = showParen (p > 9) (showString "{}")
  showsPrec p xs    = showParen (p > 9) (showChar '{' . showSignal1 xs)
    where
      showSignal1 NullS      = showChar '}'
      showSignal1 (y:-NullS) = shows y . showChar '}'
      showSignal1 (y:-ys)    = shows y . showChar ',' . showSignal1 ys

instance Read a => Read (Signal a) where
  readsPrec _ s = readsSignal s

readsSignal    :: (Read a) => ReadS (Signal a)
readsSignal s
  =  [((x:-NullS), rest)
     | ("{", r2)   <- lex s,
       (x, r3)     <- reads r2,
       ("}", rest) <- lex r3]
     ++ [(NullS, r4)       
        | ("{", r5) <- lex s,
          ("}", r4) <- lex r5]
     ++ [((x:-xs), r6)     
        | ("{", r7) <- lex s,
          (x, r8)   <- reads r7,
          (",", r9) <- lex r8,
          (xs, r6)  <- readsValues r9]

readsValues    :: (Read a) => ReadS (Signal a)
readsValues s
  =  [((x:-NullS), r1) 
     | (x, r2)   <- reads s,
       ("}", r1) <- lex r2]
     ++ [((x:-xs), r3)    
        | (x, r4)   <- reads s,
          (",", r5) <- lex r4,
          (xs, r3)  <- readsValues r5]

signal []          =  NullS
signal (x:xs)      =  x :- signal xs 

fromSignal NullS   =  []
fromSignal (x:-xs) =  x : fromSignal xs

unitS x =  x :- NullS

nullS NullS =  True
nullS _     =  False

headS NullS  = error "headS : Signal is empty"
headS (x:-_) = x

tailS NullS   = error "tailS : Signal is empty"
tailS (_:-xs) = xs

atS _ NullS   = error "atS: Signal has not enough elements"
atS 0 (x:-_)  = x
atS n (_:-xs) = atS (n-1) xs

(!-) :: Signal a -> Int -> a
(!-) xs n = atS n xs

takeS 0 _      = NullS
takeS _ NullS  = NullS
takeS n (x:-xs)
  | n <= 0     = NullS
  | otherwise  = x :- takeS (n-1) xs

dropS 0 NullS  = NullS
dropS _ NullS  = NullS 
dropS n (x:-xs)
  | n <= 0     = x:-xs
  | otherwise  = dropS (n-1) xs


selectS offset step xs = select1S step (dropS offset xs) 
  where
    select1S _  NullS   = NullS
    select1S st (y:-ys) = y :- select1S st (dropS (st-1) ys) 

(-:) xs x = xs +-+ (x :- NullS)

(+-+) NullS   ys = ys
(+-+) (x:-xs) ys = x :- (xs +-+ ys)

lengthS NullS   = 0
lengthS (_:-xs) = 1 + lengthS xs

infiniteS f x = x :- infiniteS f (f x)

copyS 0 _ = NullS
copyS n x = x :- copyS (n-1) x

fanS p1 p2 xs = (p1 xs, p2 xs)

writeS NullS   = []
writeS (x:-xs) = show x ++ "\n" ++ writeS xs

readS xs             = readS' (words xs)
  where
    readS' []        = NullS
    readS' ("\n":ys) = readS' ys
    readS' (y:ys)    = read y :- readS' ys

foldrS k z = go
  where
    go NullS   = z
    go (y:-ys) = y `k` go ys

allS p = foldrS (\a prev -> p a && prev) True









