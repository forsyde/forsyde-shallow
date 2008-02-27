module HD.Types where

import Data.List (intersperse)

-------------------
-- Type definitions
-------------------

-- FIXME: change Int for Int32

-- Our basic type
-- It is just so cool that data constructors and type constructors have 
-- different namespaces in Haskell!!!
data HDType = Bool | Int
 deriving (Show,Eq)

-- Function types
newtype HDFunType = HDFunType [HDType]
 deriving Eq

instance Show HDFunType where
 -- FIXME: the list shouldn't be traversed 3 times to be shown! 
 show (HDFunType ts) =  concat $ intersperse " -> " $ map show ts

-- Signal Type
data HDSignalType = HDSignalType HDType
 deriving Eq


instance Show HDSignalType where
 show (HDSignalType s)= "HDSignal "++show s


-- Hardware Description Primitive Type class
-- The primitive types are the ones allowed to live inside a signal
-- A primitive type has to be able to supply a constant and its type value
-- the user will only be allowed to use types belonging to this class
-- FIMXE: the names HDType and HDPrimType don't look good
class HDPrimType a where
 toPConstant :: a -> HDPrimConst
 -- Corresponding HDSignal Type 
 sTypeOfC    :: a -> HDSignalType
 
instance HDPrimType Int where
 toPConstant i  = HDIntConst  i
 sTypeOfC _     = HDSignalType Int

instance HDPrimType Bool where
 toPConstant b  = HDBoolConst b
 sTypeOfC _     = HDSignalType Bool


------------
-- Constants
------------

data HDPrimConst = HDIntConst Int | HDBoolConst Bool


instance Show HDPrimConst where
 show (HDIntConst i)      = show i
 show (HDBoolConst b)     = show b

