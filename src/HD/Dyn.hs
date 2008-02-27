{-# OPTIONS_GHC -fglasgow-exts #-}
module HD.Dyn where
import GHC.Prim (unsafeCoerce#)
-- FIXME: remove this (use Data.Dynamic or at least rename it to 
-- dynamic (maybe including the types, Dyn sucks
data Dyn = Dyn

toDyn :: a -> Dyn
toDyn = unsafeCoerce#

fromDyn :: Dyn -> a
fromDyn = unsafeCoerce#
