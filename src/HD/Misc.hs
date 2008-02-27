{-# OPTIONS_GHC -fglasgow-exts #-}
-- due to (+!)

-- Miscellaneous helper functions and types

module HD.Misc (module HD.Misc,
                module Control.Monad.Error) where

import Data.Maybe (isNothing, fromJust)
import Control.Monad.Error 

-- Error prone type (Error Monad)

type EProne a = Either String a

-- Append a string to the error and throw it back again
-- (used to pass error messages)
(+!) :: MonadError String m => m a -> String -> m a
(+!) ep str = catchError ep (\e -> throwError $ str ++ ": " ++ e)  

infix 9 +!

-- Show the a final error or return the inner type

fromEProne :: String -> EProne a  -> a
fromEProne str (Left err ) = error $ "Error: " ++ str ++ ": " ++ err
fromEProne _   (Right val) = val

-- General functions without a standard equivalent in Haskell libraries

initLast :: [a] -> ([a],a)
-- obtain init and last in just one traversal
initLast xs
 | isNothing maybeIL = error "initLast: emptyList"
 | otherwise         = fromJust maybeIL
  where maybeIL = safeInitLast xs

safeInitLast :: [a] -> Maybe ([a],a)
-- obtain init and last safely in one traversal
safeInitLast []  = Nothing
safeInitLast (x:xs)  =  Just $ go [] x xs
  where go accum current []     = (accum,current)
        go accum current (x:xs) = go (x:accum) x xs 

