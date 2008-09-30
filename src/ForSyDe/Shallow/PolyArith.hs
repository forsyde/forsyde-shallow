-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.PolyArith
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This is the polynomial arithematic library. The arithematic operations include 
-- addition, multiplication, division and power. However, the computation time is 
-- not optimized for multiplication and is O(n2), which could be considered to be 
-- optimized by FFT algorithms later on.
-----------------------------------------------------------------------------
module ForSyDe.Shallow.PolyArith(
              -- *Polynomial data type
              Poly(..),
              -- *Addition, DmMultiplication, division and power operations
              addPoly, mulPoly, divPoly, powerPoly,
              -- *Some helper functions
              getCoef, scalePoly, addPolyCoef, subPolyCoef, scalePolyCoef
    )
    where 

-- |Polynomial data type.
data Num a => Poly a = Poly [a]
                     | PolyPair (Poly a, Poly a) deriving (Eq)


-- |Multiplication operation of polynomials.
mulPoly :: Num a => Poly a -> Poly a -> Poly a
mulPoly (Poly []) _ = Poly []
mulPoly _ (Poly []) = Poly []
-- Here is the O(n^2) version of polynomial multiplication
mulPoly (Poly xs) (Poly ys) = Poly $ foldr (\y zs ->
  let (v:vs) = scalePolyCoef y xs in v :addPolyCoef vs zs) [] ys
mulPoly (PolyPair (a, b)) (PolyPair (c, d)) =
  PolyPair (mulPoly a c, mulPoly b d)
mulPoly (PolyPair (a, b)) (Poly c) =
  PolyPair (mulPoly a (Poly c), b)
mulPoly (Poly c) (PolyPair (a, b)) =
  mulPoly (PolyPair (a, b)) (Poly c)

-- |Division operation of polynomials.
divPoly :: Num a => Poly a -> Poly a -> Poly a
divPoly (Poly a) (Poly b) = PolyPair (Poly a,Poly b)
divPoly (PolyPair (a, b)) (PolyPair (c, d)) =
  mulPoly (PolyPair (a, b)) (PolyPair (d, c))
divPoly (PolyPair (a, b)) (Poly c) =
  PolyPair (a, mulPoly b (Poly c))
divPoly (Poly c) (PolyPair (a, b)) =
  PolyPair (mulPoly b (Poly c), a)

-- |Addition operations of polynomials.
addPoly :: Num a => Poly a -> Poly a -> Poly a
addPoly (Poly a) (Poly b) = Poly $ addPolyCoef a b
addPoly (PolyPair (a, b)) (PolyPair (c, d)) =
    if b==d then  -- simplifyPolyPair $
          PolyPair (addPoly a c, d)
    else  -- simplifyPolyPair $
          PolyPair (dividedPoly, divisorPoly)
  where
    divisorPoly = if b ==d then b else mulPoly b d
    dividedPoly = if b == d then addPoly a c
                  else addPoly (mulPoly a d) (mulPoly b c)
addPoly (Poly a) (PolyPair (c, d) ) =
    addPoly (PolyPair (multiPolyHelper, d)) (PolyPair (c,d) )
  where
    multiPolyHelper = mulPoly (Poly a) d
addPoly  abPoly@(PolyPair _) cPoly@(Poly _) = addPoly cPoly abPoly
 
-- |Power operation of polynomials.
powerPoly :: Num a => Poly a -> Int -> Poly a
powerPoly p n = powerX' (Poly [1]) p n
  where
    powerX' :: Num a => Poly a -> Poly a -> Int -> Poly a
    powerX' p' _ 0 = p'
    powerX' p' p n = powerX' (mulPoly p' p) p (n-1)

-- |Some helper functions below.

-- |To get the coefficients of the polynomial.
getCoef :: Num a => Poly a -> ([a],[a])
getCoef (Poly xs) = (xs,[1])
getCoef (PolyPair (Poly xs,Poly ys)) = (xs,ys)
getCoef _ = error "getCoef: Nested fractions found"

scalePoly :: (Num a) => a -> Poly a -> Poly a
scalePoly s p = mulPoly (Poly [s]) p

addPolyCoef :: Num a => [a] -> [a] -> [a]
addPolyCoef = zipWithExt (0,0) (+)
subPolyCoef :: RealFloat a => [a] -> [a] -> [a]
subPolyCoef = zipWithExt (0,0) (-)

scalePolyCoef :: (Num a) => a -> [a] -> [a]
scalePolyCoef s p = map (s*) p

-- |Extended version of 'zipWith', which will add zeros to the shorter list.
zipWithExt :: (a,b) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithExt _ _ [] [] = []
zipWithExt (x0,y0) f (x:xs) [] = f x y0 : (zipWithExt (x0,y0) f xs [])
zipWithExt (x0,y0) f [] (y:ys)  = f x0 y : (zipWithExt (x0,y0) f [] ys)
zipWithExt (x0,y0) f (x:xs) (y:ys)  = f x y : (zipWithExt (x0,y0) f xs ys)

