module DELib where

infixr 5 :- 

data DEToken a = U | D a deriving (Eq, Show, Ord)

data Signal a = NullS | a :- Signal a deriving (Eq, Show)


-- delayDE without cleaning
delayDE delta s0 xs = (0, s0) :- delayDE' delta s0 xs

delayDE' delta s0 NullS       = NullS
delayDE' delta s0 ((t,v):-xs) = (t+delta,v) :- delayDE' delta s0 xs


{-
delayDE delta s0 xs = (0, s0) :- delayDE' delta s0 s0 xs

delayDE' delta s0 s NullS       = NullS
delayDE' delta s0 s ((t,v):-xs) = if v /= s then
                                     (t+delta,v) :- delayDE' delta s0 v xs
                                  else
                                     delayDE' delta s0 s xs
-}
                                     


mapDE f = mapDE' f U

mapDE' f _ NullS       = NullS
mapDE' f U ((t,v):-xs) = (t,f v) :- mapDE' f v xs
mapDE' f w ((t,v):-xs) = if (f v) == w then
                            mapDE' f w xs
                         else
                            (t, (f v)) :- mapDE' f (f v) xs 

-- Add old values on the inputs to zipWithDE!

zipWithDE f = zipWithDE' f U U U


zipWithDE' f _ _ _ NullS          NullS          = NullS
zipWithDE' f _ _ _ _              NullS          = NullS
zipWithDE' f _ _ _ NullS          _              = NullS
zipWithDE' f _ _ U ((tx, vx):-xs) ((ty, vy):-ys)
   = if tx < ty then
        zipWithDE' f vx U U xs ((ty, vy):-ys)
     else 
        if ty > tx then
           zipWithDE' f U vy U ((tx, vx):-xs) ys
        else          
           (tx, f vx vy) :- zipWithDE' f vx vy (f vx vy) xs ys
zipWithDE' f x y w ((tx, vx):-xs) ((ty, vy):-ys)
   = if tx < ty then
        zipWithDE' f vx y (f vx y) xs ((ty, vy):-ys)
     else 
        if ty > tx then
           zipWithDE' f x vy (f x vy) ((tx, vx):-xs) ys
        else          
           (tx, f vx vy) :- zipWithDE' f vx vy (f vx vy) xs ys

addDE _ U = U
addDE U _ = U
addDE (D x) (D y) = D (x + y)

incDE U = U
incDE (D x) = D (x + 1)

idDE U = U
idDE (D x) = D x

sourceDE1 = out
  where
    out = mapDE incDE s
    s   = delayDE 1 (D 0) out
 
sourceDE2 = out
  where
    out = mapDE idDE s
    s   = delayDE 1 (D 0) out

s1 = (10, D 1) :- (20, D 2) :- NullS
s2 = (15, D 10) :- (18, D 20) :- NullS 
