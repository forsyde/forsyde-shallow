-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.TypeLib
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides basic functions related to Template-Haskell's 'Type'.
-- 
-----------------------------------------------------------------------------
module Language.Haskell.TH.TypeLib 
 (Context,
  mkContext,
  monoContext,
  isPoly,
  contextVarNames,
  contextConstraints,
  mkForallT, 
  unArrowT, 
  unAppT,
  (-->),
  reAppT,
  reArrowT,
  dynTHType,
  thTypeOf,
  typeRep2Type,
  tyCon2Type) 
 where

import Data.Dynamic
import Data.Typeable
import Language.Haskell.TH (Type(..), Cxt, Name, pprint, mkName)
import Text.Regex.Posix ((=~))

-----------
-- Context
-----------

-- | A 'Context' represents the forall part and constraints of a type
--   (see 'ForallT')
--  For instance, the context of the type
--  @
--  forall a b. (Show a, Show b) => (a,b)
--  @
--  is @forall a b. (Show a, Show b) =>@
--  where @a@ and @b@ are the the context variables and  
--  @(Show a, Show b)@ are the context constraints 
data Context = Context 
                   [Name] -- Variable names 
                   Cxt    -- Constraints (the context itself)

instance Show Context where
-- FIXME: this is really ugly, refactor and improve its look
 showsPrec _ (Context n cxt) = 
   showVars n . showConstraints cxt 
   where showVars n = showForall (not (null n))  (showVars' n)
         showVars' (n:ns) = shows n . showChar ' ' . showVars' ns
         showVars' []   = id
         showConstraints c = (\s -> if not (null c) then ' ':s else s).
                             showParen (length c > 1) (showConstraints' c) .
                             (\s -> if not (null c) then s ++ " =>" else s)
         showConstraints' [c]    = shows c
         showConstraints' (c:cx) = showString (pprint c) . showString ", " . 
                                   showConstraints' cx
         showConstraints' []    = id
         showForall b s = if b then showString "forall " . s . showChar '.' 
                               else s

-- | 'Context' constructor
mkContext :: [Name] -> Cxt -> Context
mkContext n c = Context n c

-- | Empty context for monomorphic types
monoContext :: Context
monoContext = Context [] []
 
-- | Tells if the context corresponds to a polymorphic type
isPoly :: Context -> Bool
isPoly (Context [] _) = False
isPoly _              = True

-- | Returns the variable names related to a context
contextVarNames :: Context -> [Name]
contextVarNames (Context n _) = n

-- | Returns the context constraints
contextConstraints :: Context -> Cxt
contextConstraints (Context _ cxt) = cxt

-- | Builds a 'ForallT' type out of a context and a type
mkForallT :: Context -> Type -> Type
mkForallT (Context n cxt) t = ForallT n cxt t

--------------------------------
-- Functions to observe a 'Type'
--------------------------------

-- | Obtains the arguments and return type of a given 'Type' 
--   (normally a function)
--   together with its 'Context' (non-empty if the type is polymorphic)
unArrowT :: Type                    -- ^ Type to observe  
        ->  ([Type], Type, Context) -- ^ (args 'Type', ret 'Type', 'Context')
unArrowT (ForallT names cxt t) = let (args,ret) = unArrowT' t 
                                 in (args, ret, Context names cxt)
unArrowT t = let (args,ret) = unArrowT' t 
             in (args, ret, Context [] [])

-- unArrowT for non-Forall Types
unArrowT' :: Type -> ([Type], Type)
unArrowT' ((ArrowT `AppT` first) `AppT` rest) = let (args, ret) = unArrowT' rest
                                                in  (first:args, ret)
unArrowT' t = ([],t)

-- | Obtains the type constructor of a 'Type' together with its
--   arguments and its 'Context' (non-empty if the type is polymorphic)
unAppT :: Type                    -- ^ Type to observe
       -> (Type, [Type], Context) -- ^ (Constructor, args 'Type', Context)
unAppT (ForallT names cxt t) = let (cons, args)  = unAppT' t
                               in (cons, args, Context names cxt)
unAppT t = let (cons, args)  = unAppT' t
           in (cons, args, Context [] [])

-- unAppT for non-Forall Types 
unAppT' :: Type -> (Type, [Type])
unAppT' t = (first,rest)
  where first:rest = unAppT'ac [] t
        -- Since the constructor is a leaf of the Type tree representation,
        -- it is the last element to be gathered and thus, an accumulator
        -- is used to reverse the list to be returned
        unAppT'ac :: [Type] -> Type -> [Type]
        unAppT'ac acum (prefix `AppT` lastarg) = unAppT'ac (lastarg:acum) prefix
        unAppT'ac acum cons                   = cons:acum
------------------------------
-- Functions to build a 'Type'
------------------------------

-- | Form a function type out of two types
(-->) :: Type -- ^ Argument type
      -> Type -- ^ Return type
      -> Type -- ^ Resulting function
arg --> ret = (ArrowT `AppT` arg) `AppT` ret

-- | Rebuild a type out of a constructor, its argument types and its context
--   (inverse of 'unAppT')
reAppT :: (Type, [Type], Context)  -- ^ (Constructor, type arguments, context)
       -> Type                     -- ^ resulting 'Type'
-- Polymorphic types
reAppT (cons, args, cxt) | isPoly cxt = 
 mkForallT cxt (reAppT (cons, args, monoContext)) 
-- Monomorphic types
reAppT (cons, args, _) = foldl1 AppT (cons:args)

-- | Rebuild a function type out of its argument types, return type
--   and context (inverse of 'ArrowT')
reArrowT :: ([Type], Type, Context)  -- ^ (Constructor, type arguments, context)
           -> Type                   -- ^ resulting 'Type'
-- Polymorphic types
reArrowT (args, ret, cxt) | isPoly cxt = 
 mkForallT cxt (reArrowT (args, ret, monoContext)) 
-- Monomorphic types
reArrowT (args, ret, _) = foldr1 (-->) (args ++ [ret])

  

-------------------------------------------------------------------
-- Transforming Data.Typeable.TypeRep into Language.Haskell.TH.Type
-------------------------------------------------------------------

dynTHType :: Dynamic -> Type
dynTHType = typeRep2Type . dynTypeRep

-- | Give the template haskell 'Type' of a Typeable object
thTypeOf :: Typeable a => a -> Type
thTypeOf = typeRep2Type . typeOf

-- | Translate a 'TypeRep' to a Template Haskell 'Type'
typeRep2Type :: TypeRep -> Type
typeRep2Type rep = let (con, reps) = splitTyConApp rep
  in reAppT (tyCon2Type con, map typeRep2Type reps, monoContext)
 
-- | Gives the corresponding Template Haskell 'Type' of a 'TyCon'
tyCon2Type :: TyCon -> Type
tyCon2Type = tyConStr2Type . tyConString


----------------------------
-- Internal Helper Functions
----------------------------

tyConStr2Type :: String -> Type
-- NOTE: The tyCon strings of basic types are not qualified and buggy in 
-- some cases.
-- See http://hackage.haskell.org/trac/ghc/ticket/1841
-- FIXME: update this function whenever the bug is fixed
-- FIXME FIXME: This code is incorrect:
-- mkName doesn't generate global names! ''Maybe /= mkName "Data.Maybe.Maybe"
tyConStr2Type "->" = ArrowT
tyConStr2Type  tupStr | tupStr =~ "^,+$" = 
 ConT (mkName $ "Data.Tuple.(" ++ tupStr ++ ")")   
tyConStr2Type str  = ConT $ mkName str