{-# LANGUAGE MagicHash, TemplateHaskell  #-}  
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- Due to the use of unboxed types, TH, and deprecated Packed Strings
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.TH.Lift
-- Copyright   :  (c) Ian Lynagh, 2006, (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde_dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Automatically derive Template Haskell's 'Lift' class for datatypes
-- using Template Haskell splices.
-- 
-- Based on Lynagh's th-lift package: <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/th-lift>
-----------------------------------------------------------------------------
module Language.Haskell.TH.Lift (deriveLift1, deriveLift) where

import GHC.Exts
import Data.PackedString
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad (liftM)

modName :: String
modName = "Language.Haskell.TH.Lift"

-- | Automatically generate an instance of 'Lift' for one data type. For example:
-- 
-- @
-- \{\-\# LANGUAGE TemplateHaskell \#\-\}
-- module Colour where
-- import Language.Haskell.TH.Lift
--
-- data RGB = Red | Green | Blue
--
-- \-\- Generate the Lift instance of RGB
-- \$(deriveLift1 ''RGB)
-- @
deriveLift1 :: Name -> Q [Dec]
deriveLift1 = (liftM (\a -> [a])) . deriveLift 

-- | Version of 'deriveLif1' used to automatically generate instances
--   of Lift for multiple data types. For instance:
--
-- @
-- \{\-\# LANGUAGE TemplateHaskell \#\-\}
-- module Colour where
-- import Language.Haskell.TH.Lift
--
-- data RGB = Red | Green | Blue
--
-- data Num a => Pixel a = Pixel a a a
--
-- \-\- Generate Lift instances for RGB and Pixel
-- $(mapM deriveLift [''RGB, ''Pixel])
-- @
deriveLift :: Name -> Q Dec
deriveLift n
 = do i <- reify n
      case i of
          TyConI (DataD dcxt _ vs cons _) ->
              let ctxt = liftM (++ dcxt) $ 
                         cxt  [conT ''Lift `appT` varT v | v <- vs] 
                  typ = foldl appT (conT n) $ map varT vs
                  fun = funD 'lift (map doCons cons)
              in instanceD ctxt (conT ''Lift `appT` typ) [fun]
          _ -> error (modName ++ ".deriveLift: unhandled: " ++ pprint i)

doCons :: Con -> Q Clause
doCons (NormalC c sts)
 = do let ns = zipWith (\_ i -> "x" ++ show i) sts [(0::Integer)..]
          con = [| conE c |]
          args = [ [| lift $(varE (mkName n)) |] | n <- ns ]
          e = foldl (\e1 e2 -> [| appE $e1 $e2 |]) con args
      clause [conP c (map (varP . mkName) ns)] (normalB e) []
doCons (InfixC st1 n st2) = doCons (NormalC n [st1,st2])
doCons (RecC n vsts)  
 = let st (_, s, t) = (s, t)
   in doCons (NormalC n (map st vsts))
doCons c = error (modName ++ ".doCons: Unhandled constructor: " ++ pprint c)

instance Lift Name where
    lift (Name occName nameFlavour) = [| Name occName nameFlavour |]

instance Lift PackedString where
    lift ps = [| packString $(lift $ unpackPS ps) |]

instance Lift NameFlavour where
    lift NameS = [| NameS |]
    lift (NameQ modName) = [| NameQ modName |]
    lift (NameU i) = [| case $( lift (I# i) ) of
                            I# i' -> NameU i' |]
    lift (NameL i) = [| case $( lift (I# i) ) of
                            I# i' -> NameL i' |]
    lift (NameG nameSpace pkgName modName)
     = [| NameG nameSpace pkgName modName |]

instance Lift NameSpace where
    lift VarName = [| VarName |]
    lift DataName = [| DataName |]
    lift TcClsName = [| TcClsName |]

