{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process.ProcType
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (non-standard instances)
--
-- This module includes and exports, the internal definition, instantiations
-- and related types of 'ProcType', a class used to constrain the arguments
-- taken by process constructors.
----------------------------------------------------------------------------- 
module ForSyDe.Process.ProcType (
 EnumAlgTy(..), 
 ProcType(..), 
 genTupInstances) where

import Control.Monad (replicateM)
import Data.List (intersperse)
import Data.Data
import Data.Set (Set, union)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Text.ParserCombinators.ReadP

-- | Data type describing an algebraic enumerated type (i.e. an algrebraic 
--   type whose data constructors have arity zero)
data EnumAlgTy = EnumAlgTy String [String] 
 deriving Show

instance Eq EnumAlgTy where
 (EnumAlgTy d1 _) == (EnumAlgTy d2 _) = d1 == d2

instance Ord EnumAlgTy where
 (EnumAlgTy d1 _) `compare` (EnumAlgTy d2 _) = d1 `compare` d2

-- | Class used to constrain the arguments (values and 'ProcFun's) taken by
--   process constructors
class (Data a, Lift a) => ProcType a where
 -- | Get the associated enumerated type-definitions of certain value, 
 --   taking nesting in account.  
 -- 
 --   For example:  
 --
 -- 
 -- >  module MyMod where
 -- >
 -- >  data Colour = Blue | Red
 -- >   deriving (Data, Typeable)
 -- >  data Shapes = Circle | Square
 -- >   deriving (Data, Typeable)
 -- >
 -- >  getEnums (Prst Blue, Circle) =                 
 -- >   fromList [EnumAlgTy "MyMod.Colour" ["Blue", "Red"],
 -- >             EnumAlgTy "MyMod.Shapes" ["Circle", "Square"]]
 getEnums :: a -> Set EnumAlgTy
 -- | Read a process type
 readProcType :: ReadP a

-- Function to automatically generate ProcType, Data, Lift and
-- Typeable instances for tuples (with 2 or more elements) with
-- Template Haskell. For example, in the case of 2 elements, the code
-- generated would be:
--
-- @
-- instance (ProcType o1, ProcType o2) => ProcType (o1, o2) where
--  getEnums _ = getEnums (undefined :: a) `union` getEnums (undefined :: b) 
--  readProcType = do
--            skipSpaces >> char '('
--            o1 <- readProcType
--            skipSpaces >> char ','
--            o2 <- readProcType
--            skipSpaces >> char ')'
--            return (o1,o2)
--
-- The next two are only necessary for tuples with more than 7 elements
--
-- instance (Typeable o1, Typeable o2) => Typeable (o1, o2) where
--  typeOf _ = mkTyCon "," `mkTyConApp` 
--               [typeOf (undefined :: o1), typeOf (undefined :: o2)]
--
-- instance (Data o1, Data o2) => Data (o1, o2) where
--  gfoldl k z (o1, o2) = z (,) `k` o1 `k` o2
--  gunfold k z _ = k (k (z (,) ))
--  toConstr a = mkConstr (dataTypeOf a) "(,)" [] Prefix
--  dataTypeOf a = mkDataType "Data.Tuple.(,)" [toConstr a]
--
-- FIXME: This won't be necessary once the Data a => Lift a instance is created
--
-- instance (Lift o1, Lift o2) => Lift (o1, o2) where
--  lift (o1, o2) = tupE [lift o1, lift o2]
-- @
genTupInstances :: Int -- ^ number of outputs to generate
             -> Q [Dec]
genTupInstances n = do
  -- Generate N o names
  outNames <- replicateM n (newName "o")
  let tupType = foldl accumApp (tupleT n) outNames
      accumApp accumT vName = accumT `appT` varT vName 
  if n <= 7 
     then sequence [genProcTypeIns outNames tupType]
     else sequence [genTypeableIns outNames tupType,
                    genDataIns outNames tupType]
                    -- genLiftIns outNames tupType,
                    --genProcTypeIns outNames tupType]

 where 
  undef t = sigE [| undefined |] (varT t)
  genProcTypeIns :: [Name] -> Q Type -> Q Dec
  genProcTypeIns names tupType = do
    let getEnumsExpr =  
            foldr1 (\e1 e2 -> infixE (Just e1) 
                                     (varE 'union)
                                     (Just e2) )
                   (map (\n -> varE  'getEnums `appE` undef n) names)     
        getEnumsD = funD 'getEnums [clause [wildP]  (normalB getEnumsExpr) []] 
        readProcTypeExpr = doE $ 
            bindS wildP [| skipSpaces >> char '(' |] : 
            (intersperse (bindS wildP [| skipSpaces >> char ',' |]) 
                        (map (\n -> bindS (varP n) [| readProcType |]) names) ++
             [bindS wildP [| skipSpaces >> char ')' |],
              noBindS [| return $(tupE $ map varE names) |] ] )
        readProcTypeD = funD 'readProcType 
                             [clause []  (normalB readProcTypeExpr) []]
        procTypeCxt = map (\vName -> return $ ClassP ''ProcType [VarT vName]) names ++
                      map (\vName -> return $ ClassP ''Data [VarT vName]) names ++
                      map (\vName -> return $ ClassP ''Lift [VarT vName]) names
    instanceD (cxt procTypeCxt) 
                     (conT ''ProcType `appT` tupType) 
                     [getEnumsD, readProcTypeD]
  genDataIns :: [Name] -> Q Type -> Q Dec
  genDataIns names tupType = do
   k <- newName "k"
   c <- newName "c"
   z <- newName "z"
   a <- newName "a"
   let tupCons = conE tupName
       tupName = tupleDataName n
       gfoldlExpr = foldl (\acum n -> infixE (Just acum)
                                             (varE k)
                                             (Just $ varE n))
                           (varE z`appE` tupCons) 
                           names                    
       gfoldlD = funD 'gfoldl 
                       [clause [varP k, varP z, tupP (map varP names)] 
                               (normalB gfoldlExpr) []] 
       gunfoldExpr = let nKs 0 = (varE z `appE` tupCons)
                         nKs n = varE k `appE` (nKs (n-1))
                     in nKs n
       gunfoldD = funD 'gunfold 
                      [clause [varP k, varP z, wildP] (normalB gunfoldExpr) []] 
       toConstrExpr = [| mkConstr (dataTypeOf $(varE a))
                                  $(litE $ stringL (nameBase tupName))
                                  [] 
                                  Prefix  |]
       toConstrD = funD 'toConstr
                        [clause [varP a] (normalB toConstrExpr) []]
       dataTypeOfExpr = [| mkDataType $(litE $ stringL (show tupName)) 
                                      [toConstr $(varE a)] |] 
       dataTypeOfD = funD 'dataTypeOf
                          [clause [varP a] (normalB dataTypeOfExpr) []]
       dataCxt = map (\vName -> return $ ClassP ''Data [VarT vName]) names 
   instanceD (cxt dataCxt) 
             (conT ''Data `appT` tupType) 
             [gfoldlD, gunfoldD, toConstrD, dataTypeOfD]
  genTypeableIns :: [Name] -> Q Type -> Q Dec
  genTypeableIns names tupType = do
   -- generate n-1 commas to be consistent with the (faulty) instances
   -- of tuples from 2 to 7 elements
   let strRep = '(':replicate (n-1) ','++")"
       typeOfExpr = [| mkTyCon 
                        $(litE $ stringL strRep)
                        `mkTyConApp`
                        $(listE $ map (\n -> varE 'typeOf `appE` undef n) names)
                     |]
       typeOfD = funD 'typeOf
                      [clause [wildP] (normalB typeOfExpr) []]
       typeableCxt = map (\vName -> return $ ClassP ''Typeable [VarT vName]) names
   instanceD (cxt typeableCxt) 
             (conT ''Typeable `appT` tupType) 
             [typeOfD]
  genLiftIns :: [Name] -> Q Type -> Q Dec
  genLiftIns names tupType = do
   let liftExpr = 
           varE 'tupE `appE` listE (map (\n -> varE 'lift `appE` varE n) names)
       liftD = funD 'lift 
                 [clause [tupP (map varP names)] (normalB liftExpr) []]
       liftCxt = map (\vName -> return $ ClassP ''Lift [VarT vName]) names
   instanceD (cxt liftCxt) 
             (conT ''Lift `appT` tupType) 
             [liftD]


