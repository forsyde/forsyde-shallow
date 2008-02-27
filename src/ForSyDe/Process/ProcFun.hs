{-# LANGUAGE TemplateHaskell #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process.ProcFun
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides a type ('ProcFun') to store the functions  passed
-- to process constructors.
--
----------------------------------------------------------------------------- 
module ForSyDe.Process.ProcFun 
 (ProcFun(..),
  ProcFunAST(..), 
  defArgVal,
  defArgPF,
  procFun2Dyn,
  contProcFun2Dyn,
  newProcFun) where

import ForSyDe.Process.ProcVal (ProcValAST, mkProcValAST)
import ForSyDe.ForSyDeErr

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.LiftInstances ()
import Data.Dynamic
import Data.Maybe (fromJust)


-- | A Process Function 
data ProcFun a = 
   ProcFun {val  :: a,          -- ^ Value of the function
            ast  :: ProcFunAST} -- ^ AST of the function



-- | A process Function AST
data ProcFunAST = 
  ProcFunAST {name :: Name,     -- ^ Function Name 
                                    -- (FIXME: maybe just a String?) 
              typ  :: Type,     -- ^ Function Type
              cls  :: [Clause], -- ^ Function clauses 
              pars :: [DefArg]} -- ^ Default parameters


-- | A process Function default argument
--   Either a process function AST or a value AST
data DefArg = FunAST ProcFunAST | ValAST ProcValAST



procFun2Dyn :: Typeable a => ProcFun a -> ProcFun Dynamic
procFun2Dyn f = f{val= toDyn (val f)}


contProcFun2Dyn :: (Typeable a, Typeable b, Functor container) =>
                   ProcFun (container a -> b) -> 
                   ProcFun (container Dynamic -> Dynamic)
contProcFun2Dyn f = f{val = fmapDyn (val f)}
       where  fmapDyn f cont = toDyn (f (fmap (fromJust.fromDynamic) cont)) 


-- | Sets a default value for an argument of the process function
defArgVal :: (Lift a, Typeable a) => ProcFun (a -> b) -> a -> ProcFun b
-- FIXME: inneficient, use a queue data structure
defArgVal pf v = 
   pf{ ast = astPF {pars = ((pars astPF) ++ [ValAST (mkProcValAST v)])}, 
       val = (val pf) v}                    
  where astPF = ast pf

-- | Sets a default value for an argument of the process function 
--   when the argument is a process function itself
defArgPF :: ProcFun (a -> b) -> ProcFun a -> ProcFun b
defArgPF pf v = pf{ ast = astPF {pars = ((pars astPF) ++ [FunAST (ast v)])}, 
                    val = (val pf) (val v)}                    
  where astPF = ast pf 
                   
-- | Template Haskell constructor for 'ProcFun'
--   TODO: provide sample use-case
newProcFun :: Q [Dec] -> ExpQ
newProcFun fDecQs = do 
      fDecs <- fDecQs   
      -- Check for the declarations to be correct
      (name, typ, cls) <- recover
                            (currErr $ IncorrProcFunDecs fDecs) 
                            (checkDecs fDecs)
      -- Generate the main expression
      exp <-  [| let  fName    = name
                      fType    = typ
                      fClauses = cls 
                 in ProcFun $(varE name) 
                            (ProcFunAST fName fType fClauses []) |]
      -- Add the function declarations to the expression
      return $ LetE fDecs exp  
 where currErr = qError "newProcFun"
 
----------------------------
-- Internal Helper Functions
----------------------------

-- | Check the decarations passed to newProcFun to be correct
checkDecs :: [Dec] -> Q (Name, Type, [Clause])
checkDecs [SigD name1 t, FunD name2 cls] | name1 == name2 = 
  return (name1, t, cls) 
checkDecs _                  = qGiveUp name
  where name = "ForSyDe.Process.ProcFun.checkDecs"