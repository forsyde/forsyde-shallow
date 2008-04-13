{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 
-- Due to template haskell and Lexically scoped type variables  
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.Simulate
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell, LSTV)
--
-- This module provides the simulation backend of ForSyDe's embedded compiler
--
-- /This module is based on Lava2000/: <http://www.cs.chalmers.se/~koen/Lava/>
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.Simulate (simulate) where

import ForSyDe.OSharing
import ForSyDe.Netlist
import ForSyDe.Netlist.Traverse
import ForSyDe.Signal
import ForSyDe.System.SysDef
import ForSyDe.System.SysFun
import ForSyDe.ForSyDeErr
import ForSyDe.Process.ProcVal

import Control.Monad (liftM, replicateM, mapM_, zipWithM_)
import Data.Maybe (fromJust)
import Control.Monad.ST
import Data.STRef
import qualified Data.Traversable as DT
import Data.List (lookup, transpose)
import Data.Dynamic
import Language.Haskell.TH
import Language.Haskell.TH.TypeLib


-- | 'simulate' is a Template Haskell function which Generates another 
--   function able to simulate a System using a list-based representation 
--   of its signals. 'simulate' needs the name of a 'SysDef' variable as 
--   argument. 
--   TODO: provide a use-case example.
simulate :: Name -> ExpQ
-- FIXME: refactor code with SysDef and Instantiate
simulate sysDefN =            
           do i <- reify sysDefN
              -- check if a SysDef variable name was provided
              sysFType <- case i of
                 VarI _ t  _  _ -> 
                   case t  of 
                    ConT name `AppT` syst | name == ''SysDef -> return syst
                    _  -> currError (NonSysDef sysDefN t) 
                 _  -> currError  (NonVarName sysDefN)
              -- Get the number and types of the inputs and outputs of 
              -- the system
              ((inTypes, nIn),(outTypes, nOut)) <- checkSysFType sysFType
              -- Generate a pattern for each input 
              inNames <- replicateM nIn (newName "i")     
              let inPats  = [varP n | n <- inNames]
              -- Generate a list with all the inputs in dynamic form
                  inList  = listE [ [| map toDyn $(varE n) |] | n <- inNames]
              -- Generate the body of the simulation function
                  simBody  =    
                    [|
                      let 
                      -- The primary system definition
                      primSysDef = unSysDef $(varE sysDefN) 
                      -- Put all the inputs in a list
                      dynIns = $inList
                      -- Get the dynamic output of the simulation
                      dynOuts = simulateDyn primSysDef dynIns
                      -- Put the outputs in a tuple of lists 
                      toTup = $(dynLists2Tup nOut)       
                      in  toTup dynOuts |]
              -- Done, just add the input pattern and the concrete
              -- type signature 
              sigE (lamE inPats simBody) (return $ simType inTypes outTypes)
    
  where currError = qError "simulate"




----------------------------
-- Internal Helper Functions
----------------------------


-- | Generate a lambda expression to transform a list containing 
--   N Dynamic lists into a tuple of 'Signal's
dynLists2Tup :: Int  -- ^ size of the tuple
            -> ExpQ
dynLists2Tup n = 
    do -- Generate a list pattern of n elements
       -- and a tuple including those elements transformed in Signal
       names <- replicateM n (newName "elem")
       -- use pattern (elem1:elem2:elem3:.. :xs) and not not [elem1,elem2,elem3]
       -- because the second one doesn't work with infinite data structures
       let consName = '(:)
           listPat = foldr (\name pat -> infixP (varP name) consName pat) 
                           wildP names 
           tupExp  = tupE  [ [| map fromDynErr $(varE n) |] | n <- names]
       lamE [listPat] tupExp

-- | Transform a dynamic value into a specific one giving an error
--   if the type of the dynamic object doesn't match with the target
fromDynErr :: forall a . Typeable a => Dynamic -> a
fromDynErr dyn = fromDyn dyn (intError funName error)   
 where  funName = "ForSyDe.Backend.Simulate.fromDynErr"
        expectedType = typeOf (undefined :: a) 
        error =  DynMisMatch dyn expectedType

-- | Obtains the type of the simulation function from the type of the
--   input and output signal types of the system
simType :: [Type] -> [Type] -> Type
simType inTypes outTypes = 
   reArrowT (map signal2List inTypes, retType outTypes, monoContext) 
 where -- Obtain the simulation output type using the individual
       -- output signal types
       retType :: [Type] -> Type
       -- Just one signal output
       retType [out] = signal2List out
       -- None or multiple outputs
       retType outs  = 
            reAppT (TupleT (length outs), map signal2List outs, monoContext) 
       
       -- Transform a Signal type into a list type
       signal2List :: Type -> Type
       -- FIMXE: refactor with isSignal
       signal2List (ConT name `AppT` arg) | name == ''Signal = ListT `AppT` arg
       signal2List typ = intError "ForSyDe.Simulate.simType" (SigMisMatch typ) 
       
--------------------
-- Simulation itself
--------------------

-- FIXME: clean and document the following horrible code!

--------------------------------------
-- The following was adapted from Lava
--------------------------------------

type Var s
  = (STRef s Dynamic, STRef s (Wire s))

data Wire s
  = Wire
    { dependencies :: [Var s]
    , kick         :: ST s ()
    }

----------------------------------------------------------------
-- simulate


simulateDyn :: PrimSysDef  -> [[Dynamic]] -> [[Dynamic]]
simulateDyn pSysDef inps | any null inps = replicate outN []
   where outN = (length . oIface . readURef . unPrimSysDef) pSysDef
simulateDyn pSysDef inps = runST (
  do let sysDefVal = (readURef . unPrimSysDef) pSysDef
         sysDefInIface = iIface sysDefVal
     -- List where to store the Vars generated by delay processes
     roots <- newSTRef []
     -- Input port ids paired with a reference to each input value list
     inpPairs <- zipWithM (\(id,_) inputL -> 
                            do {ref <- newSTRef inputL; return (id,ref)}) 
                         sysDefInIface inps

     let sysF = sysFun  sysDefVal
         -- Add a Var to the roots list 
         root r =
           do rs <- readSTRef roots
              writeSTRef roots (r:rs)

         -- Create an empty var
         empty = do rval <- newSTRef (error "val?")
                    rwir <- newSTRef (error "wire?")
                    return (rval, rwir)
         new node = 
           do mapM (\tag -> do {e <- empty; return (tag, e)}) (outTags node)
         newInstance varPairs node =
           let funName = "ForSyDe.Backend.Simulate.simulateDyn" 
           in case node of
             InPort id ->
                 case lookup id varPairs of
                     -- FIXME: replace the Other error with a custom one
                     Nothing  -> intError funName (Other "inconsistency")
                     Just var -> return [(InPortOut, var)]
             _      -> new node
         
         -- define for the general traversal
         define  nodeVarPairs childVars = 
           case (nodeVarPairs,childVars) of
            ([(InPortOut, var)], InPort name) -> do
              let inputRef = fromJust $ lookup name inpPairs
              relate var [] $
                do (curr:rest) <- readSTRef inputRef
                   writeSTRef inputRef rest
                   return curr
                   
            _ -> defineShared nodeVarPairs childVars

         -- define for instances
         defineInstance nodeVarPairs childVars = 
           case (nodeVarPairs,childVars) of
            ([(InPortOut, _)], InPort _) -> return ()
            _ -> defineShared nodeVarPairs childVars
         
         -- Shared part of define define for instances and the main traversal
         defineShared  nodeVarPairs childVars = -- r s =
           case (nodeVarPairs,childVars) of
            ([(InPortOut, _)], InPort _) -> return ()

            (nodeVarPairs,
             Proc _ (SysIns pSysDef ins)) ->
               -- FIXME: ugly ugly ugly
               do let sysDefVal = (readURef . unPrimSysDef) pSysDef
                      taggedIns = zipWith (\(id,_) var -> (id,var)) 
                                          (iIface sysDefVal) ins
                  sr  <- traverseST 
                           (newInstance taggedIns)
                           defineInstance 
                           (getSymNetlist sysDefVal)

                  let relateIns prevVar@(prevValR,_) (_,nextVar) =
                         relate nextVar [prevVar] (readSTRef prevValR)
 
                  zipWithM_ relateIns sr nodeVarPairs

            ([(DelaySYOut, nodeVar)], 
             Proc _ (DelaySY (ProcVal init _) sigVar)) ->
               do valVar <- empty
                  relate valVar [] (return init)                    
                  delay nodeVar valVar sigVar
            _ ->
              do let evalPairs = eval `fmap` DT.mapM (readSTRef.fst) childVars
                     args = arguments childVars
                     relEval (tag, var) = 
                         relate var args $
                            -- FIXME: remove fromJust and write a proper error
                            liftM (fromJust.(lookup tag)) evalPairs
                 mapM_ relEval  nodeVarPairs  
          where
           delay r ri@(rinit,_) r1@(pre,_) =
               do state <- newSTRef Nothing
                  r2 <- empty
                  root r2

                  relate r [ri] $
                    do ms <- readSTRef state
                       case ms of
                         Just s  -> return s
                         Nothing ->
                           do s <- readSTRef rinit
                              writeSTRef state (Just s)
                              return s

                  relate r2 [r,r1] $
                    do s <- readSTRef pre
                       writeSTRef state (Just s)
                       return s
     
     sr   <- traverseST new define (getSymNetlist sysDefVal)
     rs   <- readSTRef roots
     -- remove tags of the resulting vars (all the root nodes should only
     -- have one output and thus a must return a unique list)
     step <- drive (sr ++ rs)

     outs <- lazyloop $
       do step
          s <- DT.mapM (readSTRef . fst) sr
          return s
     -- Since the simulation is done in a per-cycle basis
     -- the results (outs) are transposed (not what we want)
     -- e.g.
     -- imagine a system whose outputs are its inputs plus 1
     -- then, for this these two inputs [[1,2,3],[4,5,6]]
     -- outs would be [[2,5],[3,6],[4,7]]
     --
     -- We need as well to check that all inputs are defined in
     -- each simulation cycle e.g. [[1,2,3],[4,5,6,7]] as input
     -- makes imposible to simulate cycle 4
     --
     -- NOTE: having to check this makes simulation really inneficient
     -- a solution would providing a cycle-based simulation input/output
     -- which wouldn't suffer from this problems
     -- Or, even better, a simulation which showed a diffierent type of output 
     let inN = length sysDefInIface
         res = if inN == 0 then
                 transpose outs
               else transpose (checkIns inN (transpose inps) outs)
     return res
  )

   
-- evaluation order

relate :: Var s -> [Var s] -> ST s Dynamic -> ST s ()
relate (rval, rwir) rs f =
  do writeSTRef rwir $ 
       Wire{ dependencies = rs
           , kick = do b <- f
                       writeSTRef rval b
           }

drive :: [Var s] -> ST s (ST s ())
drive [] =
  do return (return ())

drive ((rval,rwir):rs) =
  do wire <- readSTRef rwir
     writeSTRef rwir (error "detected combinational loop")
     driv1 <- drive (dependencies wire)
     writeSTRef rwir $
       Wire { dependencies = [], kick = return () }
     driv2 <- drive rs
     return $
       do driv1
          kick wire
          driv2

----------------------------------------------------------------
-- helper functions

lazyloop :: ST s a -> ST s [a]
lazyloop m = 
  do a  <- m
     as <- unsafeInterleaveST (lazyloop m)
     return (a:as)

-- transform a list of dynamic input values into a circular 
-- chain of linked delay processes
toDelays :: [[Dynamic]] -> [NlSignal]
toDelays = map link
  where 
    link :: [Dynamic] -> NlSignal
    link xs = let out = foldr delay out xs
              in out
    delay :: Dynamic -> NlSignal -> NlSignal  
    delay val signal = 
                  let t = dynTypeRep val
                      -- The Exp and Type part won't be ever accessed
                      -- during simulation and can be left undefined
                      procval x t = ProcVal x (ProcValAST undefined t)
                  in node2NlSignal  
                      (newURef (Proc "" (DelaySY (procval val t) signal))) 
                      DelaySYOut

-- | check that there will only be output as long as there are inputs 
checkIns :: Int -- ^ number of inputs 
         -> [[a]] -- ^ transposed inputs 
         -> [[b]] -- ^ transposed outputs (infinitie list) 
         -> [[b]] -- ^ selected outputs

-- The lazy pattern match is used to avoid evaluating the output list 
-- if length i /= nIns. If that happens the input lists of simulate will 
-- implicitly be simulated, and due to lack of inputs it will cause an error. 
checkIns nIns (i:is) ~(o:os) |  length i == nIns = o : checkIns nIns is os  
checkIns _ _ _ = []



----------------------------------------------------------------
-- the end.


