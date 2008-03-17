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
       let listPat = listP [varP n | n <- names]
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
-- Empty inputs
simulateDyn pSysDef inps | all null inps = replicate outN []
  where outN = (length . oIface . readURef . unPrimSysDef) pSysDef

-- Non-empty inputs
simulateDyn pSysDef inps | seq argsl True = runST (
  do let sysDefVal = (readURef . unPrimSysDef) pSysDef
     -- List where to store the Vars generated by delay processes
     roots <- newSTRef []
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
         
         define  nodeVarPairs childVars = -- r s =
           case (nodeVarPairs,childVars) of
            ([(InPortOut, _)], InPort _) -> return ()

            (nodeVarPairs,
             Proc _ (SysIns pSysDef ins)) ->
               -- FIXME: ugly ugly ugly
               do let sysDefVal = (readURef . unPrimSysDef) pSysDef
                  sr  <- traverseST 
                           (newInstance ins)
                           define 
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
               -- FIXME: eval doesn't work for processes with various outputs
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
     
     sr   <- traverseST new define (Netlist $ sysF (toDelays inps))
     rs   <- readSTRef roots
     -- remove tags of the resulting vars (all the root nodes should only
     -- have one output and thus a must resturn a unique list)
     step <- drive (sr ++ rs)

     outs <- lazyloop $
       do step
          s <- DT.mapM (readSTRef . fst) sr
          return s
     -- Since the simulation is done in a per-cycle basis
     -- the results need to be transposed  
     let res = transpose (take argsl outs)
     return res
  )
 where -- Check length of arguments
         -- FIXME: can this be avoided?
         --        it prevents simulation from working on infinite lists
         argsl = foldr1 check inLengths
          where inLengths = map length inps
                check a b | a == b = a
                -- FIXME: we use error even if it's not an exception
                --        only an informational message should be shown
                --        should something else be used?
                check a b = error $ show (InLengthMisMatch a b)
   
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


----------------------------------------------------------------
-- the end.

