{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables,
             MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.System.SysFun.Instances
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides the instances for 'SysFun', it cannot
-- be included in "ForSyDe.System.SysFun" directly due to a Template Haskell
-- bug which prevents Template Haskell from executing functions defined in the
-- same module: <http://hackage.haskell.org/trac/ghc/ticket/1800>
--
----------------------------------------------------------------------------- 
module ForSyDe.System.SysFun.Instances () where

import Data.Dynamic
import Control.Monad (liftM)
import Language.Haskell.TH.Syntax (runIO, runQ, lift)
import System.IO.Unsafe (unsafePerformIO)

import ForSyDe.Config (maxTupleSize)
import ForSyDe.System.SysFun (
     SysFun(..), 
     SysFunToSimFun(..), 
     SysFunToIOSimFun(..), 
     funOutInstances)
import ForSyDe.Netlist
-- This aparently unnecesary SysDef import is needed as a workaround for 
-- http://hackage.haskell.org/trac/ghc/ticket/1012
import ForSyDe.System.SysDef()
import ForSyDe.Process.ProcType (ProcType(..))


--   IMPORTANT NOTE: even if all ProcType constraints in SysFun and
--         SysFunToSimFun instances could be less restrictive
--         (Typeable would do), it makes more sense, and who nows,
--         maybe we end up requiring full ProcType functionality at
--         some point.


-- This three instances are the ones in charge of providing the necessary 
-- recursion step needed to support the variable number of arguments.
-- In each step, the system function is provided with a new input signal port
-- until the output signals are obtained.
instance (ProcType a, SysFun f) => SysFun (Signal a -> f) where
 applySysFun f ids = (outSignals, currInType : nextInTypeReps, outTypeReps)
  where (outSignals, nextInTypeReps, outTypeReps) = 
          case ids of
            [] -> applySysFun (f (Signal (newInPort "default"))) []
            (i:is) -> applySysFun (f (Signal (newInPort i))) is 
        currInType = typeOf (undefined :: Signal a)
 fromListSysFun f accum s = fromListSysFun f ((unSignal s):accum)

instance (ProcType a, SysFunToSimFun sysFun simFun) => 
         SysFunToSimFun (Signal a -> sysFun) ([a] -> simFun) where
 fromDynSimFun f accum s = fromDynSimFun f ((map toDyn s):accum)

instance (ProcType a, SysFunToIOSimFun sysFun simFun) => 
         SysFunToIOSimFun (Signal a -> sysFun) ([a] -> simFun) where
 fromTHStrSimFun f accum s = fromTHStrSimFun f ((map unsafeLift s):accum)
   -- FIXME: This won't be needed once the Data a => Lift a instance
   --        is created
   where unsafeLift = unsafePerformIO.runQ.lift

-- Generate instances for the system function outputs up to the maximum
-- tuple size
$(let concatMapM f xs = liftM concat (mapM f xs) 
      listFunOutInstances = liftM (\(a,b,c) -> [a,b,c]) . funOutInstances
      msg = "Generating and compiling " ++ show maxTupleSize ++ 
            " output instances of " ++
            show ''SysFun ++ show ''SysFunToSimFun ++ 
            " and " ++ show ''SysFunToIOSimFun ++ 
            ", this might take some time ... \n"
  in runIO (putStrLn $ msg) >>
     concatMapM listFunOutInstances [0..maxTupleSize]
  )
