-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Process.ProcVal
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (Template Haskell)
--
-- This module provides a type ('ProcVal') to store the value arguments passed
-- to process constructors.
--
----------------------------------------------------------------------------- 
module ForSyDe.Process.ProcVal where


import Data.Typeable (Typeable)
import Data.Dynamic (toDyn, Dynamic)
import Language.Haskell.TH (ExpQ, Type)
import Language.Haskell.TH.Syntax (Lift(..))
import Language.Haskell.TH.TypeLib (thTypeOf)

data ProcVal = ProcVal 
                  {dyn     :: Dynamic,    --  Dynamic value 
                   valAST  :: ProcValAST} --  its AST

data ProcValAST = ProcValAST
                    {exp    :: ExpQ,    --  Its AST representation
                     expTyp :: Type}    --  Type of the value   

-- | 'ProcVal' constructor
mkProcVal :: (Lift a, Typeable a) => a -> ProcVal
-- FIXME: would unsafePerformIO cause any harm to get the exp out of the
--        Q monad in this context?
mkProcVal val = ProcVal (toDyn val) (ProcValAST (lift val) (thTypeOf val))

mkProcValAST :: (Lift a, Typeable a) => a -> ProcValAST 
mkProcValAST val = ProcValAST (lift val) (thTypeOf val) 