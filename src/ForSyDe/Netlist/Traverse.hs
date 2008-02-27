{-# LANGUAGE ScopedTypeVariables #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Netlist.Traverse
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  non-portable (LSTV)
--
-- This module provides traversing operations for 'ForSyDe.Netlist'
--
--
-- /This module is based on Lava2000/: <http://www.cs.chalmers.se/~koen/Lava/>
-- 
-----------------------------------------------------------------------------
module ForSyDe.Netlist.Traverse  where


import ForSyDe.Netlist
import ForSyDe.OSharing
import ForSyDe.ForSyDeErr


import Data.Maybe (fromMaybe)
-- qualified to avoid nameclash
import qualified Data.Foldable  as DF (Foldable(foldMap), foldr)
import Data.Monoid (mempty)
-- qualified to avoid nameclash
import qualified Data.Traversable as DT (Traversable(traverse,mapM)) 
import Control.Applicative (pure, (<$>))
import Control.Monad.State
import Data.List (lookup)
import Control.Monad.ST (ST)

-- Instances to traverse a netlist Node (and implicitly the whole netlist)

instance DF.Foldable NlProc where
 foldMap f (ZipWithNSY _ is) = DF.foldMap f is
 foldMap f (ZipWithxSY _ is) = DF.foldMap f is
 foldMap f (UnzipNSY _ _ i)  = f i
 foldMap f (UnzipxSY _ _ i)  = f i                                  
 foldMap f (DelaySY _ i)     = f i
 foldMap f (SysIns  _ i)     = DF.foldMap (f.snd) i



instance DF.Foldable NlNode where
 foldMap _ (Const _)     = mempty 
 foldMap _ (InPort  _)   = mempty
 foldMap f (Proc _ proc) = DF.foldMap f proc

instance Functor NlProc where
 fmap f (ZipWithNSY pf is) = ZipWithNSY pf (fmap f is)
 fmap f (ZipWithxSY pf is) = ZipWithxSY pf (fmap f is)
 fmap f (UnzipNSY n pf i)  = UnzipNSY n pf (f i)
 fmap f (UnzipxSY n pf i)  = UnzipxSY n pf (f i)                       
 fmap f (DelaySY c i)      = DelaySY c (f i)
 fmap f (SysIns def is)    = SysIns def (fmap (fmap f) is)



instance Functor NlNode where
 fmap _ (Const val)          = Const val
 fmap _ (InPort  id)         = InPort  id
 fmap f (Proc id proc)       = Proc id (fmap f proc) 

instance DT.Traversable NlProc where 
 traverse f (ZipWithNSY pf is) = ZipWithNSY pf <$> DT.traverse f is
 traverse f (ZipWithxSY pf is) = ZipWithxSY pf <$> DT.traverse f is
 traverse f (UnzipNSY n pf i)  = UnzipNSY n pf <$> f i
 traverse f (UnzipxSY n pf i)  = UnzipxSY n pf <$> f i
 traverse f (DelaySY c i)      = DelaySY c <$> f i
 traverse f (SysIns def is)    = SysIns def <$> DT.traverse f' is 
  where f' (id,s) = (,) id <$> f s




instance DT.Traversable NlNode where
 traverse _ (Const val)    = pure (Const val)
 traverse _ (InPort  id)   = pure (InPort id)
 traverse f (Proc id proc) = Proc id <$> DT.traverse f proc 

{-
-- | Traversing state for the IO Monad
type TravSIO s = StateT s IO


-- | traverseSIO traverses a netlist and returns a final user-defined 
--   traversing state (s) given:
--  new: generates the new (and normally unique) tag of every node given
--       the traversing state (which is updated as well).
--  define: given the tag of a node, current iteration state, and the tag of
--          its children, generates the netlist of that node, updating the 
--          traversing state
--
-- 
--FIXME: 1) why IO and not ST?
--       2) make a stateless version of netlist:
traverseSIO :: forall container s nt. DT.Traversable container =>
               (TravSIO s NlSignal -> TravSIO s nt)        -- ^ new 
            -> (TravSIO s nt -> NlNode nt -> TravSIO s ()) -- ^ define
            -> Netlist container                         
            -> TravSIO s (container nt)
traverseSIO new define (Netlist container) =
  do uRefTable  <-  lift newURefTable     
     let gather :: TravSIO s NlSignal  ->  TravSIO s nt
         gather stateNlSignal =
            do nlSignal@(NlTree (NlEdge uRef _ _ )) <- stateNlSignal 
               visited <- lift (query uRefTable uRef) 
               case visited of
                 Just tag  -> return tag
                 Nothing -> do tag'  <- new stateNlSignal
                               lift (addEntry uRefTable uRef tag')
                               s <- DT.mapM (gather.return) (readURef uRef)
                               define (return tag') s
                               return tag'
           
     DT.mapM (gather.return) container
-}


traverseST :: DT.Traversable container => 
             (NlNode NlSignal -> ST s [(NlNodeOut, ninfo)]) 
          -> ([(NlNodeOut, ninfo)] -> NlNode ninfo -> ST s ()) 
          -> Netlist container 
          -> ST s (container ninfo)
traverseST new define (Netlist rootSignals) =
  do uRefTable <- newURefTableST

     let gather (NlTree (NlEdge nodeRef tag _ )) =
           do visited <- queryST uRefTable nodeRef
              case visited of
                Just infoPairs  -> return (specifyOut tag infoPairs)
                Nothing -> do let node = readURef nodeRef
                              infoPairs <- new node
                              addEntryST uRefTable nodeRef infoPairs
                              childInfo <- DT.mapM gather node
                              define infoPairs childInfo
                              return (specifyOut tag infoPairs)

         specifyOut :: NlNodeOut -> [(NlNodeOut, a)] -> a
         specifyOut tag pairs = fromMaybe err maybeOut
             where funName = "ForSyDe.NetList.Traverse.traverseST"
                   err = intError funName (InconsOutTag tag)
                   maybeOut = lookup tag pairs

     DT.mapM gather rootSignals

-- | Obtain the arguments of a node
arguments :: NlNode a -> [a]
arguments = DF.foldr (:) []