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
 foldMap f (ZipWithNSY _ is)   = DF.foldMap f is
 foldMap f (ZipWithxSY _ _ is) = DF.foldMap f is
 foldMap f (UnzipNSY _ _ i)    = f i
 foldMap f (UnzipxSY _ _ i)    = f i                                  
 foldMap f (DelaySY _ i)       = f i
 foldMap f (SysIns  _ i)       = DF.foldMap (f.snd) i



instance DF.Foldable NlNode where
 foldMap _ (Const _)     = mempty 
 foldMap _ (InPort  _)   = mempty
 foldMap f (Proc _ proc) = DF.foldMap f proc

instance Functor NlProc where
 fmap f (ZipWithNSY pf is)   = ZipWithNSY pf (fmap f is)
 fmap f (ZipWithxSY n pf is) = ZipWithxSY n pf (fmap f is)
 fmap f (UnzipNSY n pf i)    = UnzipNSY n pf (f i)
 fmap f (UnzipxSY n pf i)    = UnzipxSY n pf (f i)                       
 fmap f (DelaySY c i)        = DelaySY c (f i)
 fmap f (SysIns def is)      = SysIns def (fmap (fmap f) is)



instance Functor NlNode where
 fmap _ (Const val)          = Const val
 fmap _ (InPort  id)         = InPort  id
 fmap f (Proc id proc)       = Proc id (fmap f proc) 

instance DT.Traversable NlProc where 
 traverse f (ZipWithNSY pf is)   = ZipWithNSY pf <$> DT.traverse f is
 traverse f (ZipWithxSY n pf is) = ZipWithxSY n pf <$> DT.traverse f is
 traverse f (UnzipNSY n pf i)    = UnzipNSY n pf <$> f i
 traverse f (UnzipxSY n pf i)    = UnzipxSY n pf <$> f i
 traverse f (DelaySY c i)        = DelaySY c <$> f i
 traverse f (SysIns def is)      = SysIns def <$> DT.traverse f' is 
  where f' (id,s) = (,) id <$> f s




instance DT.Traversable NlNode where
 traverse _ (Const val)    = pure (Const val)
 traverse _ (InPort  id)   = pure (InPort id)
 traverse f (Proc id proc) = Proc id <$> DT.traverse f proc 


-- | Traversing monad, stacking state and error transformers over IO
type TravSEIO s e a = (StateT s (ErrorT e IO)) a



-- | traverseSIO traverses a netlist and returns a final user-defined 
--   traversing state (@s@) given:
--  new: generates a new (and normally unique) tag for the outputs of each 
--       netlist node given the traversing state (which is possibly updated 
--       as well).
--  define: given the output tags of a node, current iteration state, 
--          and the output tags of its children, @define@
--          generates the netlist of that node, possibly updating 
--          the traversing state
-- FIXME: shoudn't the arguments of define go the other way around?
--        first inputs then outputs.
-- FIXME: why are tags needed in define? [oinfo] should be enough.
--        tags are ugly in general (see the pattern matches) fix this problem.
traverseSEIO :: (DT.Traversable container, Error e) => 
         (NlNode NlSignal -> TravSEIO s e [(NlNodeOut, oinfo)]) -- ^ new
      -> ([(NlNodeOut, oinfo)] -> NlNode oinfo -> TravSEIO s e ()) -- ^ define
      -> Netlist container 
      -> TravSEIO s e (container oinfo)
traverseSEIO new define (Netlist rootSignals) =
  do uRefTable <- liftIO $ newURefTableIO

     let gather (NlTree (NlEdge nodeRef tag)) =
           do visited <- liftIO $ queryIO uRefTable nodeRef
              case visited of
                Just infoPairs  -> return (specifyOut tag infoPairs)
                Nothing -> do 
                  let node = readURef nodeRef
                  infoPairs <- new node
                  liftIO $ addEntryIO uRefTable nodeRef infoPairs
                  childInfo <- DT.mapM gather node
                  define infoPairs childInfo
                  return (specifyOut tag infoPairs)

         specifyOut :: NlNodeOut -> [(NlNodeOut, a)] -> a
         specifyOut tag pairs = fromMaybe err maybeOut
             where funName = "ForSyDe.NetList.Traverse.traverseIO"
                   err = intError funName (InconsOutTag tag)
                   maybeOut = lookup tag pairs

     DT.mapM gather rootSignals

traverseST :: DT.Traversable container => 
             (NlNode NlSignal -> ST s [(NlNodeOut, oinfo)]) 
          -> ([(NlNodeOut, oinfo)] -> NlNode oinfo -> ST s ()) 
          -> Netlist container 
          -> ST s (container oinfo)
traverseST new define (Netlist rootSignals) =
  do uRefTable <- newURefTableST

     let gather (NlTree (NlEdge nodeRef tag)) =
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