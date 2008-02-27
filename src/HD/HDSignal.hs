module HD.HDSignal where

import HD.Ref
import HD.HDFun
import HD.Types

import Data.Foldable (Foldable(foldMap))
import Data.Monoid (mempty, mappend)
import Data.Traversable (Traversable(traverse)) 
import Control.Applicative (pure,(<*>),(<$>))
 
-----------
-- HDSignal
-----------


-- Primitive signals without the reference & type layer
-- TODO: explain that a signal holds all the previously chained
-- signals (it allows to view all the signals connected "on its left")
-- TODO: explain how block instances work
data  S s = -- Ports
            -- TODO, should string be a new type (PortEntryId?)
            InPortS   String        | -- Port Input signal 
            UndefS                  | -- Initial signal value in OutPorts
                                      -- and the destination (left) side
                                      -- of a BlockInstance 
            OutPortS String s       | -- Output Port
            -- Block Instance
            BlockInsS [(String,s)] | -- Block Instance Signal
             -- (the list represents the block instance input signals)
            -- Synchronous lib
            MapSY     HDPrimFun s   |
            ZipWithSY HDPrimFun s s |
            DelaySY   HDPrimConst s  



instance Foldable S where
 foldMap f (InPortS  _)        = mempty
 foldMap f UndefS              = mempty
 foldMap f (OutPortS _ s)      = f s
 foldMap f (BlockInsS  xs)     = foldr (mappend.f.snd) mempty xs
                                 -- more efficient than mconcat (map snd xs)
 foldMap f (MapSY _ s)         = f s
 foldMap f (ZipWithSY _ s1 s2) = f s1 `mappend` f s2
 foldMap f (DelaySY _ s)       = f s

instance Functor S where
 fmap f (InPortS  id)        = InPortS  id
 fmap f UndefS               = UndefS
 fmap f (OutPortS id s)      = OutPortS id (f s)
 fmap f (BlockInsS  xs)      = BlockInsS  (map (fmap f) xs)        
 fmap f (MapSY mf s)         = MapSY mf (f s)
 fmap f (ZipWithSY fz s1 s2) = ZipWithSY fz (f s1) (f s2)
 fmap f (DelaySY c s)        = DelaySY c (f s)

instance Traversable S where
 traverse _ (InPortS  id)        = pure (InPortS id)
 traverse _ UndefS               = pure U4ndefS
 traverse f (OutPortS id s)      = OutPortS id <$> f s
 traverse f (BlockInsS  xs)      = BlockInsS <$> traverse f' xs 
  where f' (id,s) = (,) id <$> f s
 traverse f (MapSY mf s)         = MapSY mf <$> f s
 traverse f (ZipWithSY fz s1 s2) = ZipWithSY fz <$> f s1 <*> f s2
 traverse f (DelaySY c s)        = DelaySY c <$> f s


-- We add: 
--  * references (in order to obtain observable sharing) 
--  * block instance output-info. In case the node is
--    an instance it indicates to which specific output
--    are we refering (read below for more info))  
--  * and types
-- obtaning the final primitive signal which is, as well, the netlist
-- representation of the system
-- FIXME: its not really the netlist, it represents an end node
--        of a connected (no islands) and directed graph
-- Note: the instance output information is not included with 
--       BlockInsS to guarantee sharing
--       e.g: let's imagine the following situation
--       
--        .-------------------.    
--        | Block Instance "o1"|----> mapSY (+2)   
--        |                "o2"|----> mapSY (+1)
--        |                "o3"|----> mapSY (+3)
--        |____________________| 
--     
--       Three processes connected to the same instane (outputs "o1" "o2" and 
--        "o3).
--       If the instance output info was bundled with BlockInsS 
--       (i.e. data S s = ... | BlockInsS MaybeInsOut ...) there would be no
--       way to make the three processes share the same instance node
--       since each process refers to a different output entry and using
--       a unique reference to the instance would not be possible
--       (i.e. ref (BlockInst "o1") and ref (BlockInst "o2") constitute
--        diferent references) 
 

-- TODO: There is not a reason to store the types anymore. They are currently
--       only used for typechecking in ports and block instances, but since we
--       have explicit type representations (e.g. Bool and Int, see
--       HD.Types.HDType) whatever checking can be done in SourcePort and
--       DestPort based on that. However, typechecking is still based on the
--       stored type and it should be fixed first.
type IsBlockInsOut  = Maybe String
data HDPrimSignal = HDPrimSignal IsBlockInsOut HDSignalType (Ref (S HDPrimSignal))

-- We add the final type-safety layer obtaining the final HDSignal
newtype HDPrimType a => HDSignal a = HDSignal HDPrimSignal


-------------------
-- Helper functions
-------------------

-- Class made for library convenience, it allows us to forget 
-- whether we are working with HDSignals or HDPrimSignals
-- TODO: the class could probably be removed once the type-tag of
--       the signals is gets removed.

class SWrapper a where
 wrap      :: HDSignalType -> S HDPrimSignal -> a
 unwrap    :: a -> S HDPrimSignal
 typeOfS   :: a -> HDSignalType 
 typeMatch :: a -> a -> Bool  
 hasType   :: a -> HDSignalType -> Bool
 isUndefS  :: a -> Bool

instance SWrapper HDPrimSignal where
 wrap t                     = (HDPrimSignal Nothing t). ref 
 unwrap  (HDPrimSignal _ _ r) = deref r
 typeOfS (HDPrimSignal _ t _) = t
 typeMatch s1 s2              = typeOfS s1 == typeOfS s2
 hasType s t                  = typeOfS s == t
 isUndefS s                   = case unwrap s of
                                 UndefS -> True
                                 _      -> False

instance SWrapper (HDSignal a) where
 wrap  t                               = HDSignal.(wrap t)  
 unwrap  (HDSignal s)                  = unwrap s
 typeOfS (HDSignal s)                  = typeOfS s
 typeMatch (HDSignal s1) (HDSignal s2) = typeMatch s1 s2
 hasType (HDSignal s) t                = hasType s t
 isUndefS (HDSignal s)                 = isUndefS s 



 









