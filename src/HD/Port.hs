{-# OPTIONS_GHC -fglasgow-exts #-}
-- The use of -fglasgow-exts is due to non-standard subport instances such as
-- "instance Index ix => SubPort (ix,ix)" which is not haskell98

-- FIXME: rename this module to Hierarchy.hs or Components.hs
-- FIXME: clean functions provided by Oleg
-- FIXME: do all the type checking in the typeclass defalut functions       
-- FIXME: "error" is used all the time when providing user errors, 
--        shouldn't we use EProne for that?

module HD.Port  where

import HD.Misc
import HD.Types
import HD.HDSignal

import Data.List (findIndex)

-- TODO: Create operands for connect and friends ... 
-- TODO: connecting outports in a monad?
---------------
-- Public Types
---------------



-- Port descriptor
-- Each string is a signal identifier
-- it shouldn't include duplicates (which is checked anyway)
-- TODO: should String be redefined to PortEntryId?
type PortDesc  = [(String,HDType)]

-- Port, it's a Port Descriptor which also holds the signals
-- FIXME: maybe an array or sequence would be better .. 
type Port = [(String,HDType,HDPrimSignal)]

portDesc :: Port -> PortDesc
portDesc port = [(id,t) | (id,t,s) <- port  ]

-- FIXME: Rename it to System?
type Circuit = (InPort -> OutPort)



----------
-- Classes
----------


-------------
-- Port Index
-------------

-- FIXME: Why Show ix?

-- Index to address a signal in a port
class Show ix => PortIndex ix where
 -- Given a port descriptor, get the equivalent Int of the index
 -- making sure it fits with the descriptor   
 fromIndex :: PortDesc -> ix ->  EProne Int
 

instance PortIndex Int where
 fromIndex pd i
    | i >= length pd || i < 0 = throwError err
    | otherwise               = return i 
  where pdL = length pd
        err = "index " ++ show i  ++ " is out of port range " ++ show (0,pdL-1)

instance PortIndex String where
 fromIndex pd id = maybe (throwError err) return maybeIx 
  where maybeIx = findIndex ((==id).fst) pd
        err     = "port descriptor doesn't contain identifier " ++ show id
--------------
-- Source Port
--------------

-- A source of multiple signals which can be individually addressed by an index
class SourcePort s where
 sourcePD :: s -> PortDesc
 -- Plug an external signal to the port 
 -- It uses dynamic existential types, without them aand due to the
 -- HDPrimType constraint (which is essential to typecheking), the
 -- connect* functions would break, see
 -- http://groups.google.com/group/fa.haskell/browse_thread/thread/6b7ee527cce1f0f1?hl=en
 -- for details
 plugSigD  :: (PortIndex ix) => ix -> s  ->
             (forall a. HDPrimType a => HDSignal a -> b) -> b
 plugSigD ix s f = either error (\ix -> unsafePlugSigD ix s f) eErrIx
    where eErrIx  = fromIndex (sourcePD s) ix
 -- Unsafe but fast signal plugging using Ints 
 -- (without needing to call fromIndex)
 -- only used directly within this module (not exported)
 -- for the same reason as plugSigD, it uses existentials
 unsafePlugSigD :: Int -> s -> (forall a. HDPrimType a => HDSignal a -> b) -> b


-- The following is the inverse of  "sTypeOfC    :: a -> HDSignalType"
-- TODO: It should probably be in Types.hs
withSignalOfType :: HDSignalType ->
                   (forall a. HDPrimType a => HDSignal a -> w) -> w
-- given a type
withSignalOfType (HDSignalType Int) f = f (undefined::HDSignal Int)
withSignalOfType (HDSignalType Bool) f = f (undefined::HDSignal Bool)


-- Safe cast from one signal type to the other. Useful to transform
-- a static signal (e.g. HDSignal Int) to a dynamic one 
-- (i.e. forall a.HDPrimType a => HDSignal a)
sigCast :: forall a b. (HDPrimType a, HDPrimType b) => HDSignal a -> HDSignal b
sigCast (HDSignal x) | sTypeOfC (undefined::a) ==
                       sTypeOfC (undefined::b)
                           = HDSignal x
sigCast x = error "Bad signal type"



-- This is the function showed to the end user in order to hide existential 
-- types of plugSigD
plugSig :: (SourcePort s, PortIndex ix, HDPrimType a) => 
           ix -> s  -> (HDSignal a -> b) -> b
plugSig ix s f = plugSigD ix s (f.sigCast)

 -- helper function
 -- TODO, extend this to 7
plugSig2 :: (PortIndex ix1, PortIndex ix2, SourcePort s,
             HDPrimType a, HDPrimType b) => 
            ix1 -> ix2 -> s ->
            (HDSignal a -> HDSignal b -> c) -> c
plugSig2 ix1 ix2 s f = plugSig ix2 s (plugSig ix1 s f)


-------------------
-- Destination Port
-------------------

-- A destination of multiple signals which can be individually 
-- addressed by an index
class DestPort d where
 destPD     :: d -> PortDesc
 -- Supply a signal to the port
 supplySig  :: (PortIndex ix) => HDSignal a -> ix -> d -> d
 supplySig s ix d = either error (\ix -> unsafeSupplySig s ix d) eErrIx
  where eErrIx = fromIndex (destPD d) ix
 -- Unsafe but fast  signal supply using Ints 
 -- (without needing to call fromIndex)
 -- only used directly within this module (not exported)
 unsafeSupplySig :: HDSignal a -> Int -> d -> d

  
-----------
-- SubPorts
-----------

-- Given a port, a SubPort is a collection of port signals belonging to it
-- (duplicates are allowed)

class Show sp => SubPort sp  where
 -- Given a port descriptor, get the equivalent index-list of the subport
 -- making sure it fits with the descriptor   
 fromSubPort ::  PortDesc -> sp -> EProne [Int]


instance PortIndex ix => SubPort [ix] where
 fromSubPort pd ixs = (mapM  (fromIndex pd) ixs) +! "incorrect subport" 



instance PortIndex ix => SubPort (ix,ix) where
  fromSubPort pd range@(low,high) = 
   do l <- (fromIndex pd low) +! lowErr
      h <- (fromIndex pd high) +! highErr
      if l > h then throwError rangeErr else return [l..h]
   where lowErr   =  "incorrect lowest range value " ++ show low
         highErr  =  "incorrect highest range value " ++ show high
         rangeErr =  "incoherent range " ++ show range



--------------------------------
-- Connect Source to Destination
--------------------------------

-- Connect providing indexes
connectIx :: (SourcePort s, PortIndex six, DestPort d, PortIndex dix) =>
             six -> s -> dix -> d -> d
-- This can seem ugly,
-- it would be easier for us having different types for plugSigD and supplySig
-- but the final user would find it much more difficult to deal with
-- supplysig
connectIx six s dix d = plugSigD six s ((push2 supplySig) dix d)

-- unsafe and fast Signal connect (only used internally)
unsafeConnectIx :: (SourcePort s, DestPort d) => Int -> s -> Int -> d -> d
unsafeConnectIx six s dix d = 
 unsafePlugSigD six s ((push2 unsafeSupplySig) dix d)


-- Connect providing subports
connectSP :: (SourcePort s, SubPort ssp, DestPort d, SubPort dsp) =>
             ssp -> s -> dsp -> d -> d
connectSP ssp s dsp d = fromEProne "in connectSP" $
  do sis <- fromSubPort (sourcePD s) ssp +! sspErr
     dis <- fromSubPort (destPD d)   dsp +! dspErr 
     let maybeDup = findDup dis   
     if length sis /= length dis then throwError lErr else
        maybe (return $ doConnect sis s dis d) (throwError.dupErr) maybeDup 
  where sspErr   = "incorrect source subport " ++ show ssp 
        dspErr   = "incorrect destination subport " ++ show dsp
        lErr     = "source and destination subports have different lengths"
        dupErr d = "destination subport contains a duplicate with int value (" 
                   ++ show d ++ ")"  
        doConnect sis s dis d = foldr f d (zip sis dis)
             where f (six,dix) d = unsafeConnectIx six s dix d



-- Connect only providing the subport for the source
connectTo :: (SourcePort s, SubPort ssp, DestPort d) =>
             ssp -> s -> d -> d
connectTo ssp s d = connectSP ssp s (getIds.destPD $ d) d

-- Connect only providing the subport for the destination
connectFrom :: (SourcePort s, DestPort d, SubPort dsp) =>
               s -> dsp -> d -> d
connectFrom s dsp d = connectSP (getIds.sourcePD $ s) s  dsp d

-- Connect all signals of source and destination
connect :: (SourcePort s, DestPort d) => s -> d -> d
connect s d = connectSP (getIds.sourcePD $ s) s  (getIds.destPD $ d) d 

-------------
-- Input Port
-------------

-- For an input port we cannot just keep its port descriptor
-- if new Input signals (and thus new and different references) were generated
-- each time  plugSig was called we would loose Sharing 
-- (the same reference must be used all the time for each entry)
newtype InPort = InPort Port

mkInPort :: Int -> PortDesc -> InPort
-- Create an Input Port given the its port descriptor
mkInPort l pd = InPort  [ (id,t, (wrap $ HDSignalType t) (InPortS id)) |
                          (id,t) <- checkedPD ]
 where checkedPD = fromEProne "incorrect input port" (wrongPortPar l pd)


instance SourcePort InPort where
 sourcePD (InPort port)      = portDesc port
 unsafePlugSigD  ix (InPort port) f = 
     -- FIXME, IMPORTANT: explain with this doesn't work
     -- f (HDSignal s)
     -- and explain the mechanism provided by Oleg
     withSignalOfType (HDSignalType t) (\a -> f ((HDSignal s) `asTypeOf` a))
   where (id,t,s) = port !! ix

--------------
-- Output Port
--------------

-- In this case, we need as well to hold the signals themselves, 
-- apart from the descriptor
-- FIXME: should an array be used to make supplySig faster?
newtype OutPort = OutPort Port

mkOutPort :: Int -> PortDesc -> OutPort
mkOutPort l pd = OutPort [ (id,t, (undefS.HDSignalType) t) | (id,t) <- pd  ] 
 where checkedpd  = fromEProne "incorrect output port" (wrongPortPar l pd)
       undefS t   = wrap t UndefS

instance DestPort OutPort where
 destPD (OutPort port) = portDesc port
 unsafeSupplySig  (HDSignal newS) ix op@(OutPort port)   
   | not $ isUndefS  currentS = error "Signal was already supplied"
   | not $ typeMatch currentS newS 
             = error $ "Type mismatch: expected type = "
                       ++ (show.typeOfS) currentS ++
                                      " supplied type = "
                       ++ (show.typeOfS) newS
   | otherwise  = OutPort (replaceN ix (id,t,newOutPS) port) 
  where (id,t,currentS) = port !! ix
        newOutPS  = wrap (typeOfS newS) (OutPortS  id newS)
       


---------
-- Block
---------

-- TODO: Document it properly
data Block = Block String InPort OutPort 
--TODO: include unique reference to distinguish the block
--FIXME: why is the full InPort included? it's signals are not useful afterwards
--       its descriptor should be more than enough
 
-- TODO: does it feel ugly to provide an input port and the circuit?
--       providing a developed output port should be enough
mkBlock :: String -> InPort -> Circuit -> Block
mkBlock str ip circ = Block str ip (circ ip)


------------------
-- Block Instance
------------------



-- By instantiating we transform a Block  from a white box to a black box
-- to which we can plug and supply signals
-- Thus, the destination of the block (OutPort) turns into a source
-- and its source (InPort) turns into a destination
data BlockIns = BlockIns String 
                         BIDest  -- Block Instance Destination:
                                 -- it holds the information of the source side
                                 -- of the instance
                                  -- (i.e to be used when calling supplySig)
                         BISource -- Block Instance Source:
                                  -- it holds source information of the source side
                                  -- of the instance
                                  -- (i.e to be used when calling plugSig 
                         Block    -- Block from which the instance was generated


-- Block Instance Destination side
-- we reuse an OutPort
type BIDest = OutPort

-- Block Instance Source side, 
-- works similarly to an input port, but only one signal is sound
-- (its BlockInsS)
data BISource = BISource PortDesc HDPrimSignal


instantiate :: Block -> BlockIns
instantiate (Block bname ip  op) = 
  BlockIns bname 
           -- we transform the input port into an output one
           (mkOutPort ipl ipPD) 
           -- we transform the output port into an input one
           (BISource ) 
 where ipl  = length ipPD
       ipPD = sourcePD ip      
{-

instance DestPort BlockIns where
 destPD    (BlockIns _ op _) = destPD op
 unsafeSupplySig s ix (BlockIns bname op ip) = BlockIns bname op' ip  
  where op' = unsafeSupplySig s ix op   
  



instance SourcePort BlockIns where
 sourcePD (BlockIns id op ip) = portDesc ip
 unsafePlugSigD ix (BlockIns name op ip) f =
 -- TODO: this function has horrible side effects (depending on how the execution order
 --                                                of supplySig and supplySig the results can be
 --                                                different)
 --       _document them_

   fromEProne "" $
    -- Make sure first that all the signals of the output port where suplied
    -- to the instance before plugging anything to the input port
    -- (otherwise some signals of the Block Instance can remain unkown
    --  and propagated)
    -- FIXME: Is this asking for too much?
    --        It's inneficient anyway ... 
    --        And ugly
    do if isJust maybeUndefOEntry then 
           throwerror  "signal " ++ id ++ " of instance " ++ name 
            ++ " hasn't been supplied yet"
       else withSignalOfType (HDSignalType t)
               (\a -> f ( ((wrap $ HDSignalType t) s)
                  `asTypeOf` a))
    
    where maybeUndefOEntry = do (id,_,_) <- find (\(_,_,s) -> isUndef s) op 
                                return id  
  | | otherwise = replaceN

    where (id,t,s) = 
             | maybe ip !! ix
         checkNoUndef op = 
            |  all (\ BlockInsOS id (zipWith join pd xs) 
         join (id,_) s 
          | isUndefS s = error $ 
          | otherwise  = (id,s)


-}
-------------------
-- Helper Functions
-------------------

getIds :: PortDesc -> [String]
getIds = fst.unzip

wrongPortPar :: Int -> PortDesc -> EProne PortDesc
-- Check port creation parameters for wrong values
wrongPortPar l pd
  | l /= length pd = throwError lErr  
  | otherwise      = maybe (return pd) (\id -> throwError (mulErr id)) maybeDup
 where maybeDup  = (findDup.getIds)  pd
       idsL      = length  pd
       mulErr id = "multiply defined identifier: " ++ id
       lErr      = "supplied length (" ++ show l ++ 
                   ") doesn't match descriptor length(" ++ show idsL ++")"
       

findDup :: Eq a => [a] -> Maybe a
-- find a duplicate in a list
findDup []  = Nothing 
findDup [x] = Nothing
findDup (x:xs)
 | elem x xs = Just x
 | otherwise = findDup xs


replaceN :: Int -> a -> [a] -> [a]
replaceN n x = (uncurry (++)).(fmap ((x:).tail)).(splitAt n)

push2 :: (a -> b -> c -> d) -> b -> c -> a -> d 
push2 f = (\b c a -> f a b c)  
