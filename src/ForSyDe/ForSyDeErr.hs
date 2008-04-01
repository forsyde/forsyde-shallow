-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.ForSyDeErr
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- ForSyDe error-related types and functions.
--
-----------------------------------------------------------------------------
module ForSyDe.ForSyDeErr 
 (ForSyDeErr(..),
  ContextErr(..),
  Context(..),
  SysDefLoc,
  EProne,
  liftEither,
  intError,
  qError,
  qGiveUp,
  qPutTraceMsg,
  printError,
  printVHDLError,
  module Control.Monad.Error,
  module Debug.Trace) where


import {-# SOURCE #-} ForSyDe.Netlist
import ForSyDe.Ids

import Debug.Trace
import Control.Monad.Error 
import Data.Dynamic
import Data.Typeable
import Language.Haskell.TH.Syntax (Name, Dec, Type, Quasi(..))
import Language.Haskell.TH.Ppr (pprint)

-------------
-- ForSyDeErr
-------------

-- | All Errors thrown or displayed in ForSyDe
data ForSyDeErr = 
  -- Used in ForSyDe.Netlist
  EvalErr String                             |

  -- Used in ForSyDe.System.*
  -- | Not a variable name
  NonVarName Name                            | 
  -- | Incompatible system function
  IncomSysF  Name Type                       | 
  -- | Incompatible input interface length                
  InIfaceLength   (Name,Int) ([String],Int)  | 
  -- | Incompatible output interface length
  OutIfaceLength  (Name,Int) ([String],Int)  |
  -- | Multiply defined port identifier                
  MultPortId  String                         |
  -- | Not a SysDef variable
  NonSysDef Name Type                        |

  -- Used in ForSyDe.Proc.ProcFun 
  -- | Incorrect Declarations provided to create a ProcFun
  IncorrProcFunDecs [Dec]                    |

  -- Used in ForSyDe.Netlist.Traversable     
  -- | Inconsistent output tag
  InconsOutTag NlNodeOut                     |

  -- Used in ForSyDe.Backend.Simulate
  -- | Inconsistent System Definition Port
  InconsSysDefPort PortId                    | 
  -- | Dynamic type mismatch
  DynMisMatch Dynamic TypeRep                |
  -- | Signature mismatch
  SigMisMatch Type                           |
  -- | Simulation input signals length mismatch
  InLengthMisMatch Int Int                   | 

  -- Used in ForSyDe.Backend.VHDL.*
  -- | Empty VHDL identifier
  EmptyVHDLId                                |
  -- | Incorrect Basic VHDL Identifier
  IncVHDLBasId String                        |
  -- | Incorrect Extended VHDL Identifier
  IncVHDLExtId String                        |

  -- Common Backend errors              
  -- | UnsUpported type
  UnsupportedType TypeRep                    |
  -- | Reserved identifier
  ReservedId String                          |
  -- | Unsupported process
  UnsupportedProc                            |

  -- | Other Errors
  Other String           

 

-- | Show errors
instance Show ForSyDeErr where
 show (EvalErr str) = "Non evaluable node (" ++ show str ++ ")"
 show (NonVarName name) = show name ++ " is not a variable name."
 show (IncomSysF fName inctype) = 
   "Incompatible system function type\n"++
   show strFName ++ " was expected to have type:\n" ++
   "  Signal i1 -> Signal i2 -> ..... -> Signal in ->\n" ++
   "  (Signal o1, Signal o2, ... , Signal om)\n" ++
   "  with n <- |N U {0} and m <- |N U {0}\n"  ++ 
   "       i1 .. in, o1 .. im monomorphic types\n" ++
   "However " ++ strFName ++ " has type\n  " ++
   "  " ++ pprint inctype
  where strFName = show fName
 show (InIfaceLength   sysFInInfo portIdsInInfo) =
    showIfaceLength "input interface" sysFInInfo portIdsInInfo
 show (OutIfaceLength   sysFOutInfo portIdsOutInfo) =
    showIfaceLength "output interface" sysFOutInfo portIdsOutInfo
 show (MultPortId  portId) = 
   "Multiply defined port identifier " ++ show portId
 show (NonSysDef name t) = 
   "A variable with SysDef type was expected\n" ++
   "However " ++ show name ++ " has type " ++ pprint t
 show (IncorrProcFunDecs decs) =
  "Only a function declaration (possibly precedeeded by a signature)" ++ 
  "is accepted\n"++ 
  "The specific, incorrect declarations follow:\n" ++
  pprint decs
 show (InconsOutTag nlNodeOut) = "Inconsistent output tag: " ++ show nlNodeOut
 show (InconsSysDefPort id) = "Inconsistent port in SysDef: " ++ show id
 show (DynMisMatch dyn rep) = 
   "Type matching error in dynamic value with typerep " ++ 
   show (dynTypeRep dyn) ++
   "\n(Expected type: " ++ show rep ++ " )."
 show (SigMisMatch t) = 
  "Signal mismatch:  expected a Signal type but got " ++ pprint t 
 show (InLengthMisMatch l1 l2) = 
   "Cannot simulate: simulation arguments length-mismatch: " ++ 
     show l1 ++ " /= " ++ show l2
 show EmptyVHDLId = "Empty VHDL identifier"
 show (IncVHDLBasId id)  = "Incorrect VHDL basic identifier " ++ 
                           "`" ++ id ++ "'"
 show (IncVHDLExtId id)  = "Incorrect VHDL extended identifier " ++ 
                           "`" ++ id ++ "'"
 show (UnsupportedType tr) = "Unsupported type " ++ show tr
 show (ReservedId str)  = "Identifier `" ++ str ++ "' is reserved"
 show UnsupportedProc = "Unusupported process"
 show (Other str) = str 

-- | help function for the show instance
showIfaceLength :: String -> (Name,Int) -> ([String],Int) -> String
showIfaceLength ifaceMsg  (sysFName,sysFIfaceL) (ifaceIds, ifaceL) = 
   "Incorrect length of " ++ ifaceMsg  ++ " (" ++ show ifaceL ++ ")\n" ++
   "  " ++ show ifaceIds ++ "\n" ++    
   show sysFName ++ " expects an " ++ show ifaceMsg ++ " length of " ++
   show sysFIfaceL 

-----------------
-- Context Error
-----------------

-- | A context error: a 'ForSyDeErr' with context information (indicating where
--   the error ocurred)
data ContextErr = ContextErr Context ForSyDeErr

-- | A context: it indicates where an error ocurred.
data Context = SysDefC SysId SysDefLoc        | -- ^ In a System definition 
               ProcC   SysId SysDefLoc ProcId | -- ^ In a process 
               Empty                            -- ^ Empty context

-- | type indicating where a system definition is located in the user's source
--   code
type SysDefLoc = String

instance Show Context where
 show (SysDefC id loc)   = "system definition " ++ id ++ 
                           " (created in " ++ loc ++ ")"
 show (ProcC sysid sysloc pid) = "process `" ++ pid ++ "' belonging to " ++ 
                      show (SysDefC sysid sysloc)
 show Empty = ""

instance Show ContextErr where
 show (ContextErr cxt err) = case cxt of
   Empty -> show err
   _ -> show err ++ "\nin " ++ show cxt

--------------
-- Error Monad
--------------

-- | We make ForSyDeErr an instance of the Error class to be able to throw it
-- as an exception.
instance Error ForSyDeErr where
 noMsg  = Other "An Error has ocurred"
 strMsg = Other


instance Error ContextErr where
 noMsg = ContextErr Empty noMsg
 strMsg = \str ->  ContextErr Empty (Other str)

-- | 'EProne' represents failure using Left ForSyDeErr  or a successful 
--   result of type a using Right a
-- 
--  'EProne' is implicitly an instance of 
--   ['MonadError']  (@Error e => MonadError e (Either e)@)
--   ['Monad']       (@Error e => Monad (Either e)@)
type EProne a = Either ForSyDeErr a


-------------------
-- Helper functions
-------------------

-- | Throws an internal error
intError :: String     -- ^ Function which caused the internal error 
         -> ForSyDeErr -- ^ Error to show
         -> a
intError funName err = error $ "Internal error in " ++ funName ++ ": " ++ 
                               show err ++ "\n" ++
                               "Please report!"

-- | lift an Either expression to an Error Monad
liftEither :: MonadError e m => Either e a  -> m a
liftEither = either throwError return



-- | An error reporting function for Quasi monads
--   Executing in the monad will stop inmideately after calling qError
-- Note, it does not work for GHC<6.8
-- see <http://hackage.haskell.org/trac/ghc/ticket/1265>
qError :: Quasi m => String      -- ^ The name of the function  
                                 --   called in the splice 
                     -> ForSyDeErr  -- ^ Error to show
                     -> m a
qError fname err =  fail $ "Error when calling " ++ fname ++ ":\n"  ++ 
                           show err


-- | Stop execution, find the enclosing qRecover
--   if a recover is not found, it is considered as an internal error
--   and the string provided will used as a reference to 
--   the origin of the error.
-- Note, it does not work for GHC<6.8
-- see <http://hackage.haskell.org/trac/ghc/ticket/1265>
qGiveUp :: Quasi m  =>  String -> m a
qGiveUp name = fail $ "qGiveUp: Internal error in " ++ name ++ 
                      ", please report."

-- | Output a trace message in a quasi monad (similar to 'putTraceMsg')
qPutTraceMsg :: Quasi m => String -> m ()
qPutTraceMsg msg = qRunIO (putTraceMsg msg)


-- | Print an Error
printError :: Show a => a -> IO ()
printError = putStrLn.("Error: "++).show

-- | Print a VHDL compilation error
printVHDLError :: Show a => a -> IO ()
printVHDLError = putStrLn.("VHDL Compilation Error: "++).show