{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module ButtonEncoder where

import ForSyDe
import Language.Haskell.TH.Lift
import Data.Generics (Data, Typeable)
import Prelude hiding (Either(..))

-- Combinational syncrhonous system to encode the keypresses of
-- an arrow-keypad into directions which are easier to process
-- 
-- This example shows how the VHDL backend can process custom Enumerated
-- Algebraic types (ie. Algebraic types whose data constructors have all
-- zero arity)


data Direction = Up | Down | Left | Right | Unknown
 deriving (Data,Typeable,Show)

$(deriveLift1 ''Direction)

type ButtonPress = (Bit, -- is left pressed 
                    Bit, -- is right pressed
                    Bit, -- is up pressed  
                    Bit) -- is down pressed

buttonEncoder :: Signal ButtonPress -> Signal Direction  
buttonEncoder = mapSY "encoder" transFun
  where transFun = 
           $(newProcFun [d| transFun (left,right,up,down) =
                               if left  == H then Left else
                                if right == H then Right else
                                 if up == H then Up else
                                  if down == H then Down else Unknown |])

buttonEncoderSysDef :: SysDef (Signal ButtonPress -> Signal Direction)
buttonEncoderSysDef = $(newSysDefTHName 'buttonEncoder ["buttonPress"] ["direction"])
                                                    