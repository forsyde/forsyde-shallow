{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Encoder where


import ForSyDe
import Language.Haskell.TH.Lift (deriveLift1)

import Prelude hiding (Left, Right)
import Data.Generics (Data,Typeable)
import Data.Param.FSVec
import Data.TypeLevel.Num hiding ((==))


data Direction = Left | Down | Right | Up
 deriving (Typeable, Data, Eq, Show)

$(deriveLift1 ''Direction)

encoderFun :: ProcFun (FSVec D4 Bit -> AbstExt Direction)
encoderFun = $(newProcFun 
  [d| encode :: FSVec D4 Bit -> AbstExt Direction
      encode v = if v ! d0 == H then Prst Left  else
                 if v ! d1 == H then Prst Down  else
                 if v ! d2 == H then Prst Right else
                 if v ! d3 == H then Prst Up else Abst  |])

encoderProc :: Signal (FSVec D4 Bit) -> Signal (AbstExt Direction)
encoderProc = mapSY "encoder" encoderFun

encoderSysDef :: SysDef (Signal (FSVec D4 Bit) -> Signal (AbstExt Direction))
encoderSysDef = newSysDef encoderProc "KeypadEncoder" ["arrowBits"] ["direction"]