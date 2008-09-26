{-# LANGUAGE TemplateHaskell #-}
module FullAdder where

import ForSyDe

faNextState :: ProcFun (Bit -> (Bit,Bit) -> Bit)
faNextState = $(newProcFun
  [d| faNextState :: Bit -> (Bit,Bit) -> Bit
      faNextState st input =
         if st == L then
            case input of
              (H, H) -> H
              _ -> L
         else
            case input of
              (L,L) -> L
              _ -> H
     |])

faOut :: ProcFun (Bit -> (Bit,Bit) -> Bit)
faOut= $(newProcFun
  [d| faOut :: Bit -> (Bit,Bit) -> Bit
      faOut st input =
         if st == L then
            case input of
              (L, L) -> L
              (L, H) -> H
              (H, L) -> H
              (H, H) -> L
         else
            case input of
              (L, L) -> H
              (L, H) -> L
              (H, L) -> L
              (H, H) -> H 
     |])


faProc :: Signal (Bit,Bit) -> Signal Bit
faProc = mealySY "addProc" faNextState faOut L

faSysDef :: SysDef (Signal (Bit,Bit) -> Signal Bit)
faSysDef = newSysDef faProc "fullAdder" ["op1", "op2"] ["res"]
