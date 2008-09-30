{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.GlobalNameTable
-- Copyright   :  (c) SAM Group, KTH/ICT/ECS 2007-2008
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Global tranlsation table (symbol table) from Template Haskell Names to VHDL 
-- expressions
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.GlobalNameTable (globalNameTable) where

import Data.Bits
import Language.Haskell.TH


import ForSyDe.AbsentExt
import qualified ForSyDe.Bit as B (not)
import ForSyDe.Bit hiding (not)
import qualified ForSyDe.Backend.VHDL.AST as VHDL
import ForSyDe.Backend.VHDL.AST
import ForSyDe.Backend.VHDL.Constants
import ForSyDe.Backend.VHDL.Generate
import qualified Data.Param.FSVec as V

-- | Global tranlsation table from Template Haskell function and
-- constructor Names to VHDL expressions. The table works like this
--
-- (function, constant constructor name, (arity, function to obtain a
--                                               VHDL expression
--                                               provided its
--                                               arguments are already
--                                               translated to VHDL   ))
globalNameTable :: [(Name, (Int, [VHDL.Expr] -> VHDL.Expr ) )]
globalNameTable = [
-- Unary constructors
  ('Prst            , (1, genExprFCall1L presentId                       ) ),
-- Constant constructors
  ('True            , (0, genZeroConsCall trueExpr                       ) ),
  ('False           , (0, genZeroConsCall falseExpr                      ) ),
  ('H               , (0, genZeroConsCall highExpr                       ) ),
  ('L               , (0, genZeroConsCall lowExpr                        ) ),
  ('Abst            , (0, genZeroConsCall (PrimName $ NSimple absentId)  ) ),
-- Quaternary functions
  ('V.select        , (4, genExprFCall4L selId                           ) ),
-- Binary functions
  ('(&&)            , (2, genBinOpCall And                               ) ),
  ('(||)            , (2, genBinOpCall Or                                ) ),
  ('(.&.)           , (2, genBinOpCall And                               ) ),
  ('(.|.)           , (2, genBinOpCall Or                                ) ),
  ('xor             , (2, genBinOpCall Xor                               ) ),
  ('(==)            , (2, genBinOpCall (:=:)                             ) ),
  ('(/=)            , (2, genBinOpCall (:/=:)                            ) ),
  ('(<)             , (2, genBinOpCall (:<:)                             ) ), 
  ('(<=)            , (2, genBinOpCall (:<=:)                            ) ),
  ('(>)             , (2, genBinOpCall (:>:)                             ) ),
  ('(>=)            , (2, genBinOpCall (:>=:)                            ) ),  
  ('(+)             , (2, genBinOpCall (:+:)                             ) ),
  ('(-)             , (2, genBinOpCall (:-:)                             ) ),
  ('(*)             , (2, genBinOpCall (:*:)                             ) ),
  ('div             , (2, genBinOpCall (:/:)                             ) ),
  ('mod             , (2, genBinOpCall Mod                               ) ),
  ('rem             , (2, genBinOpCall Rem                               ) ),
  ('(^)             , (2, genBinOpCall (:**:)                            ) ),
  ('(V.+>)          , (2, genExprFCall2L plusgtId                        ) ),
  ('(V.<+)          , (2, genExprFCall2L ltplusId                        ) ),
  ('(V.++)          , (2, genExprFCall2L plusplusId                      ) ),
  ('(V.!)           , (2, genExprFCall2L exId                            ) ),
  ('V.take          , (2, genExprFCall2L takeId                          ) ),
  ('V.drop          , (2, genExprFCall2L dropId                          ) ),
  ('V.shiftl        , (2, genExprFCall2L shiftlId                        ) ),
  ('V.shiftr        , (2, genExprFCall2L shiftrId                        ) ),
  ('V.copy          , (2, genExprFCall2L copyId                          ) ),
-- unary functions
  ('B.not           , (1, genUnOpCall Not                                ) ),
  ('not             , (1, genUnOpCall Not                                ) ),
  ('negate          , (1, genUnOpCall Neg                                ) ),
  ('abs             , (1, genUnOpCall Abs                                ) ),
  ('abstExt         , (1, genExprFCall1L presentId                       ) ),
  ('V.singleton     , (1, genExprFCall1L singletonId                     ) ),
  ('V.length        , (1, genExprFCall1L lengthId                        ) ),
  ('V.lengthT       , (1, genExprFCall1L lengthId                        ) ),
  ('V.genericLength , (1, genExprFCall1L lengthId                        ) ),
  ('V.null          , (1, genExprFCall1L isnullId                        ) ),
  ('V.head          , (1, genExprFCall1L headId                          ) ),
  ('V.last          , (1, genExprFCall1L lastId                          ) ),
  ('V.init          , (1, genExprFCall1L initId                          ) ),
  ('V.tail          , (1, genExprFCall1L tailId                          ) ),
  ('V.rotl          , (1, genExprFCall1L rotlId                          ) ),
  ('V.rotr          , (1, genExprFCall1L rotrId                          ) ),
  ('V.reverse       , (1, genExprFCall1L reverseId                       ) ),
  ('toBitVector8    , (1, genExprFCall1L toBitVector8Id                  ) ),
  ('toBitVector16   , (1, genExprFCall1L toBitVector16Id                 ) ),
  ('toBitVector32   , (1, genExprFCall1L toBitVector32Id                 ) ),
  ('fromBitVector8  , (1, genExprFCall1L fromBitVector8Id                ) ),
  ('fromBitVector16 , (1, genExprFCall1L fromBitVector16Id               ) ),
  ('fromBitVector32 , (1, genExprFCall1L fromBitVector32Id               ) ),
-- constants
  ('V.empty               , (0, genExprFCall0L emptyId                         ) )]
 where genBinOpCall op = \[e1, e2] -> e1 `op` e2
       genUnOpCall op = \[e] -> op e
       genZeroConsCall cons = \[] -> cons
