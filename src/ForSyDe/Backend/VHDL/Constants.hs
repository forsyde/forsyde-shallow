-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Backend.VHDL.Constants
-- Copyright   :  (c) The ForSyDe Team 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ecs_forsyde_development@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Constants used in the VHDL backend
--
-----------------------------------------------------------------------------
module ForSyDe.Backend.VHDL.Constants where

import ForSyDe.Backend.VHDL.AST

-- | Standard context clause used in all generated vhdl files. It imports
commonContextClause :: [ContextItem]
commonContextClause = 
  [Library forsydeId, 
   Library ieeeId,
   Use     $ NSelected (NSimple forsydeId :.: SSimple typesId) :.: All,
   Use     $ NSelected (NSimple ieeeId :.: SSimple std_logic_1164Id) :.: All,
   Use     $ NSelected (NSimple ieeeId :.: SSimple numeric_stdId) :.: All]
 where forsydeId = unsafeVHDLBasicId "forsyde" 
       ieeeId = unsafeVHDLBasicId "ieee"
       typesId = unsafeVHDLBasicId "types"
       std_logic_1164Id = unsafeVHDLBasicId "std_logic_1164"
       numeric_stdId = unsafeVHDLBasicId "numeric_std"

--------------
-- Identifiers
--------------

-- | reset and clock signal identifiers in String form
resetStr, clockStr :: String
resetStr = "resetn"
clockStr = "clock"

-- | reset and clock signal identifiers in basic VHDLId form
resetId, clockId :: VHDLId
resetId = unsafeVHDLBasicId resetStr
clockId = unsafeVHDLBasicId clockStr


-- | \"types\" identifier
typesId :: VHDLId
typesId = unsafeVHDLBasicId "types"

-- | work identifier
workId :: VHDLId
workId = unsafeVHDLBasicId "work"

-- | default function identifier
defaultId :: VHDLId
defaultId = unsafeVHDLBasicId "default"

-- | absent function identifier
absentId :: VHDLId
absentId = unsafeVHDLBasicId "absent"

-- | present function identifier
presentId :: VHDLId
presentId = unsafeVHDLBasicId "present"

-- | fromAbstExt function identifier
fromAbstExtId :: VHDLId
fromAbstExtId = unsafeVHDLBasicId "fromAbstExt"

-- | unsafeFromAbstExt function identifier
unsafeFromAbstExtId :: VHDLId
unsafeFromAbstExtId = unsafeVHDLBasicId "unsafeFromAbstExt"

-- | value element identifier
valueId :: VHDLId
valueId = unsafeVHDLBasicId "value"


-- | isPresent function and element identifier
isPresentId :: VHDLId
isPresentId = unsafeVHDLBasicId "isPresent"

-- | isAbsent function identifier
isAbsentId :: VHDLId
isAbsentId = unsafeVHDLBasicId "isAbsent"


--------
-- Names
--------

defaultSN :: VHDLName
defaultSN = NSimple defaultId

------------------
-- VHDL type marks
------------------

-- | Stardard logic type mark
std_logicTM :: TypeMark
std_logicTM = unsafeVHDLBasicId "std_logic"

-- | boolean type mark
booleanTM :: TypeMark
booleanTM = unsafeVHDLBasicId "boolean"

-- | int32 typemark (defined in ForSyDe's VHDL library)
int64TM :: TypeMark
int64TM = unsafeVHDLBasicId "int64"

-- | int32 typemark (defined in ForSyDe's VHDL library)
int32TM :: TypeMark
int32TM = unsafeVHDLBasicId "int32"

-- | int32 typemark (defined in ForSyDe's VHDL library)
int16TM :: TypeMark
int16TM = unsafeVHDLBasicId "int16"

-- | int8 typemark (defined in ForSyDe's VHDL library)
int8TM :: TypeMark
int8TM = unsafeVHDLBasicId "int8"

--------------
-- Expressions
--------------

-- | true expression
trueExpr :: Expr
trueExpr = PrimName (NSimple $ unsafeVHDLBasicId "true")
 
-- | false expression
falseExpr :: Expr
falseExpr = PrimName (NSimple $ unsafeVHDLBasicId "false")

-- | \'0\' bit expression
lowExpr :: Expr
lowExpr = PrimLit "'0'"

-- | \'1\' bit expression
highExpr :: Expr
highExpr = PrimLit "'1'"
