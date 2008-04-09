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


-- | reset and clock signal identifiers in String form
--   note that there are in lower case and thus (due to VHDL basic identifiers
--   being case insensitive) in order to check if an identifier is reserved
--   it first needs to be mapped to lowercase before comparing. 
resetStr, clockStr :: String
resetStr = "resetn"
clockStr = "clock"

-- | reserved identifiers in String form
reservedStrs :: [String]
reservedStrs = [resetStr, clockStr]

-- | reset and clock signal identifiers in basic VHDLId form
resetId, clockId :: VHDLId
resetId = unsafeVHDLBasicId resetStr
clockId = unsafeVHDLBasicId clockStr


-- | reserved Identifiers
reservedIds :: [VHDLId]      
reservedIds = [resetId, clockId]

-- | Stardard logic type mark
std_logicTM :: TypeMark
std_logicTM =  unsafeVHDLBasicId "std_logic"

-- | int32 typemark (defined in ForSyDe's VHDL library)
int32TM :: TypeMark
int32TM = unsafeVHDLBasicId "int32"

-- | types identifier
typesId :: VHDLId
typesId = unsafeVHDLBasicId "types"

-- | work identifier
workId :: VHDLId
workId = unsafeVHDLBasicId "work"