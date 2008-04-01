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
--   all the necessary libraries
contextClause :: [ContextItem]
contextClause = [Library (unsafeVHDLBasicId "forsyde"), 
                 Library (unsafeVHDLBasicId "ieee"   ),
                 Use     "ieee.std_logic_1164.all"   ]


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
std_logicTM = unsafeVHDLBasicId "std_logic"

-- | int32 typemark (defined in ForSyDe's VHDL library)
int32TM :: TypeMark
int32TM = unsafeVHDLBasicId "forsyde.types.int32"