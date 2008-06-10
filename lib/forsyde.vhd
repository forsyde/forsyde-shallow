library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
  
package types is

  
  
-- Commented out due to representation overflow (modelsim integers
-- are 32bits long)
--  type int64 is range -(2**(64-1)) to +(2**(64-1)-1);  -- 64 bit integers 
--  function default return int64;

-- Commented out due to a overflow in 2**32
--  type int32 is range -(2**(32-1)) to +(2**(32-1)-1);  -- 32 bit integers 
  type int32 is range -2147483648 to +2147483647;  -- 32 bit integers 
  function default return int32;

  type int16 is range -(2**(16-1)) to +(2**(16-1)-1);  -- 16 bit integers
  function default return int16;

  type int8 is range -(2**(8-1)) to +(2**(8-1)-1);   -- 8 bit integers
  function default return int8;

  function default return std_logic;

  function default return boolean;

  -- Indexes for unconstrained fsvecs:
  --  -1 is used to express the null vector, with bounds (0 to -1)
  subtype fsvec_index is integer range -1 to integer'high;
  
end types;

package body types is

-- Commented out due to representation overflow 
--  function default return int64 is
--  begin 
--   return 0;
--  end default;

  function default return int32 is
  begin 
   return 0;
  end default;

  function default return int16 is
  begin
   return 0;
  end default;

  
  function default return int8 is
  begin
   return 0;
  end default;

  function default return std_logic is
  begin
   return '0';
  end default;

  function default return boolean is
  begin
   return true;
  end default;

  
end types;
