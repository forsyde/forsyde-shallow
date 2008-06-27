library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
  
package types is
  
  function default return integer;
  function show (i : integer) return string;  
  
-- Commented out due to representation overflow (modelsim integers
-- are 32bits long)
-- subtype int64 is integer range -(2**(64-1)) to +(2**(64-1)-1);  

-- Commented out due to a overflow in 2**32
-- subtype int32 is integer range -(2**(32-1)) to +(2**(32-1)-1);  
  subtype int32 is integer range -2147483648 to +2147483647;  
  
  subtype int16 is integer range -(2**(16-1)) to +(2**(16-1)-1);  

  subtype int8 is integer range -(2**(8-1)) to +(2**(8-1)-1);

  function default return std_logic;
  function show (s : std_logic) return string;
  
  function default return boolean;
  function show (b : boolean) return string;
  
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

  function default return integer is
  begin 
   return 0;
  end default;

  function show (i : integer) return string is
  begin
    return integer'image(i);
  end show;

  function default return std_logic is
  begin
   return '0';
  end default;

  function show (s : std_logic) return string is
  begin
    if s = '1' then
      return "H";
    else
      return "L";
    end if;
  end show;

  
  function default return boolean is
  begin
   return true;
  end default;

  function show (b : boolean) return string is
  begin
    if b then
      return "True";
    else
      return "False";
    end if;
  end show;

  
end types;
