library ieee;
use ieee.numeric_std.all;

package types is
  type int32 is range -(2**(32-1)) to +(2**(32-1)-1);  -- 32 bit integers 
  function default return int32;
  type int16 is range -(2**(16-1)) to +(2**(16-1)-1);  -- 16 bit integers
  function default return int16;

end types;

package body types is
  function default return int32 is
  begin 
   return 0;
  end default;

  function default return int16 is
  begin
   return 0;
  end default;

end types;
