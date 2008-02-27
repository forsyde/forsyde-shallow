-- Generated with ForSyDe
library forsyde;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity plus1 is
     port (plus1Input : in forsyde.types.int32;
           plus1Output : out forsyde.types.int32;
           clk : in std_logic;
           resetn : in std_logic);
end entity plus1;


architecture Structural of plus1  is
     signal mapSY_0 : forsyde.types.int32;
begin
     plus1Output <= mapSY_0;
     
     mapSY_0_block : block
          port (mapSY_input : in forsyde.types.int32;
                mapSY_output : out forsyde.types.int32);
          port map (mapSY_input => plus1Input,
                    mapSY_output => mapSY_0);
          function doPlus1 (a : forsyde.types.int32)
                           return forsyde.types.int32 is
          begin
               return a + TO_SIGNED (1, 32);
          end;
     begin
          mapSY_output <= doPlus1 (a => mapSY_input);
     end block mapSY_0_block;
end architecture Structural;
