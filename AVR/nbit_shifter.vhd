-----------------------------------------------------------------------------------------
--
-- nbit_rotator.vhd
--
-- Performs n-bit shift right once
--
-- Entities:
--      NBitShifter
--          Shifts n bits right once
--
-- Revision History:
--      01 Feb 17   Tim Menninger       Created
--

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;

--
-- NBitShifter entity
--
-- Generic:
--      n : natural
--          Number of bits in shifter
--
-- Inputs:
--      ShiftIn : std_logic
--          Bit that is shifted in on the left (next MSB)
--      ToShift : std_logic_vector(n-1 downto 0)
--          Value to shift right
--
-- Outputs:
--      Shifted : std_logic_vector(n-1 downto 0)
--          Shifted value
--      ShiftOut : std_logic
--          Bit shifted out (previous LSB)
--
entity NBitShifter is
    generic (
        n           : natural := 8                          -- Number of bits
    );
    port (
        ShiftIn     : in  std_logic;                        -- Bit shifted in
        ToShift     : in  std_logic_vector(n-1 downto 0);   -- Value to shift right
        Shifted     : out std_logic_vector(n-1 downto 0);   -- Shifted value
        ShiftOut    : out std_logic                         -- Bit shifted out
    );
end entity;

--
-- Architecture
--
architecture shift of NBitShifter is
begin
    -- Shift value
    ShiftOut                <= ToShift(0);
    Shifted(n-2 downto 0)   <= ToShift(n-1 downto 1);
    Shifted(n-1)            <= ShiftIn;
end architecture;
