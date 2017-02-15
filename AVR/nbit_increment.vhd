-----------------------------------------------------------------------------------------
--
-- nbit_increment.vhd
--
-- Performs increment of n-bit value where $ff...f incremented equals $00...0
--
-- Entities:
--      bitFlipper:
--          Flips the bit if enable is low
--      NBitIncrementer:
--          Increments n-bit value
--
-- Revision History:
--      15 Feb 17   Tim Menninger       Created
--

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

--
-- bitFlipper entity
--
-- Inputs:
--      aIn : std_logic
--          Unflipped bit
--      EN : std_logic
--          Active high enables flip
--
-- Outputs:
--      aOut : std_logic
--          Flipped bit if enabled, unflipped otehrwise
--
entity bitFlipper is
    port (
        aIn     : in  std_logic;
        EN      : in  std_logic;
        doNext  : out std_logic;
        aOut    : out std_logic
    );
end bitFlipper;

--
-- architecture flip
--
-- Flips the bit if necessary
--
architecture flip of bitFlipper is
begin
    -- Set the flipped bit when eabled
    aOut <= aIn when EN = '0' else
            '1' when aIn = '0' else
            '0';

    -- We stop incrementing when we are no long enabled or this bit was a 0
    doNext <= '1' when EN = '0' and aIn = '1' else '0';
end architecture;

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

--
-- NBitIncrementer entity
--
-- Generic:
--      n : number of bits
--
-- Inputs:
--      a : std_logic_vector(n-1 downto 0)
--          First input
--
-- Outputs:
--      result : std_logic_vector(n-1 downto 0)
--          a incremented
--
entity NBitIncrementer is
    generic (
        n       : natural := 8
    );
    port (
        a       : in  std_logic_vector(n-1 downto 0);
        result  : out std_logic_vector(n-1 downto 0)
    );
end entity;

--
-- architecture increment
--
-- Performs the increment
--
architecture increment of NBitIncrementer is

    -- Temporary vector to hold Couts (and Cin at beginning)
    signal enables : std_logic_vector(n downto 0);

    -- The component that flips bits in our increment
    component bitFlipper is
        port (
            aIn     : in  std_logic;
            EN      : in  std_logic;
            doNext  : out std_logic;
            aOut    : out std_logic
        );
    end component;

begin

    -- We enable the first flip
    enables(0) <= '1';

    -- Generate the n-bit incrementer
    NthIncrementer: for i in 0 to n-1 generate
        IncrementerN: bitFlipper port map (a(i), enables(i), enables(i+1), result(i));
    end generate;

end architecture;
