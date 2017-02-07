-----------------------------------------------------------------------------------------
--
-- nbit_adder.vhd
--
-- Performs n-bit full addition with carry in and carry out.
--
-- Entities:
--      FullAdder
--          A full adder
--      NBitAdder
--          N-bit adder that utilizes fulladder
--
-- Revision History:
--      01 Feb 17   Tim Menninger       Created
--

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;

--
-- FullAdder entity
--
entity FullAdder is
    port (
        Cin     : in  std_logic;
        a       : in  std_logic;
        b       : in  std_logic;

        Cout    : out std_logic;
        result  : out std_logic
    );
end entity;

--
-- Architecture
--
architecture add of FullAdder is
begin
    -- Compute the result
    result  <= a xor b xor Cin;
    -- Fill Cout
    Cout    <= (a and b) or (a and Cin) or (b and Cin);
end architecture;

-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;

--
-- adder entity
--
-- Generic:
--      n : number of bits
--
-- Inputs:
--      Cin : std_logic
--          Carry in
--      a : std_logic_vector(n-1 downto 0)
--          First input
--      b : std_logic_vector(n-1 downto 0)
--          Second input
--
-- Outputs:
--      Cout : std_logic
--          Carry out
--      result : std_logic_vector(n-1 downto 0)
--          The sum of a and b
--
entity NBitAdder is
    generic (
        n       : natural := 8
    );
    port (
        Cin     : in  std_logic;
        a       : in  std_logic_vector(n-1 downto 0);
        b       : in  std_logic_vector(n-1 downto 0);

        Cout    : out std_logic;
        result  : out std_logic_vector(n-1 downto 0)
    );
end entity;

--
-- architectuer
--
architecture add of NBitAdder is
    -- Full adder component
    component FullAdder is
        port (
            Cin     : in  std_logic;
            a       : in  std_logic;
            b       : in  std_logic;

            Cout    : out std_logic;
            result  : out std_logic
        );
    end component;

    -- Temporary vector to hold Couts (and Cin at beginning)
    signal Couts : std_logic_vector(n downto 0);

begin

    -- Fill what we know about the Couts and output what we need
    Couts(0) <= Cin;
    Cout     <= Couts(n);

    -- Generate the n-bit adder
    NthFullAdder: for i in 0 to n-1 generate
        FullAdderN: FullAdder port map (Couts(i), a(i), b(i), Couts(i+1), result(i));
    end generate;

end architecture;
