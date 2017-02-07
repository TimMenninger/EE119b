-----------------------------------------------------------------------------------------
--
-- nbit_fblock.vhd
--
-- Performs n-bit AND
--
-- Entities:
--      NBitFBlock
--          Computes a AND b
--
-- Revision History:
--      01 Feb 17   Tim Menninger       Created
--

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;

--
-- NBitFBlock entity
--
-- Generic:
--      n : natural
--          Number of bits in FBlock
--
-- Inputs:
--      a : std_logic_vector(n-1 downto 0)
--          First operand of AND
--      b : std_logic_vector(n-1 downto 0)
--          Second operand of AND
--
-- Outputs:
--      aANDb : std_logic_vector(n-1 downto 0)
--          Computed result a AND b
--      aXORb : std_logic_vector(n-1 downto 0)
--          Computed result a XOR b
--
entity NBitFBlock is
    generic (
        n           : natural := 8                          -- Number of bits
    );
    port (
        a       : in  std_logic_vector(n-1 downto 0);       -- First operand
        b       : in  std_logic_vector(n-1 downto 0);       -- Second operand
        aANDb   : out std_logic_vector(n-1 downto 0);       -- Computed result
        aXORb   : out std_logic_vector(n-1 downto 0)        -- Computed result
    );
end entity;

--
-- Architecture
--
architecture compute of NBitFBlock is
begin
    aANDb <= a and b;
    aXORb <= a xor b;
end architecture;
