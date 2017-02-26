----------------------------------------------------------------------------
--
--  Atmel AVR Program Memory
--
--  This component describes a program for the AVR CPU.  It creates the
--  program in a small (334 x 16) ROM.
--
--  Revision History:
--     11 May 00  Glen George       Initial revision (from 5/9/00 version of
--                                  progmem.vhd).
--     28 Jul 00  Glen George       Added instructions and made memory return
--                                  NOP when not mapped.
--      7 Jun 02  Glen George       Updated commenting.
--     16 May 04  Glen George       Added more instructions for testing and
--                                  updated commenting.
--     21 Jan 08  Glen George       Updated commenting.
--
----------------------------------------------------------------------------


--
--  PROG_MEMORY
--
--  This is the program memory component.  It is just a 334 word ROM with no
--  timing information.  It is meant to be connected to the AVR CPU.  The ROM
--  is always enabled and may be changed when Reset it active.
--
--  Inputs:
--    ProgAB - address bus (16 bits)
--    Reset  - system reset (active low)
--
--  Outputs:
--    ProgDB - program memory data bus (16 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library common;
use common.common.all;


entity PROG_MEMORY is
    port (
        ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
        Reset   :  in   std_logic;                      -- system reset
        ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
    );
end PROG_MEMORY;


architecture ROM of PROG_MEMORY is

    -- define the actual ROM (initialized to a simple program).  This is auto
    -- generated after the line containing the start token, then continues afterward.

    type ROMtype is array(0 to 500) of address_t;
    signal ROMbits : ROMtype :=  (
        "1001010010001000",
        "1001010011111000",
        "1001010011001000",
        "1001010010111000",
        "1001010010011000",
        "1001010011011000",
        "1001010010101000",
        "1001010011101000",
        "1001010001001000",
        "1001010000011000",
        "1001010001111000",
        "1001010000001000",
        "1001010000101000",
        "1001010001101000",
        "1001010001011000",
        "1001010000111000",
        "1110000000000000",
        "0010111000000000",
        "0010111000010000",
        "0010111000100000",
        "0010111000110000",
        "0010111001000000",
        "0010111001010000",
        "0010111001100000",
        "0010111001110000",
        "1111100000000111",
        "1111100000010011",
        "1111100000100001",
        "1111100000110110",
        "1111100001000000",
        "1111100001010101",
        "1111100001100100",
        "1111100001110010",
        "1110110101001111",
        "0010111010000100",
        "1110000001000100",
        "0010111010010100",
        "1110011101001111",
        "0010111010100100",
        "1110000001000001",
        "0010111010110100",
        "1110111101001101",
        "0010111011000100",
        "1110010001000000",
        "0010111011010100",
        "1110111101000111",
        "0010111011100100",
        "1110000101000000",
        "0010111011110100",
        "1111101010000101",
        "1111101010010010",
        "1111101010100111",
        "1111101010110000",
        "1111101011000001",
        "1111101011010110",
        "1111101011100011",
        "1111101011110100",
        "1110111100001111",
        "1110111100011111",
        "1110000000100000",
        "1110011100110000",
        "1110000001000000",
        "1110011101011110",
        "1110011101101110",
        "1110100001110000",
        "1110010010000101",
        "1110100010010000",
        "1110111110100000",
        "1110111110111111",
        "1110010111000101",
        "1110101011011010",
        "1110011111100000",
        "1110001111111111",
        "0001111100000001",
        "0001111100000010",
        "0001111100100001",
        "0001111110010101",
        "0001110101100000",
        "0000111110010101",
        "0000111100000001",
        "0000111100100100",
        "0000110101110000",
        "1001011000000000",
        "1001011001010000",
        "0010001100011100",
        "0010001100011101",
        "0010000011010000",
        "0111111111001111",
        "0111111100001111",
        "0111000000000000",
        "0111111111011111",
        "1001010100000101",
        "1001010110110101",
        "1001010111100101",
        "1001010010000101",
        "1001010100000000",
        "1001010100000000",
        "1001010111000000",
        "1001010111000000",
        "0001011100010000",
        "0001011101011010",
        "0001011111111111",
        "0000011100010000",
        "0000011101010101",
        "0000011101010001",
        "0000011101010101",
        "0000011101010000",
        "1110010011100000",
        "1110011111111111",
        "0011011100011111",
        "0011010011100000",
        "0011001111110000",
        "0010111111100011",
        "1001010100011010",
        "1001010000001010",
        "1001010100011010",
        "0010111100011100",
        "0010011100011101",
        "0010011100011100",
        "0010011100100001",
        "0010011100100000",
        "0010011110001000",
        "1001010110000011",
        "1001010101100011",
        "1001010101100011",
        "1001010000000011",
        "1001010110101010",
        "1110100011110000",
        "1001010110100110",
        "1001010111100110",
        "1001010101000110",
        "1001010111110110",
        "1001010100000001",
        "1001010101100001",
        "1001010000000001",
        "1001010101010001",
        "0010101100100001",
        "0010101100101100",
        "0010101101100001",
        "0110111100011111",
        "0110000000010000",
        "0110011110011101",
        "1001010100010111",
        "1001010100110111",
        "1001010100000111",
        "1001010000000111",
        "1001010100000111",
        "1110010111110000",
        "0000101100000001",
        "0100011111110000",
        "0000101010100100",
        "1110011110011111",
        "1110011110000001",
        "0100011110101111",
        "0000101110010001",
        "0100101010000000",
        "1110000010001101",
        "1110000010010000",
        "1001011101000000",
        "1001011100000000",
        "1110011111101111",
        "1110011111111111",
        "0001101100000001",
        "0001101111110100",
        "1110010111100000",
        "1110011111110001",
        "0101011101001111",
        "0101011111100000",
        "0101000011110000",
        "1001010101010010",
        "1001010010100010",
        "1001010110110010",
        "1001001110101111",
        "1001001110111111",
        "1001001111001111",
        "1001001111011111",
        "1001001111101111",
        "1001001111111111",
        "1001000000001111",
        "1001000000011111",
        "1001000000101111",
        "1001000000111111",
        "1001000001001111",
        "1001000001011111",
        "1110111110111111",
        "1110111110101111",
        "1110111111011111",
        "1110110011000000",
        "1110000011110000",
        "1110100011100000",
        "1001001000000000",
        "0101010101010101",
        "1001001000010000",
        "1010101010101010",
        "1001001000101100",
        "1001001000111110",
        "1001001001001101",
        "1001001001011101",
        "1001001001101100",
        "1001001001001110",
        "1001001001111001",
        "1000001010001000",
        "1001001010011010",
        "1000001010101000",
        "1010111010111100",
        "1000001011001010",
        "1000101011011110",
        "1000001011101001",
        "1001001011110001",
        "1000001100000000",
        "1001001100010010",
        "1000001100100000",
        "1000111100110110",
        "1000001101000001",
        "1010111101010111",
        "1010001101100000",
        "1110111111011111",
        "1110111011000000",
        "1001001101111010",
        "1001001110001001",
        "1010111110011111",
        "1110000010110000",
        "1110000010100000",
        "1110111111011111",
        "1110111111001111",
        "1110111111111111",
        "1110110011100000",
        "1001000001100000",
        "1010101010101010",
        "1001000001110000",
        "0101010101010101",
        "1001000010001100",
        "1001000010011110",
        "1001000010101101",
        "1001000010111100",
        "1001000011001001",
        "1000000011011000",
        "1001000011101010",
        "1010000011111000",
        "1001000100000001",
        "1000000100010000",
        "1001000100100010",
        "1010110100110100",
        "1001001010001111",
        "1001001010011111",
        "1001001010101111",
        "1001001010111111",
        "1001001011001111",
        "1001001011011111",
        "1001001011101111",
        "1001001011111111",
        "1001001100001111",
        "1001001100011111",
        "1001001100101111",
        "1001001100111111",
        "1001000101001111",
        "1001000101011111",
        "1001000101101111",
        "1001000101111111",
        "1001000110001111",
        "1001000110011111",
        "1001000110101111",
        "1001000110111111",
        "1001000111001111",
        "1001000111011111",
        "1001000111101111",
        "1001000111111111",
        "1001010000001100",
        "0000000100001111",
        "1110000010110000",
        "1110000011000000",
        "1110010101101010",
        "1110010101111010",
        "1100000000000010",
        "1110000010110000",
        "1110000011000000",
        "1110000111101001",
        "1110000011110001",
        "1001010000001001",
        "1110000010110000",
        "1110000011000000",
        "0010010000000000",
        "1001010000001110",
        "0000000110000110",
        "1101000001101001",
        "1110100011100110",
        "1110000011110001",
        "1001010100001001",
        "1110011111001111",
        "1110111110111111",
        "0001011111001011",
        "1111000000010000",
        "1001010000001100",
        "0000000100100000",
        "1111001111001100",
        "1111001111000001",
        "1111010000010001",
        "1001010000001100",
        "0000000100100000",
        "1110011001011001",
        "0000111101010101",
        "1111011110010101",
        "0010101110111011",
        "1111000000010010",
        "1001010000001100",
        "0000000100100000",
        "0010101111001100",
        "1111001101100010",
        "1111010000010010",
        "1001010000001100",
        "0000000100100000",
        "0010101110111011",
        "1111011100111010",
        "0001101111001011",
        "1111000000010011",
        "1001010000001100",
        "0000000100100000",
        "1001010111001010",
        "1111011100001011",
        "1001010111001010",
        "1111010000010011",
        "1001010000001100",
        "0000000100100000",
        "0011000010110001",
        "1111011011011100",
        "0011000011000001",
        "1111010000010100",
        "1001010000001100",
        "0000000100100000",
        "1001010011111000",
        "1111001010101111",
        "1001010000001110",
        "0000000110001010",
        "1111000000010111",
        "1001010000001100",
        "0000000100100000",
        "1111011001111111",
        "1001010011111000",
        "1111010000010111",
        "1001010000001100",
        "0000000100100000",
        "1111101111100001",
        "1111011001001110",
        "1111101111100011",
        "1111010000010110",
        "1001010000001100",
        "0000000100100000",
        "1111001000100110",
        "1111101111100001",
        "1111000000010110",
        "1001010000001100",
        "0000000100100000",
        "0000111111101110",
        "1111010000010000",
        "1001010000001100",
        "0000000100100000",
        "0000111111101110",
        "1111011111110000",
        "1111000000010101",
        "1001010000001100",
        "0000000100100000",
        "0010111101100111",
        "0001001101100111",
        "1100111111111101",
        "0001001101100111",
        "1001010000001100",
        "0000000101101001",
        "0001001101101000",
        "1110100001100000",
        "1001010011101000",
        "1001010010111000",
        "1001010011111000",
        "1001010000001000",
        "1001010001011000",
        "1001010010011000",
        "1111110101100110",
        "1110111101101111",
        "1111110101100011",
        "1001010000001100",
        "0000000101101001",
        "1111110101100111",
        "1110101001100101",
        "1111111101100000",
        "1110000001100000",
        "1111111101100101",
        "1001010000001100",
        "0000000101101001",
        "1111111101100001",
        "1001010000001100",
        "0000000000000000",
        "1110111110111111",
        "1110011111001111",
        "1110000011010000",
        "1001010100001000",
        "1110111110011111",
        "1110011110101111",
        "1110011011100110",
        "1001010100011000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000",
        "0000000000000000"
    );

begin

    -- the address has changed - read the new value
    ProgDB <= ROMbits(CONV_INTEGER(ProgAB)) when (CONV_INTEGER(ProgAB) <= ROMbits'high)  else
              X"DEAD";


    -- process to handle Reset
    process(Reset)
    begin

        -- check if Reset is low now
        if  (Reset = '0')  then
            -- reset is active - initialize the ROM (nothing for now)
        end if;

    end process;

end  ROM;