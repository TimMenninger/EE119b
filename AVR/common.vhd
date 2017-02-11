-----------------------------------------------------------------------------------------
--
--  AVR common package
--
--  This package defines constants and subtypes used across many or all of
--  the components in the AVR.
--
--  Revision History
--      10 Feb 17   Tim Menninger   Created
--
-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package common is

    -------------------------------------------------------------------------------------
    --
    -- Data and addressing
    --
    -------------------------------------------------------------------------------------

    -- Addresses
    subtype address_t       is std_logic_vector(15 downto 0);

    -- The offset of the address when accessing memory
    subtype addrOffset_t    is std_logic_vector( 5 downto 0);

    -- Data length
    subtype data_t          is std_logic_vector( 7 downto 0);
    subtype dataWord_t      is std_logic_vector(15 downto 0);

    -------------------------------------------------------------------------------------
    --
    -- Registers
    --
    -------------------------------------------------------------------------------------

    -- Length of instruction register
    subtype instruction_t   is std_logic_vector(15 downto 0);

    -- Length of the status register
    subtype status_t        is std_logic_vector( 7 downto 0);

    -------------------------------------------------------------------------------------
    --
    -- Select signals
    --
    -------------------------------------------------------------------------------------

    -- The size of the clock index.  We make a subtype for this because if
    -- the maximum number of clocks ever changes, it is less bug prone to
    -- update here only
    subtype clockIndex_t    is natural range 0 to 3;

    -- The width of the selector type to the ALU.  This tells the ALU
    -- which of its computations should be put onto the result
    subtype ALUSelector_t   is std_logic_vector( 1 downto 0);

    -- Selects a register from the register array
    subtype regSelector_t   is std_logic_vector( 4 downto 0);

    -- Selects a word register.  There are X, Y and Z word registers
    subtype wordSelector_t  is std_logic_vector( 2 downto 0);

    -- Selects which input to pay attention to on the registers
    subtype regInSelector_t is std_logic_vector( 1 downto 0);

    -- Selects which address to pay attention to on the memory addressing unit when
    -- accessing memory
    subtype addrSelector_t  is std_logic_vector( 1 downto 0);

    -- Selects a bit in the flag
    subtype flagSelector_t  is std_logic_vector( 2 downto 0);

    -------------------------------------------------------------------------------------
    --
    -- Constants
    --
    -------------------------------------------------------------------------------------

    -- Number of bits on the data bus.
    constant dataBits   : natural       := 8;

    -- Number of bits on address bus
    constant addrBits   : natural       := 16;


end package;
