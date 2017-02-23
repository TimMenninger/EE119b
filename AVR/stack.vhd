-----------------------------------------------------------------------------------------
--
-- stack.vhd
--
-- This component is responsible for the stack pointer.  It continuously outputs the
-- stack pointer and when given the control signal to, updates the SP with the data
-- on the dataIn.
--
-- Inputs:
--      clk: std_logic;
--          System clock
--      reset: std_logic
--          Active low reset.  When active, this resets the stack pointer to all 1's
--      dataIn: address_t
--          The data input to overwrite the stack pointer.
--      ENWr: std_logic
--          Active low.  When low, the stack pointer will latch what is on the dataIn
--          line.
--
-- Outputs:
--      SP : address_t
--          The stack pointer.  This points to the first unused block of memory in
--          stack space.
--
-- Revision History:
--      08 Feb 17   Tim Menninger   Created
--
-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library common;
use common.common.all;

--
-- entity stack
--
-- Defines the status register
--
entity Stack is
    port (
        clk         : in  std_logic;        -- system clock
        reset       : in  std_logic;        -- active low reset

        dataIn      : in  address_t;        -- new stack pointer
        ENWr        : in  std_logic;        -- when low, look at data

        SP          : out address_t         -- stack pointer
    );
end Stack;

--
-- architecture that handles the stack pointer
--
architecture update of Stack is

    -- The register containing the stack pointer
    signal stackPointer : address_t := "1111111111111111";

begin

    -- Always output stack pointer
    SP <= stackPointer;

    -- Update stack pointer on clock edges and reset
    process (clk, reset) is
    begin
        -- Reset stack pointer on reset
        if (reset = '0') then
            stackPointer <= "1111111111111111";
        end if;

        -- Set it on rising edge of clock whe write signal received
        if (rising_edge(clk) and reset = '1' and ENWr = '0') then
            stackPointer <= dataIn;
        end if;
    end process;

end architecture;
