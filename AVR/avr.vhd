-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-----------------------------------------------------------------------------------------
--
-- avr.vhd
--
-- The system architecture for the Atmel AVR emulator.  It connects all of the
-- subcomponents.
--
-- Inputs:
--      sysclk : std_logic
--          The global clock
--      reset : std_logic
--          Active low global reset signal
--
-- Outputs:
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity AVR
--
-- Defines the inputs and outputs for the Atmel AVR system
--
entity AVR is
    port (
        sysclk      : in  std_logic;            -- system clock
        reset       : in  std_logic             -- system reset
    );
end AVR;
