-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-----------------------------------------------------------------------------------------
--
-- memory.vhd
--
-- This contains logic to read and write to memory.  When RW indicates a read, data
-- is asserted on the output.  Otherwise, it is high impedance while data from the
-- input is written.  When enable is inactive, no read or write occurs and the output
-- is on high impedance.
--
-- Inputs:
--      sysclk : std_logic
--          The global clock
--      reset : std_logic
--          Active low global reset signal
--      addr : std_logic_vector(15 downto 0)
--          Address from which to read memory
--      offset : std_logic_vector(7 downto 0)
--           The offset to the address to read at
--      dataIn : std_logic_vector(7 downto 0)
--          Data to write to memory when applicable
--      RW : std_logic
--          Read/write signal high when reading, low when writing.  When writing,
--          outputs are at high impedance.
--      EN : std_logic
--          Active low signal.  When active, RW paid attention to.  When inactive,
--          nothing done and outputs at high impedance.
--
-- Outputs:
--      dataOut : std_logic_vector(7 downto 0)
--          Data output when reading from memory.  High impedance when not reading or
--          not enabled.
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity MemoryUnit
--
-- Defines the inputs and outputs for the memory unit of the Atmel AVR
-- emulator
--
entity MemoryUnit is
    port (
        sysclk      : in  std_logic;                    -- system clock
        reset       : in  std_logic;                    -- system reset

        addr        : in  std_logic_vector(15 downto 0);-- access address
        offset      : in  std_logic_vector(7 downto 0); -- address offset
        dataIn      : in  std_logic_vector(7 downto 0); -- data to write

        RW          : in  std_logic;                    -- read/write signal
        EN          : in  std_logic;                    -- enable signal

        dataOut     : out std_logic_vector(7 downto 0)  -- data output on reads
    );
end MemoryUnit;
