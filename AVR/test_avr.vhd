----------------------------------------------------------------------------
--
--  This is the entity that actually tests the AVR
--
--  Revision History:
--     23 Feb 17  Tim Menninger     Created
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

library common;
use common.common.all;

entity AVR_TESTBENCH is
end AVR_TESTBENCH;

architecture testbench of AVR_TESTBENCH is

    -- Independent component that tests registers
    component AVR_CPU is
        port (
            ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
            Reset   :  in     std_logic;                       -- reset signal
            INT0    :  in     std_logic;                       -- interrupt signal
            INT1    :  in     std_logic;                       -- interrupt signal
            clock   :  in     std_logic;                       -- system clock
            ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
            DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
            DataWr  :  out    std_logic;                       -- data memory write en
            DataRd  :  out    std_logic;                       -- data memory read en
            DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
        );
    end component;

    -- This is data memory, initialized to unknown values, that is accessed by the CPU
    component DATA_MEMORY is
        port (
            RE      : in     std_logic;             	       -- read enable (active low)
            WE      : in     std_logic;		                   -- write enable (active low)
            DataAB  : in     std_logic_vector(15 downto 0);    -- memory address bus
            DataDB  : inout  std_logic_vector(7 downto 0)      -- memory data bus
        );
    end component;

    -- This is program memory, which contains the program
    component PROG_MEMORY is
        port (
            ProgAB  :  in   std_logic_vector(15 downto 0);  -- program address bus
            Reset   :  in   std_logic;                      -- system reset
            ProgDB  :  out  std_logic_vector(15 downto 0)   -- program data bus
        );
    end component;

    -- Test case files
    file AVR_vectors: text;

    -- Input and output signals
    signal clock        : std_logic := '0'; -- System clock
    signal reset        : std_logic := '1'; -- System reset (active low)
    signal ProgDB       : address_t := "0000000000000000"; -- Program data bus
    signal ProgAB       : address_t := "0000000000000000"; -- Program address bus
    signal DataDB       : data_t    := "00000000"; -- Data bus to data memory
    signal DataAB       : address_t := "0000000000000000"; -- Address bus to data memory
    signal DataRd       : std_logic := '1'; -- Read signal to data memory (active low)
    signal DataWr       : std_logic := '1'; -- Write signal to data memory (active low)

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    AVR_UUT     : AVR_CPU
        port map (ProgDB, reset, '1', '1', clock, ProgAB, DataAB, DataWr, DataRd, DataDB);

    DataMemory  : DATA_MEMORY
        port map (DataRd, DataWr, DataAB, DataDB);

    ProgMemory  : PROG_MEMORY
        port map (ProgAB, reset, ProgDB);

    process
        -- Variables for reading register test file
        variable currLine       : line;
        variable delimiter      : character;
        variable instruction    : string (1 to 5);
        variable expRd          : std_logic;
        variable expWr          : std_logic;
        variable expDataDB      : data_t;
        variable expDataAB      : address_t;
        variable expProgAB      : address_t;

    begin
        -- Wait a few clocks with reset active
        Reset <= '0';
        wait for 200 ns;
        Reset <= '1';

        -- Open the testcase file
        file_open(AVR_vectors, "testcases/AVR_vectors.txt", read_mode);

        -- Go trough every test case
        while not endfile(AVR_vectors) loop
            -- Parse the line
            readline(AVR_vectors, currLine);
            read(currLine, instruction);
            read(currLine, delimiter);
            read(currLine, expRd);
            read(currLine, expWr);
            read(currLine, delimiter);
            read(currLine, expDataDB);
            read(currLine, delimiter);
            read(currLine, expDataAB);
            read(currLine, delimiter);
            read(currLine, expProgAB);

            -- Instruction comes in short after clock rising edge
            wait for 5 ns;

            -- Check that we are looking in the right place for the instruction
            assert (std_match(ProgAB, expProgAB))
                report  instruction & " program address bus incorrect"
                severity  ERROR;

            -- Make sure signals inactive
            assert (std_match(DataRd, '1'))
                report  instruction & " read signal incorrect"
                severity  ERROR;
            assert (std_match(DataWr, '1'))
                report  instruction & " write signal incorrect"
                severity  ERROR;

            -- Wait until end of clock
            wait for 40 ns;

            -- At the end of the cycle, when clock is low, check the outputs
            assert (std_match(DataRd, expRd))
                report  instruction & " read signal incorrect"
                severity  ERROR;
            assert (std_match(DataWr, expWr))
                report  instruction & " write signal incorrect"
                severity  ERROR;
            assert (std_match(DataDB, expDataDB))
                report  instruction & " data bus incorrect"
                severity  ERROR;
            assert (std_match(DataAB, expDataAB))
                report  instruction & " data address bus incorrect"
                severity  ERROR;

            wait for 5 ns;
        end loop;
        file_close(AVR_vectors);

        -- Done simulation
        END_SIM <= TRUE;
        wait;

    end process;

    -- this process generates a 50 ns period, 50% duty cycle clock
    CLOCK_CLK : process
    begin
        -- only generate clock if still simulating
        if END_SIM = FALSE then
            clock <= '1';
            wait for 25 ns;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            clock <= '0';
            wait for 25 ns;
        else
            wait;
        end if;
    end process;

end architecture;
