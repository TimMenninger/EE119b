----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the AVR
--  system.  It uses entities for individual components, but tests the
--  entire system here at once.  Tests use vectors from files.
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--      1 Feb 17  Tim Menninger     Filled in structure
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

entity TEST_SYSTEM is
end TEST_SYSTEM;

architecture testbench of TEST_SYSTEM is

    -- Independent component that tests ALU
    component TEST_ALU is

        port (
            IR        :  in  std_logic_vector(15 downto 0);     -- Instruction Register
            OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
            OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
            clk       :  in  std_logic;                         -- system clock
            Result    :  out std_logic_vector(7 downto 0);      -- ALU result
            StatReg   :  out std_logic_vector(7 downto 0)       -- status register
        );

    end component;

    -- Independent component that tests registers
    component TEST_REG is

        port(
            IR        :  in  std_logic_vector(15 downto 0);     -- Instruction Register
            RegIn     :  in  std_logic_vector(7 downto 0);       -- input register bus
            clk       :  in  std_logic;                          -- system clock
            RegAOut   :  out std_logic_vector(7 downto 0);       -- register bus A out
            RegBOut   :  out std_logic_vector(7 downto 0)        -- register bus B out
        );

    end component;

    -- Test case files
    file ALU_vectors: text;
    file REG_vectors: text;

    -- All the variables we need
    signal clk          : std_logic := '0';
    signal ALU_inst     : std_logic_vector(15 downto 0) := "0000000000000000";
    signal REG_inst     : std_logic_vector(15 downto 0) := "0000000000000000";
    signal opA          : std_logic_vector(7 downto 0)  := "00000000";
    signal opB          : std_logic_vector(7 downto 0)  := "00000000";
    signal result       : std_logic_vector(7 downto 0)  := "00000000";
    signal status       : std_logic_vector(7 downto 0)  := "00000000";
    signal regIn        : std_logic_vector(7 downto 0)  := "00000000";
    signal regAOut      : std_logic_vector(7 downto 0)  := "00000000";
    signal regBOut      : std_logic_vector(7 downto 0)  := "00000000";

    -- Signifies end of simulation
    signal END_ALU      : boolean := FALSE;
    signal END_REG      : boolean := FALSE;

begin

    ALU_UUT : TEST_ALU
        port map (ALU_inst, opA, opB, clk, result, status);

    REG_UUT : TEST_REG
        port map (REG_inst, regIn, clk, regAOut, regBOut);

    DO_ALU_TEST: process
        -- Variables for reading ALU test file
        variable currLine       : line;
        variable instruction    : std_logic_vector(15 downto 0);
        variable operandA       : std_logic_vector(7 downto 0);
        variable operandB       : std_logic_vector(7 downto 0);
        variable expResult      : std_logic_vector(7 downto 0);
        variable expStatus      : std_logic_vector(7 downto 0);
        variable delimiter      : character;
    begin
        -- Open the testcase file
        file_open(ALU_vectors, "testcases/ALU_vectors.txt", read_mode);

        -- Wait a few clocks
        wait for 200 ns;

        -- First line is column headers
        readline(ALU_vectors, currLine);

        -- Go trough every test case
        while not endfile(ALU_vectors) loop
            -- Parse the line
            readline(ALU_vectors, currLine);
            read(currLine, instruction);
            read(currLine, delimiter);
            read(currLine, operandA);
            read(currLine, delimiter);
            read(currLine, operandB);
            read(currLine, delimiter);
            read(currLine, expResult);
            read(currLine, delimiter);
            read(currLine, expStatus);

            -- Instruction comes in short after clock rising edge
            wait for 5 ns;
            REG_inst <= instruction;
            opA <= operandA;
            opB <= operandB;

            -- Allow time for computation then check output and simulate result writeback
            wait for 40 ns;
            assert (std_match(result, expResult))
                report  "incorrect ALU result"
                severity  ERROR;
            assert (std_match(status, expStatus))
                report  "incorrect ALU status output"
                severity  ERROR;

            -- Finish clock cycle then repeat
            wait for 5 ns;
        end loop;

        -- Done simulation
        END_ALU <= TRUE;
        file_close(ALU_vectors);
    end process;

    DO_REG_TEST: process
        -- Variables for reading register test file
        variable currLine       : line;
        variable instruction    : std_logic_vector(15 downto 0);
        variable nextRegIn      : std_logic_vector(7 downto 0);
        variable expRegAOut     : std_logic_vector(7 downto 0);
        variable expRegBOut     : std_logic_vector(7 downto 0);
        variable delimiter      : character;
    begin
        -- Open the testcase file
        file_open(REG_vectors, "testcases/REG_vectors.txt", read_mode);

        -- Wait a few clocks
        wait for 200 ns;

        -- Go trough every test case
        while not endfile(REG_vectors) loop
            -- Parse the line
            readline(REG_vectors, currLine);
            read(currLine, instruction);
            read(currLine, delimiter);
            read(currLine, nextRegIn);
            read(currLine, delimiter);
            read(currLine, expRegAOut);
            read(currLine, delimiter);
            read(currLine, expRegBOut);

            -- Instruction comes in short after clock rising edge
            wait for 5 ns;
            REG_inst <= instruction;

            -- Allow time for computation then check output and simulate result writeback
            wait for 40 ns;
            assert (std_match(regAOut, expRegAOut))
                report  "incorrect register A output"
                severity  ERROR;
            assert (std_match(regBOut, expRegBOut))
                report  "incorrect register B output"
                severity  ERROR;
            regIn <= nextRegIn;

            -- Finish clock cycle then repeat
            wait for 5 ns;
        end loop;

        -- Done simulation
        END_REG <= TRUE;
        file_close(REG_vectors);
    end process;

    -- this process generates a 50 ns period, 50% duty cycle clock
    CLOCK_CLK : process
    begin
        -- only generate clock if still simulating
        if END_ALU = FALSE or END_REG = FALSE then
            clk <= '1';
            wait for 25 ns;
        else
            wait;
        end if;

        if END_ALU = FALSE or END_REG = FALSE then
            clk <= '0';
            wait for 25 ns;
        else
            wait;
        end if;
    end process;

end architecture;
