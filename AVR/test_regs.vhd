----------------------------------------------------------------------------
--
--  Atmel AVR Register Array Test Entity Declaration
--
--  This is the entity declaration which must be used for building the
--  register array portion of the AVR design for testing.  It is to be
--  used by a top-level test (this does no testing itself).
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     22 Apr 02  Glen George       Updated comments.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--      1 Feb 17  Tim Menninger     Filled in structure
--
----------------------------------------------------------------------------


--
--  TEST_REG
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clk     - the system clk
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;


entity  TEST_REG  is
    port(
        IR       :  in  std_logic_vector(15 downto 0);     -- Instruction Register
        RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
        clk      :  in  std_logic;                          -- system clk
        RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
        RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
    );
end  TEST_REG;

architecture toplevel of TEST_REG is

    -- Registers component we are testing
    component ControlUnit is
        port (
            clk         : in  std_logic;                    -- system clk
            reset       : in  std_logic;                    -- system reset

            instruction : in  std_logic_vector(15 downto 0);-- instruction

            BLD         : out std_logic;                    -- '1' when BLD
            BST         : out std_logic;                    -- '1' when BST

            sel         : out std_logic_vector(2 downto 0); -- selects flag index
            flagMask    : out std_logic_vector(7 downto 0); -- status bits affected
            clkIdx      : out natural range 0 to 3;         -- byte index of result
            ENRes       : out std_logic;                    -- set SREG to R

            immed       : out std_logic_vector(7 downto 0); -- immediate value
            ENALU       : out std_logic_vector(1 downto 0); -- ALU operation type
            ENImmed     : out std_logic;                    -- enable immed
            ENCarry     : out std_logic;                    -- enable carry
            ENInvOp     : out std_logic;                    -- negate operand in ALU
            ENInvRes    : out std_logic;                    -- negate result in ALU

            regSelA     : out std_logic_vector(4 downto 0); -- register A select
            regSelB     : out std_logic_vector(4 downto 0); -- register B select
            ENMul       : out std_logic;                    -- write to registers 0 and 1
            ENSwap      : out std_logic;                    -- SWAP instruction
            ENRegA      : out std_logic;                    -- enable register A
            ENRegB      : out std_logic;                    -- enable register B
            ENRegRd     : out std_logic;                    -- enable register reading
            ENRegWr     : out std_logic                     -- enable register writing
        );
    end component;

    -- Control unit which is needed to get from instruction to register out
    component Registers is
        port (
            clk         : in  std_logic;                    -- system clk
            clkIdx      : in  natural range 0 to 3;         -- number of clocks since instr
            dataIn      : in  std_logic_vector(7 downto 0); -- data input

            BLD         : in  std_logic;                    -- true when BLD occurring
            sel         : in  std_logic_vector(2 downto 0); -- bit select for BLD
            T           : in  std_logic;                    -- T flag

            regSelA     : in  std_logic_vector(4 downto 0); -- register select
            regSelB     : in  std_logic_vector(4 downto 0); -- register select
            ENMul         : in  std_logic;                    -- write to registers 0 and 1
            ENSwap      : in  std_logic;                    -- swap nibbles
            ENRegA      : in  std_logic;                    -- active low enable reg A
            ENRegB      : in  std_logic;                    -- active low enable reg B
            ENRead      : in  std_logic;                    -- active low enable read
            ENWrite     : in  std_logic;                    -- active low enable write

            Rdb         : out std_logic;                    -- b'th bit of reg A
            dataOutA    : out std_logic_vector(7 downto 0); -- low byte of output
            dataOutB    : out std_logic_vector(7 downto 0)  -- high byte of output
        );
    end component;

    -- Signals required for passing around
    signal reset       : std_logic := '1';

    signal BLD         : std_logic;
    signal BST         : std_logic;

    signal sel         : std_logic_vector(2 downto 0);
    signal flagMask    : std_logic_vector(7 downto 0);
    signal ENRes       : std_logic;

    signal immed       : std_logic_vector(7 downto 0);
    signal ENALU       : std_logic_vector(1 downto 0);
    signal ENImmed     : std_logic;
    signal ENCarry     : std_logic;
    signal ENInvOp     : std_logic;
    signal ENInvRes    : std_logic;

    signal regSelA     : std_logic_vector(4 downto 0);
    signal regSelB     : std_logic_vector(4 downto 0);
    signal ENMul       : std_logic;
    signal ENSwap      : std_logic;
    signal ENRegA      : std_logic;
    signal ENRegB      : std_logic;
    signal ENRegRd     : std_logic;
    signal ENRegWr     : std_logic;

    signal clkIdx      : natural range 0 to 3;

    signal T           : std_logic := '0';

    signal Rdb         : std_logic := '0';

begin

    ControlUUT : ControlUnit
        port map (
            clk,
            reset,

            IR,

            BLD,
            BST,

            sel,
            flagMask,
            clkIdx,
            ENRes,

            immed,
            ENALU,
            ENImmed,
            ENCarry,
            ENInvOp,
            ENInvRes,

            regSelA,
            regSelB,
            ENMul,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegRd,
            ENRegWr
        );

    RegisterUUT : Registers
        port map (
            clk,      -- Test entity input
            clkIdx,
            RegIn,

            BLD,
            sel,
            T,

            regSelA,
            regSelB,
            ENMul,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegRd,
            ENRegWr,

            Rdb,
            RegAOut,    -- Test entity output
            RegBOut     -- Test entity output
        );

end architecture;

----------------------------------------------------------------------------
--
--  This is the entity that actually tests the registers
--
--  Revision History:
--      1 Feb 17  Tim Menninger     Created
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

entity REGS_TESTBENCH is
end REGS_TESTBENCH;

architecture testbench of REGS_TESTBENCH is

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
    file REG_vectors: text;

    -- All the variables we need
    signal clk          : std_logic := '0';
    signal IR           : std_logic_vector(15 downto 0) := "0000000000000000";
    signal regIn        : std_logic_vector(7 downto 0)  := "00000000";
    signal regAOut      : std_logic_vector(7 downto 0)  := "00000000";
    signal regBOut      : std_logic_vector(7 downto 0)  := "00000000";

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    REG_UUT : TEST_REG
        port map (IR, regIn, clk, regAOut, regBOut);

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

        -- Skip first line
        readline(REG_vectors, currLine);

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
            IR <= instruction;

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
        file_close(REG_vectors);

        -- Done simulation
        END_SIM <= TRUE;
    end process;

    -- this process generates a 50 ns period, 50% duty cycle clock
    CLOCK_CLK : process
    begin
        -- only generate clock if still simulating
        if END_SIM = FALSE then
            clk <= '1';
            wait for 25 ns;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            clk <= '0';
            wait for 25 ns;
        else
            wait;
        end if;
    end process;

end architecture;