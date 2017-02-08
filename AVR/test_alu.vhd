----------------------------------------------------------------------------
--
--  Atmel AVR ALU Test Entity Declaration
--
--  This is the entity declaration which must be used for building the ALU
--  portion of the AVR design for testing.  It is to be used by a top-level
--  test (this does no testing itself).
--
--  Revision History:
--     17 Apr 98  Glen George       Initial revision.
--     20 Apr 98  Glen George       Fixed minor syntax bugs.
--     18 Apr 04  Glen George       Updated comments and formatting.
--     21 Jan 06  Glen George       Updated comments.
--      1 Feb 17  Tim Menninger     Filled in structure
--
----------------------------------------------------------------------------


--
--  TEST_ALU
--
--  This is the ALU testing interface.  It just brings all the important
--  ALU signals out for testing along with the Instruction Register.
--
--  Inputs:
--    IR       - Instruction Register (16 bits)
--    OperandA - first operand to ALU (8 bits) - looks like the output
--               of the register array
--    OperandB - second operand to ALU (8 bits) - looks like the output
--               of the register array
--    clk    - the system clk
--
--  Outputs:
--    Result   - result of the ALU operation selected by the Instruction
--               Register (8 bits)
--    StatReg  - Status Register contents (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

-- use work.opcodes.all;


entity  TEST_ALU  is

    port (
        IR        :  in  std_logic_vector(15 downto 0);     -- Instruction Register
        OperandA  :  in  std_logic_vector(7 downto 0);      -- first operand
        OperandB  :  in  std_logic_vector(7 downto 0);      -- second operand
        clk       :  in  std_logic;                         -- system clk
        Result    :  out std_logic_vector(7 downto 0);      -- ALU result
        StatReg   :  out std_logic_vector(7 downto 0)       -- status register
    );

end  TEST_ALU;

architecture toplevel of TEST_ALU is

    -- ALU component we are testing
    component ALU is
        port (
            opA         : in  std_logic_vector(7 downto 0); -- operand 1
            opB         : in  std_logic_vector(7 downto 0); -- operand 2
            immed       : in  std_logic_vector(7 downto 0); -- immediate value
            SREG        : in  std_logic_vector(7 downto 0); -- flags

            ENALU       : in  std_logic_vector(1 downto 0); -- operation type
            ENCarry     : in  std_logic;                    -- opcode uses carry
            ENImmed     : in  std_logic;                    -- opcode uses immed
            ENInvOp     : in  std_logic;                    -- negate operand
            ENInvRes    : in  std_logic;                    -- negate result

            ENMul       : in  std_logic;                    -- active (low) when MUL
            clkIdx      : in  natural range 0 to 3;         -- num clks since instrctn

            Rd0         : out std_logic;                    -- bit 0 of operand A
            Rd3         : out std_logic;                    -- bit 3 of operand A
            Rr3         : out std_logic;                    -- bit 3 of operand B
            Rd7         : out std_logic;                    -- bit 7 of operand A
            Rr7         : out std_logic;                    -- bit 7 of operand B

            result      : out std_logic_vector(7 downto 0)  -- computed result
        );
    end component;

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
            ENMul       : in  std_logic;                    -- write to registers 0 and 1
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

    -- The status register is updated by the ALU
    component Status is
        port (
            clk         : in  std_logic;                            -- system clk

            R           : in  std_logic_vector(7 downto 0);         -- result from ALU
            Rd0         : in  std_logic;                            -- bit 0 of operand A
            Rd3         : in  std_logic;                            -- bit 3 of operand A
            Rr3         : in  std_logic;                            -- bit 3 of operand B
            Rd7         : in  std_logic;                            -- bit 7 of operand A
            Rr7         : in  std_logic;                            -- bit 7 of operand B
            Rdb         : in  std_logic;                            -- Bit to set T to

            BST         : in  std_logic;                            -- '1' when in BST
            sel         : in  std_logic_vector(2 downto 0);         -- selects flag index
            mask        : in  std_logic_vector(7 downto 0);         -- masks unaffected flags
            clkIdx      : in  natural range 0 to 3;                 -- clks since instrctn
            ENRes       : in  std_logic;                            -- set SREG to R

            TF          : out std_logic;                            -- always sent to regs
            SREG        : out std_logic_vector(7 downto 0)          -- status register
        );
    end component;

    -- Control unit which is needed to get from instruction to ALU out
    component ControlUnit is
        port (
            clk         : in  std_logic;                    -- system clk
            reset       : in  std_logic;                    -- system reset

            instruction : in  std_logic_vector(15 downto 0);-- instruction

            BLD         : out std_logic;                    -- '1' when BLD
            BST         : out std_logic;                    -- '1' when BST

            sel         : out std_logic_vector(2 downto 0); -- selects flag index
            flagMask    : out std_logic_vector(7 downto 0); -- status bits affected
            clkIdx      : out natural range 0 to 3;         -- clocks since instruction
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

    -- All the variables we need
    signal reset    : std_logic                     := '1';

    signal immed    : std_logic_vector(7 downto 0)  := "00000000";
    signal SREG     : std_logic_vector(7 downto 0)  := "00000000";

    signal ENALU    : std_logic_vector(1 downto 0)  := "00";
    signal ENCarry  : std_logic                     := '0';
    signal ENImmed  : std_logic                     := '0';
    signal ENInvOp  : std_logic                     := '0';
    signal ENInvRes : std_logic                     := '0';

    signal Rd0      : std_logic                     := '0';
    signal Rd3      : std_logic                     := '0';
    signal Rr3      : std_logic                     := '0';
    signal Rd7      : std_logic                     := '0';
    signal Rr7      : std_logic                     := '0';

    signal R        : std_logic_vector(7 downto 0)  := "00000000";
    signal Rdb      : std_logic                     := '0';

    signal sel      : std_logic_vector(2 downto 0)  := "000";
    signal flagMask : std_logic_vector(7 downto 0)  := "00000000";
    signal ENRes    : std_logic                     := '0';

    signal TF       : std_logic                     := '0';

    signal BLD      : std_logic                     := '0';
    signal BST      : std_logic                     := '0';
    signal clkIdx   : natural range 0 to 3          := 0;

    signal regSelA  : std_logic_vector(4 downto 0)  := "00000";
    signal regSelB  : std_logic_vector(4 downto 0)  := "00000";
    signal ENMul    : std_logic                     := '0';
    signal ENSwap   : std_logic                     := '0';
    signal ENRegA   : std_logic                     := '0';
    signal ENRegB   : std_logic                     := '0';
    signal ENRegRd  : std_logic                     := '0';
    signal ENRegWr  : std_logic                     := '0';

    signal dataOutA : std_logic_vector(7 downto 0)  := "00000000";
    signal dataOutB : std_logic_vector(7 downto 0)  := "00000000";

begin

    -- Output the status signal being passed around
    StatReg <= SREG;

    -- Output the result signal being passed around
    Result <= R;

    ALUUUT : ALU
        port map (
            OperandA,
            OperandB,
            immed,
            SREG,

            ENALU,
            ENCarry,
            ENImmed,
            ENInvOp,
            ENInvRes,

            ENMul,
            clkIdx,

            Rd0,
            Rd3,
            Rr3,
            Rd7,
            Rr7,

            R
        );

    RegistersUUT : Registers
        port map (
            clk,
            clkIdx,
            R,

            BLD,
            sel,
            TF,

            regSelA,
            regSelB,
            ENMul,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegRd,
            ENRegWr,

            Rdb,
            dataOutA,
            dataOutB
        );

    StatusUUT : Status
        port map (
            clk,

            R,
            Rd0,
            Rd3,
            Rr3,
            Rd7,
            Rr7,
            Rdb,

            BST,
            sel,
            flagMask,
            clkIdx,
            ENRes,

            TF,
            SREG
        );

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

end architecture;

----------------------------------------------------------------------------
--
--  This is the entity that actually tests the ALU.
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

entity ALU_TESTBENCH is
end ALU_TESTBENCH;

architecture testbench of ALU_TESTBENCH is

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

    -- Test case files
    file ALU_vectors: text;

    -- All the variables we need
    signal clk          : std_logic := '0';
    signal IR           : std_logic_vector(15 downto 0) := "0000000000000000";
    signal opA          : std_logic_vector(7 downto 0)  := "00000000";
    signal opB          : std_logic_vector(7 downto 0)  := "00000000";
    signal result       : std_logic_vector(7 downto 0)  := "00000000";
    signal status       : std_logic_vector(7 downto 0)  := "00000000";

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    ALU_UUT : TEST_ALU
        port map (IR, opA, opB, clk, result, status);

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
            IR <= instruction;
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
        file_close(ALU_vectors);

        -- Done simulation
        END_SIM <= TRUE;
        wait;

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
