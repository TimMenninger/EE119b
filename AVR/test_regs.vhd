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
--     01 Feb 17  Tim Menninger     Filled in structure to test registers
--     09 Feb 17  Tim Menninger     Updated with memory implementation
--
----------------------------------------------------------------------------


--
--  REG_TEST
--
--  This is the register array testing interface.  It just brings all the
--  important register array signals out for testing along with the
--  Instruction Register.
--
--  Inputs:
--    IR      - Instruction Register (16 bits)
--    RegIn   - input to the register array (8 bits)
--    clock     - the system clock
--
--  Outputs:
--    RegAOut - register bus A output (8 bits), eventually will connect to ALU
--    RegBOut - register bus B output (8 bits), eventually will connect to ALU
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library common;
use common.common.all;


entity  REG_TEST  is
    port(
        IR       :  in  opcode_word;                        -- Instruction Register
        RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
        clock    :  in  std_logic;                          -- system clock
        RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
        RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
    );
end  REG_TEST;

architecture toplevel of REG_TEST is

    -- ALU component we are testing
    component ALU is
        port (
            opA         : in  data_t;           -- operand 1
            opB         : in  data_t;           -- operand 2
            immed       : in  data_t;           -- immediate value
            SREG        : in  status_t;         -- flags

            ENALU       : in  ALUSelector_t;    -- operation type
            ENCarry     : in  std_logic;        -- opcode uses carry
            ENImmed     : in  std_logic;        -- opcode uses immed
            ENInvOp     : in  std_logic;        -- negate operand
            ENInvRes    : in  std_logic;        -- negate result

            ENMul       : in  std_logic;        -- active (low) when MUL
            clkIdx      : in  clockIndex_t;     -- num clks since instrctn

            Rd0         : out std_logic;        -- bit 0 of operand A
            Rd3         : out std_logic;        -- bit 3 of operand A
            Rr3         : out std_logic;        -- bit 3 of operand B
            Rd7         : out std_logic;        -- bit 7 of operand A
            Rr7         : out std_logic;        -- bit 7 of operand B

            result      : out data_t            -- computed result
        );
    end component;

    component Registers is
        port (
            clk         : in  std_logic;        -- system clock
            clkIdx      : in  clockIndex_t;     -- number of clocks since instr

            ALUIn       : in  data_t;           -- data input from ALU
            memIn       : in  data_t;           -- data input from memory
            immedIn     : in  data_t;           -- immediate value from instr
            sourceSel   : in  regInSelector_t;  -- used to choose data source

            wordRegIn   : in  dataWord_t;       -- new value for word register
            wordRegSel  : in  wordSelector_t;   -- selects which word register

            BLD         : in  std_logic;        -- true when BLD occurring
            sel         : in  flagSelector_t;   -- bit select for BLD
            T           : in  std_logic;        -- T flag

            regSelA     : in  regSelector_t;    -- register select
            regSelB     : in  regSelector_t;    -- register select
            ENMul       : in  std_logic;        -- write to registers 0 and 1
            ENSwap      : in  std_logic;        -- swap nibbles
            ENRegA      : in  std_logic;        -- active low enable reg A
            ENRegB      : in  std_logic;        -- active low enable reg B
            ENWrite     : in  std_logic;        -- active low enable write

            Rdb         : out std_logic;        -- b'th bit of reg A
            dataOutA    : out data_t;           -- low byte of output
            dataOutB    : out data_t;           -- high byte of output
            wordRegOut  : out dataWord_t        -- word register output
        );
    end component;

    -- The status register is updated by the ALU
    component Status is
        port (
            clk         : in  std_logic;        -- system clock

            R           : in  data_t;           -- result from ALU
            Rd0         : in  std_logic;        -- bit 0 of operand A
            Rd3         : in  std_logic;        -- bit 3 of operand A
            Rr3         : in  std_logic;        -- bit 3 of operand B
            Rd7         : in  std_logic;        -- bit 7 of operand A
            Rr7         : in  std_logic;        -- bit 7 of operand B
            Rdb         : in  std_logic;        -- Bit to set T to

            BST         : in  std_logic;        -- '1' when in BST
            CPC         : in  std_logic;        -- '1' when CPC
            sel         : in  flagSelector_t;   -- selects flag index
            mask        : in  status_t;         -- masks unaffected flags
            clkIdx      : in  clockIndex_t;     -- clks since instrctn
            ENRes       : in  std_logic;        -- set SREG to R

            Eq          : out std_logic;        -- '0' when regA = regB
            SREG        : out status_t          -- status register
        );
    end component;

    -- Control unit which is needed to get from instruction to ALU out
    component ControlUnit is
        port (
            clk         : in  std_logic;        -- system clock

            instruction : in  instruction_t;    -- instruction
            status      : in  status_t;         -- the flags

            Rdb         : in  std_logic;        -- the b'th bit of register regSelA
            Eq          : in  std_logic;        -- '1' when reg A = reg B

            BLD         : out std_logic;        -- '1' when BLD
            BST         : out std_logic;        -- '1' when BST
            CPC         : out std_logic;        -- '1' when CPC

            sel         : out flagSelector_t;   -- selects flag index
            flagMask    : out status_t;         -- status bits affected
            clkIdx      : out clockIndex_t;     -- clocks since instruction
            ENRes       : out std_logic;        -- set SREG to R

            immed       : out immediate_t;      -- immediate value
            ENALU       : out ALUSelector_t;    -- ALU operation type
            ENImmed     : out std_logic;        -- enable immed
            ENCarry     : out std_logic;        -- enable carry
            ENInvOp     : out std_logic;        -- negate operand in ALU
            ENInvRes    : out std_logic;        -- negate result in ALU

            regSelA     : out regSelector_t;    -- register A select
            regSelB     : out regSelector_t;    -- register B select
            ENMul       : out std_logic;        -- write to registers 0 and 1
            ENSwap      : out std_logic;        -- SWAP instruction
            ENRegA      : out std_logic;        -- enable register A
            ENRegB      : out std_logic;        -- enable register B
            ENRegWr     : out std_logic;        -- enable register writing
            sourceSel   : out regInSelector_t;  -- used to choose data input
            wordReg     : out wordSelector_t;   -- used to choose X Y Z regs

            -- Data memory control
            memRW       : out std_logic;        -- read/write to memory
            addrSel     : out addrSelector_t;   -- for address mux
            addBefore   : out std_logic;        -- dictates when to add to addr
            decrement   : out std_logic;        -- when low, decrementing

            -- Stack pointer control
            SPWr        : out std_logic;        -- write to stack ptr

            -- Instruction pointer control
            fetch       : out std_logic         -- Tells us when to fetch instruction
        );
    end component;

    -- Signals required for passing around
    signal reset        : std_logic         := '1';

    signal BLD          : std_logic         := '0';
    signal BST          : std_logic         := '0';
    signal CPC          : std_logic         := '0';

    signal memIn        : data_t            := "00000000";
    signal sourceSel    : regInSelector_t   := "00";

    signal wordRegIn    : dataWord_t        := "0000000000000000";
    signal wordRegSel   : wordSelector_t    := "000";

    signal sel          : flagSelector_t    := "000";
    signal flagMask     : status_t          := "00000000";
    signal ENRes        : std_logic         := '0';

    signal immed        : immediate_t       := "000000000000";
    signal ENALU        : ALUSelector_t     := "00";
    signal ENImmed      : std_logic         := '0';
    signal ENCarry      : std_logic         := '0';
    signal ENInvOp      : std_logic         := '0';
    signal ENInvRes     : std_logic         := '0';

    signal Rd0          : std_logic         := '0';
    signal Rd3          : std_logic         := '0';
    signal Rr3          : std_logic         := '0';
    signal Rd7          : std_logic         := '0';
    signal Rr7          : std_logic         := '0';

    signal R            : data_t            := "00000000";
    signal Rdb          : std_logic         := '0';

    signal regSelA      : regSelector_t     := "00000";
    signal regSelB      : regSelector_t     := "00000";
    signal ENMul        : std_logic         := '0';
    signal ENSwap       : std_logic         := '0';
    signal ENRegA       : std_logic         := '0';
    signal ENRegB       : std_logic         := '0';
    signal ENRegWr      : std_logic         := '0';

    signal clkIdx       : clockIndex_t      := 0;

    signal SREG         : status_t          := "00000000";

    signal dataOutA     : data_t            := "00000000";
    signal dataOutB     : data_t            := "00000000";
    signal wordRegOut   : dataWord_t        := "0000000000000000";

    -- Data memory control
    signal memRW        : std_logic         := '0';
    signal addrSel      : addrSelector_t    := "00";
    signal addBefore    : std_logic         := '0';
    signal decrement    : std_logic         := '0';

    -- Stack pointer control
    signal SPWr         : std_logic         := '0';

begin

    ControlUUT : ControlUnit
        port map (
            clock,

            IR,
            SREG,

            Rdb,
            '0',

            BLD,
            BST,
            CPC,

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
            ENRegWr,
            sourceSel,
            wordRegSel,

            memRW,
            addrSel,
            addBefore,
            decrement,

            SPWr,

            open
        );

    RegisterUUT : Registers
        port map (
            clock,      -- Test entity input
            clkIdx,

            RegIn,
            memIn,
            immed(7 downto 0),
            sourceSel,
            wordRegIn,
            wordRegSel,

            BLD,
            sel,
            SREG(6),

            regSelA,
            regSelB,
            ENMul,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegWr,

            Rdb,
            RegAOut,
            RegBOut,
            wordRegOut
        );

    StatusUUT : Status
        port map (
            clock,

            R,
            Rd0,
            Rd3,
            Rr3,
            Rd7,
            Rr7,
            Rdb,

            BST,
            CPC,
            sel,
            flagMask,
            clkIdx,
            ENRes,

            open,
            SREG
        );

    ALUUUT : ALU
        port map (
            dataOutA,
            dataOutB,
            immed(7 downto 0),
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

library common;
use common.common.all;

entity REG_TESTBENCH is
end REG_TESTBENCH;

architecture testbench of REG_TESTBENCH is

    -- Independent component that tests registers
    component REG_TEST is

        port(
            IR       :  in  opcode_word;                        -- Instruction Register
            RegIn    :  in  std_logic_vector(7 downto 0);       -- input register bus
            clock    :  in  std_logic;                          -- system clock
            RegAOut  :  out std_logic_vector(7 downto 0);       -- register bus A out
            RegBOut  :  out std_logic_vector(7 downto 0)        -- register bus B out
        );

    end component;

    -- Test case files
    file REG_vectors: text;

    -- All the variables we need
    signal clock        : std_logic     := '0';
    signal IR           : instruction_t := "0000000000000000";
    signal regIn        : data_t        := "00000000";
    signal regAOut      : data_t        := "00000000";
    signal regBOut      : data_t        := "00000000";

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    REG_UUT : REG_TEST
        port map (IR, regIn, clock, regAOut, regBOut);

    process
        -- Variables for reading register test file
        variable currLine       : line;
        variable instruction    : instruction_t;
        variable nextRegIn      : data_t;
        variable expRegAOut     : data_t;
        variable expRegBOut     : data_t;
        variable delimiter      : character;
    begin
        -- Open the testcase file
        file_open(REG_vectors, "testcases/REG_vectors.txt", read_mode);

        -- Skip first line
        readline(REG_vectors, currLine);

        -- Go trough every test case
        while not endfile(REG_vectors) loop
            readline(REG_vectors, currLine);

            -- Parse the line
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
