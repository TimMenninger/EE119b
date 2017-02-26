----------------------------------------------------------------------------
--
--  Atmel AVR Data Memory Test Entity Declaration
--
--  This is the entity declaration which must be used for building the data
--  memory access portion of the AVR design for testing.
--
--  Revision History:
--     24 Apr 98  Glen George       Initial revision.
--     25 Apr 00  Glen George       Fixed entity name and updated comments.
--      2 May 02  Glen George       Updated comments.
--      3 May 02  Glen George       Fixed Reset signal type.
--     23 Jan 06  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--     09 Feb 17  Tim Menninger     Implemented memory test
--
----------------------------------------------------------------------------


--
--  MEM_TEST
--
--  This is the data memory access testing interface.  It just brings all
--  the important data memory access signals out for testing along with the
--  Instruction Register and Program Data Bus.
--
--  Inputs:
--    IR     - Instruction Register (16 bits)
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    clock  - the system clock
--
--  Outputs:
--    DataAB - data memory address bus (16 bits)
--    DataDB - data memory data bus (8 bits)
--    DataRd - data read (active low)
--    DataWr - data write (active low)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library common;
use common.common.all;


entity  MEM_TEST  is

    port (
        IR      :  in     opcode_word;                      -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instruction
        Reset   :  in     std_logic;                        -- system reset signal (active low)
        clock   :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read (active low)
        DataWr  :  out    std_logic                         -- data write (active low)
    );

end  MEM_TEST;

architecture toplevel of MEM_TEST is

    -- Registers that we sometimes write to
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

    -- Control unit which is needed to get from instruction to ALU out
    component ControlUnit is
        port (
            clk         : in  std_logic;        -- system clock
            reset       : in  std_logic;        -- system reset

            instruction : in  address_t;        -- instruction
            status      : in  status_t;         -- the flags

            Eq          : in  std_logic;        -- '0' when reg A = reg B

            BLD         : out std_logic;        -- '1' when BLD
            BST         : out std_logic;        -- '1' when BST

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
            memEN       : out std_logic;        -- active low enable to memory
            addrSel     : out addrSelector_t;   -- for address mux
            addBefore   : out std_logic;        -- dictates when to add to addr
            decrement   : out std_logic;        -- when low, decrementing
            useIP       : out std_logic;        -- use IP for writing when '1'

            -- Stack pointer control
            SPWr        : out std_logic;        -- write to stack ptr

            -- Instruction pointer control
            fetch       : out std_logic         -- Tells us when to fetch instruction
        );
    end component;

    -- Memory unit
    component MemoryUnit is
        port (
            clk         : in  std_logic;        -- system clock

            regAddr     : in  address_t;        -- address from registers
            SPAddr      : in  address_t;        -- address from stack ptr
            IPAddr      : in  address_t;        -- address from instruction
            ProgAddr    : in  address_t;        -- address from program data bus
            immed       : in  immediate_t;      -- memory address offset
            decrement   : in  std_logic;        -- when low, decrement

            addrSel     : in  addrSelector_t;   -- chooses which address
            RW          : in  std_logic;        -- read/not write
            addBefore   : in  std_logic;        -- when low, add offset
                                                -- to address before output
            dataIn      : in  data_t;           -- input data
            useIP       : in  std_logic;        -- use IP for writing when '1'
            EN          : in  std_logic;        -- active low enable to memory
            addrOut     : out address_t;        -- address after inc/dec

            DataAB      : out address_t;        -- address to memory
            DataDB      : inout data_t;         -- data bus in and out
            DataRd      : out std_logic;        -- read signal to memory
            DataWr      : out std_logic         -- write signal to memory
        );
    end component;

    -- The stack pointer
    component Stack is
        port (
            clk         : in  std_logic;        -- system clock
            reset       : in  std_logic;        -- active low reset

            dataIn      : in  address_t;        -- new stack pointer
            ENWr        : in  std_logic;        -- when low, look at data

            SP          : out address_t         -- stack pointer
        );
    end component;

    -- All the variables we need, named to match the comments in component declarations
    signal immed    : immediate_t       := "000000000000";

    signal sel      : flagSelector_t    := "000";
    signal flagMask : status_t          := "00000000";
    signal ENRes    : std_logic         := '0';

    signal BLD      : std_logic         := '0';
    signal BST      : std_logic         := '0';
    signal clkIdx   : clockIndex_t      := 0;

    signal sourceSel  : regInSelector_t := "00";

    signal wordRegSel : wordSelector_t  := "000";

    signal regSelA  : regSelector_t     := "00000";
    signal regSelB  : regSelector_t     := "00000";
    signal ENMul    : std_logic         := '0';
    signal ENSwap   : std_logic         := '0';
    signal ENRegA   : std_logic         := '0';
    signal ENRegB   : std_logic         := '0';
    signal ENRegWr  : std_logic         := '0';

    signal dataOutA : data_t            := "00000000";
    signal dataOutB : data_t            := "00000000";
    signal wordRegOut : dataWord_t      := "0000000000000000";

    -- Data memory control
    signal memRW    : std_logic         := '0';
    signal memEN    : std_logic         := '0';
    signal addrSel  : addrSelector_t    := "00";
    signal addBefore: std_logic         := '0';
    signal decrement: std_logic         := '0';

    -- Stack pointer control
    signal SPWr     : std_logic         := '0';

    signal SP       : address_t         := "0000000000000000";
    signal addrOut  : address_t         := "0000000000000000";

begin

    RegistersUUT : Registers
        port map (
            clock,
            clkIdx,

            "00000000", -- What would otherwise be result from ALU
            DataDB,
            immed(7 downto 0),
            sourceSel,
            addrOut,
            wordRegSel,

            BLD,
            sel,
            '0',

            regSelA,
            regSelB,
            '1',
            '1',
            ENRegA,
            ENRegB,
            ENRegWr,

            open,
            dataOutA,
            dataOutB,
            wordRegOut
        );

    ControlUUT : ControlUnit
        port map (
            clock,
            '1',

            IR,
            "00000000",

            '0',

            BLD,
            BST,

            sel,
            flagMask,
            clkIdx,
            ENRes,

            immed,
            open,
            open,
            open,
            open,
            open,

            regSelA,
            regSelB,
            open,
            open,
            ENRegA,
            ENRegB,
            ENRegWr,
            sourceSel,
            wordRegSel,

            memRW,
            memEN,
            addrSel,
            addBefore,
            decrement,
            open,

            SPWr,
            open
        );

    MemoryUUT : MemoryUnit
        port map (
            clock,

            wordRegOut,
            SP,
            "0000000000000000",
            ProgDB,
            immed,
            decrement,

            addrSel,
            memRW,
            addBefore,
            dataOutA,
            '0',
            memEN,
            addrOut,

            DataAB,
            DataDB,
            DataRd,
            DataWr
        );

    -- Stack pointer
    StackPointer : Stack
        port map (
            clock,
            reset,

            addrOut,
            SPWr,

            SP
        );

end architecture;

----------------------------------------------------------------------------
--
--  This is the entity that actually tests the memory addressing
--
--  Revision History:
--     09 Feb 17  Tim Menninger     Created
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

entity MEM_TESTBENCH is
end MEM_TESTBENCH;

architecture testbench of MEM_TESTBENCH is

    -- Independent component that tests registers
    component MEM_TEST is
        port (
            IR      :  in     address_t;    -- Instruction Register
            ProgDB  :  in     address_t;    -- second word of instr
            Reset   :  in     std_logic;    -- system reset signal
            clock   :  in     std_logic;    -- system clock
            DataAB  :  out    address_t;    -- data address bus
            DataDB  :  inout  data_t;       -- data data bus
            DataRd  :  out    std_logic;    -- data read
            DataWr  :  out    std_logic     -- data write
        );
    end component;

    -- Test case files
    file MEM_vectors: text;

    -- All the variables we need
    signal clock        : std_logic         := '0';
    signal IR           : address_t         := "0000000000000000";
    signal ProgDB       : address_t         := "0000000000000000";
    signal Reset        : std_logic         := '0';
    signal DataAB       : address_t         := "0000000000000000";
    signal DataDB       : data_t            := "00000000";
    signal DataRd       : std_logic         := '0';
    signal DataWr       : std_logic         := '0';

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    MEM_UUT : MEM_TEST
        port map (IR, ProgDB, Reset, clock, DataAB, DataDB, DataRd, DataWr);

    process
        -- Variables for reading register test file
        variable currLine       : line;
        variable instruction    : address_t;
        variable instProgDB     : address_t;
        variable dataIn         : data_t;
        variable expDataAB      : address_t;
        variable expDataDB      : data_t;
        variable expRd          : std_logic;
        variable expWr          : std_logic;
        variable numClks        : std_logic_vector(1 downto 0);
        variable delimiter      : character;
        -- Number of clocks as integer
        variable nClksInt       : clockIndex_t;
        -- Iteration variable
        variable i              : clockIndex_t;
    begin
        -- Wait a few clocks with reset active
        Reset <= '0';
        wait for 200 ns;
        Reset <= '1';

        -- Open the testcase file
        file_open(MEM_vectors, "testcases/MEM_vectors.txt", read_mode);

        -- Skip first line
        readline(MEM_vectors, currLine);

        -- Go trough every test case
        while not endfile(MEM_vectors) loop
            -- Parse the line
            readline(MEM_vectors, currLine);
            read(currLine, instruction);
            read(currLine, delimiter);
            read(currLine, instProgDB);
            read(currLine, delimiter);
            read(currLine, dataIn);
            read(currLine, delimiter);
            read(currLine, expDataAB);
            read(currLine, delimiter);
            read(currLine, expDataDB);
            read(currLine, delimiter);
            read(currLine, expRd);
            read(currLine, delimiter);
            read(currLine, expWr);
            read(currLine, delimiter);
            read(currLine, numClks);

            nClksInt := conv_integer(numClks);

            -- Instruction comes in short after clock rising edge
            wait for 5 ns;
            IR <= instruction;
            DataDB <= "ZZZZZZZZ";

            -- Make sure signals inactive
            assert (std_match(DataRd, '1'))
                report  "read signal incorrect"
                severity  ERROR;
            assert (std_match(DataWr, '1'))
                report  "write signal incorrect"
                severity  ERROR;

            -- Wait until end of clock
            wait for 40 ns;

            -- Wait clock cycles if longer than 1 clock
            for i in 1 to nClksInt loop
                wait for 10 ns;
                -- ProgDB valid on second clock (third one instProgDB will be the same
                -- so we can just keep overwriting ProgDB)
                ProgDB <= instProgDB;

                wait for 40 ns;
            end loop;

            -- At the end of the cycle, when clock is low, check the outputs
            assert (std_match(DataRd, expRd))
                report  "read signal incorrect"
                severity  ERROR;
            assert (std_match(DataWr, expWr))
                report  "write signal incorrect"
                severity  ERROR;
            assert (std_match(DataDB, expDataDB))
                report  "data bus incorrect"
                severity  ERROR;
            assert (std_match(DataAB, expDataAB))
                report  "address bus incorrect"
                severity  ERROR;

            -- Finish clock cycle then repeat
            DataDB <= dataIn;
            wait for 5 ns;
        end loop;
        file_close(MEM_vectors);

        wait for 100 ns;

        -- Reset, then check that the stack pointer is all 1's
        Reset <= '0';
        wait for 10 ns;
        Reset <= '1';
        wait for 40 ns;
        IR <= "1001001000001111";
        wait for 45 ns;
        assert (std_match(DataAB, "1111111111111111"))
            report  "address bus incorrect"
            severity  ERROR;


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
