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
--  TEST_MEM
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


entity  TEST_MEM  is

    port (
        IR      :  in     std_logic_vector(15 downto 0);    -- Instruction Register
        ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instr
        Reset   :  in     std_logic;                        -- system reset signal
        clk     :  in     std_logic;                        -- system clock
        DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
        DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
        DataRd  :  out    std_logic;                        -- data read
        DataWr  :  out    std_logic                         -- data write
    );

end  TEST_MEM;

architecture toplevel of TEST_MEM is

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

            ALUIn       : in  std_logic_vector(7 downto 0); -- data input from ALU
            memIn       : in  std_logic_vector(7 downto 0); -- data input from memory
            immedIn     : in  std_logic_vector(7 downto 0); -- immediate value from instr
            sourceSel   : in  std_logic_vector(1 downto 0); -- used to choose data source

            wordRegIn   : in  std_logic_vector(15 downto 0);-- new value for word register
            wordRegSel  : in  std_logic_vector(2 downto 0); -- selects which word register

            BLD         : in  std_logic;                    -- true when BLD occurring
            sel         : in  std_logic_vector(2 downto 0); -- bit select for BLD
            T           : in  std_logic;                    -- T flag

            regSelA     : in  std_logic_vector(4 downto 0); -- register select
            regSelB     : in  std_logic_vector(4 downto 0); -- register select
            ENMul       : in  std_logic;                    -- write to registers 0 and 1
            ENSwap      : in  std_logic;                    -- swap nibbles
            ENRegA      : in  std_logic;                    -- active low enable reg A
            ENRegB      : in  std_logic;                    -- active low enable reg B
            ENWrite     : in  std_logic;                    -- active low enable write

            Rdb         : out std_logic;                    -- b'th bit of reg A
            dataOutA    : out std_logic_vector(7 downto 0); -- low byte of output
            dataOutB    : out std_logic_vector(7 downto 0); -- high byte of output
            wordRegOut  : out std_logic_vector(15 downto 0) -- word register output
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
            ENRegWr     : out std_logic;                    -- enable register writing
            sourceSel   : out std_logic_vector(1 downto 0); -- used to choose data input
            wordReg     : out std_logic_vector(2 downto 0); -- used to choose X Y Z regs

            -- Data memory control
            memRW       : out std_logic;                    -- read/write to memory
            addrSel     : out std_logic_vector(1 downto 0); -- for address mux
            addBefore   : out std_logic;                    -- dictates when to add to addr
            decrement   : out std_logic;                    -- when low, decrementing

            -- Stack pointer control
            SPWr        : out std_logic                     -- write to stack ptr
        );
    end component;

    -- Memory unit
    component MemoryUnit is
        port (
            clk         : in  std_logic;                        -- system clock
            clkIdx      : in  natural range 0 to 3;             -- number of clocks passed

            regAddr     : in  std_logic_vector(15 downto 0);    -- address from registers
            SPAddr      : in  std_logic_vector(15 downto 0);    -- address from stack ptr
            IRAddr      : in  std_logic_vector(15 downto 0);    -- address from instruction
            immed       : in  std_logic_vector(5 downto 0);     -- memory address offset
            decrement   : in  std_logic;                        -- when low, decrement

            addrSel     : in  std_logic_vector(1 downto 0);     -- chooses which address
            RW          : in  std_logic;                        -- read/not write
            addBefore   : in  std_logic;                        -- when low, add offset
                                                                -- to address before output
            dataIn      : in  std_logic_vector(7 downto 0);     -- input data
            addrOut     : out std_logic_vector(15 downto 0);    -- address after inc/dec

            DataAB      : out std_logic_vector(15 downto 0);    -- address to memory
            DataDB      : inout std_logic_vector(7 downto 0);   -- data bus in and out
            DataRd      : out std_logic;                        -- read signal to memory
            DataWr      : out std_logic                         -- write signal to memory
        );
    end component;

    -- The stack pointer
    component Stack is
        port (
            clk         : in  std_logic;                            -- system clock
            reset       : in  std_logic;                            -- active low reset

            dataIn      : in  std_logic_vector(15 downto 0);        -- new stack pointer
            ENWr        : in  std_logic;                            -- when low, look at data

            SP          : out std_logic_vector(15 downto 0)         -- stack pointer
        );
    end component;

    -- All the variables we need
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

    signal sourceSel  : std_logic_vector(1 downto 0)  := "00";

    signal wordRegSel : std_logic_vector(2 downto 0)  := "000";

    signal regSelA  : std_logic_vector(4 downto 0)  := "00000";
    signal regSelB  : std_logic_vector(4 downto 0)  := "00000";
    signal ENMul    : std_logic                     := '0';
    signal ENSwap   : std_logic                     := '0';
    signal ENRegA   : std_logic                     := '0';
    signal ENRegB   : std_logic                     := '0';
    signal ENRegWr  : std_logic                     := '0';

    signal dataOutA : std_logic_vector(7 downto 0)  := "00000000";
    signal dataOutB : std_logic_vector(7 downto 0)  := "00000000";
    signal wordRegOut : std_logic_vector(15 downto 0) := "0000000000000000";

    -- Data memory control
    signal memRW    : std_logic                     := '0';
    signal addrSel  : std_logic_vector(1 downto 0)  := "00";
    signal addBefore: std_logic                     := '0';
    signal decrement: std_logic                     := '0';

    -- Stack pointer control
    signal SPWr     : std_logic                     := '0';

    signal SP       : std_logic_vector(15 downto 0) := "0000000000000000";
    signal addrOut  : std_logic_vector(15 downto 0) := "0000000000000000";

begin

    ALUUUT : ALU
        port map (
            dataOutA,
            dataOutB,
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
            DataDB,
            immed,
            sourceSel,
            addrOut,
            wordRegSel,

            BLD,
            sel,
            TF,

            regSelA,
            regSelB,
            ENMul,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegWr,

            Rdb,
            dataOutA,
            dataOutB,
            wordRegOut
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
            ENRegWr,
            sourceSel,
            wordRegSel,

            memRW,
            addrSel,
            addBefore,
            decrement,

            SPWr
        );

    MemoryUUT : MemoryUnit
        port map (
            clk,
            clkIdx,

            wordRegOut,
            SP,
            ProgDB,
            immed(5 downto 0),
            decrement,

            addrSel,
            memRW,
            addBefore,
            dataOutA,
            addrOut,

            DataAB,
            DataDB,
            DataRd,
            DataWr
        );

    -- Stack pointer
    StackPointer : Stack
        port map (
            clk,
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

entity MEM_TESTBENCH is
end MEM_TESTBENCH;

architecture testbench of MEM_TESTBENCH is

    -- Independent component that tests registers
    component TEST_MEM is
        port (
            IR      :  in     std_logic_vector(15 downto 0);    -- Instruction Register
            ProgDB  :  in     std_logic_vector(15 downto 0);    -- second word of instr
            Reset   :  in     std_logic;                        -- system reset signal
            clk     :  in     std_logic;                        -- system clock
            DataAB  :  out    std_logic_vector(15 downto 0);    -- data address bus
            DataDB  :  inout  std_logic_vector(7 downto 0);     -- data data bus
            DataRd  :  out    std_logic;                        -- data read
            DataWr  :  out    std_logic                         -- data write
        );
    end component;

    -- Test case files
    file MEM_vectors: text;

    -- All the variables we need
    signal clk          : std_logic                     := '0';
    signal IR           : std_logic_vector(15 downto 0) := "0000000000000000";
    signal ProgDB       : std_logic_vector(15 downto 0) := "0000000000000000";
    signal Reset        : std_logic                     := '0';
    signal DataAB       : std_logic_vector(15 downto 0) := "0000000000000000";
    signal DataDB       : std_logic_vector(7 downto 0)  := "00000000";
    signal DataRd       : std_logic                     := '0';
    signal DataWr       : std_logic                     := '0';

    -- Signifies end of simulation
    signal END_SIM      : boolean := FALSE;

begin

    MEM_UUT : TEST_MEM
        port map (IR, ProgDB, Reset, clk, DataAB, DataDB, DataRd, DataWr);

    process
        -- Variables for reading register test file
        variable currLine       : line;
        variable instruction    : std_logic_vector(15 downto 0);
        variable instProgDB     : std_logic_vector(15 downto 0);
        variable dataIn         : std_logic_vector(7 downto 0);
        variable expDataAB      : std_logic_vector(15 downto 0);
        variable expDataDB      : std_logic_vector(7 downto 0);
        variable expRd          : std_logic;
        variable expWr          : std_logic;
        variable numClks        : std_logic_vector(1 downto 0);
        variable delimiter      : character;
        -- Number of clocks as integer
        variable nClksInt       : natural range 0 to 3;
        -- Iteration variable
        variable i              : natural range 0 to 3;
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
            ProgDB <= instProgDB;
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
                wait for 50 ns;
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
