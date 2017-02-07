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

architecture testbench of TEST_ALU is

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
            ENReg01     : in  std_logic;                    -- write to registers 0 and 1
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

            sel         : in  std_logic_vector(2 downto 0);         -- selects flag index
            mask        : in  std_logic_vector(7 downto 0);         -- masks unaffected flags
            byte        : in  std_logic;                            -- byte index of result
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

            sel         : out std_logic_vector(2 downto 0); -- selects flag index
            flagMask    : out std_logic_vector(7 downto 0); -- status bits affected
            byte        : out std_logic;                    -- byte index of result
            ENRes       : out std_logic;                    -- set SREG to R

            immed       : out std_logic_vector(7 downto 0); -- immediate value
            ENALU       : out std_logic_vector(1 downto 0); -- ALU operation type
            ENImmed     : out std_logic;                    -- enable immed
            ENCarry     : out std_logic;                    -- enable carry
            ENInvOp     : out std_logic;                    -- negate operand in ALU
            ENInvRes    : out std_logic;                    -- negate result in ALU

            regSelA     : out std_logic_vector(4 downto 0); -- register A select
            regSelB     : out std_logic_vector(4 downto 0); -- register B select
            ENReg01     : out std_logic;                    -- write to registers 0 and 1
            ENSwap      : out std_logic;                    -- SWAP instruction
            ENRegA      : out std_logic;                    -- enable register A
            ENRegB      : out std_logic;                    -- enable register B
            ENRegRd     : out std_logic;                    -- enable register reading
            ENRegWr     : out std_logic                     -- enable register writing
        );
    end component;

    -- All the variables we need
    signal reset    : std_logic := '1';

    signal immed    : std_logic_vector(7 downto 0);
    signal SREG     : std_logic_vector(7 downto 0);

    signal ENALU    : std_logic_vector(1 downto 0);
    signal ENCarry  : std_logic;
    signal ENImmed  : std_logic;
    signal ENInvOp  : std_logic;
    signal ENInvRes : std_logic;

    signal Rd0      : std_logic;
    signal Rd3      : std_logic;
    signal Rr3      : std_logic;
    signal Rd7      : std_logic;
    signal Rr7      : std_logic;

    signal R        : std_logic_vector(7 downto 0);
    signal Rdb      : std_logic;

    signal sel      : std_logic_vector(2 downto 0);
    signal flagMask : std_logic_vector(7 downto 0);
    signal byte     : std_logic;
    signal ENRes    : std_logic;

    signal TF       : std_logic;

    signal BLD      : std_logic;
    signal clkIdx   : natural range 0 to 1;

    signal regSelA  : std_logic_vector(4 downto 0);
    signal regSelB  : std_logic_vector(4 downto 0);
    signal ENReg01  : std_logic;
    signal ENSwap   : std_logic;
    signal ENRegA   : std_logic;
    signal ENRegB   : std_logic;
    signal ENRegRd  : std_logic;
    signal ENRegWr  : std_logic;

    signal dataOutA : std_logic_vector(7 downto 0);
    signal dataOutB : std_logic_vector(7 downto 0);

begin

    -- Output the status signal being passed around
    StatReg <= SREG;

    -- Output the result signal being passed around
    Result <= R;

    -- Set clkIdx to natural according to byte
    clkIdx <= 1 when byte = '1' else 0;

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
            ENReg01,
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

            sel,
            flagMask,
            byte,
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

            sel,
            flagMask,
            byte,
            ENRes,

            immed,
            ENALU,
            ENImmed,
            ENCarry,
            ENInvOp,
            ENInvRes,

            regSelA,
            regSelB,
            ENReg01,
            ENSwap,
            ENRegA,
            ENRegB,
            ENRegRd,
            ENRegWr
        );

end architecture;
