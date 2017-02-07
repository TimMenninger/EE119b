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

architecture testbench of TEST_REG is

    -- Registers component we are testing
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

    -- Signals required for passing around
    signal reset       : std_logic := '1';

    signal BLD         : std_logic;

    signal sel         : std_logic_vector(2 downto 0);
    signal flagMask    : std_logic_vector(7 downto 0);
    signal byte        : std_logic;
    signal ENRes       : std_logic;

    signal immed       : std_logic_vector(7 downto 0);
    signal ENALU       : std_logic_vector(1 downto 0);
    signal ENImmed     : std_logic;
    signal ENCarry     : std_logic;
    signal ENInvOp     : std_logic;
    signal ENInvRes    : std_logic;

    signal regSelA     : std_logic_vector(4 downto 0);
    signal regSelB     : std_logic_vector(4 downto 0);
    signal ENReg01     : std_logic;
    signal ENSwap      : std_logic;
    signal ENRegA      : std_logic;
    signal ENRegB      : std_logic;
    signal ENRegRd     : std_logic;
    signal ENRegWr     : std_logic;

    signal clkIdx      : natural range 0 to 3;

    signal T           : std_logic := '0';

    signal Rdb         : std_logic := '0';

begin

    -- Set clock index based on byte
    clkIdx <= 1 when byte = '1' else 0

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
            ENReg01,
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
