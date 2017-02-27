-----------------------------------------------------------------------------------------
--
-- alu.vhd
--
-- This contains the ALU for the Atmel AVR emulator.  It takes the opcode and the
-- operands and performs the respective instruction on them.  The operands are the
-- actual values to be used, not addresses or register indices.  Note here that two
-- clock instructions have their second clock start at the first rising edge after
-- valid instruction
--
-- Inputs:
--      opA : data_t
--          The first operand.  Consistent with the docs on wolverine.caltech.edu,
--          this is operand 1.
--      opB : data_t
--          The first operand.  Consistent with the docs on wolverine.caltech.edu,
--          this is operand 2.  In the case of an immediate value, this will contain
--          the immediate value.
--      immed : data_t
--          The immediate value for those instructions which require it
--      SREG : status_t
--          The status register before operation.
--      ENALU : ALUSelector_t
--          Describes type of operation.  When 0, use adder, when 1, use shifter
--          when 2, use AND from fblock, when 3, use XOR from fblock
--      ENCarry : std_logic
--          Active low to indicate carry should be respected
--      ENImmed : std_logic
--          Active low to indicate operation uses immed
--      ENInvOp : std_logic
--          Active low to indicate that the operand(s) should be inverted before
--          input to respective components
--      ENInvRes : std_logic
--          Active low to indicate that the result should be inverted in the case
--          of AND, and 1 should be added to it (to finish negation) in the case
--          of the Adder and XOR
--      ENMul : std_logic
--          Signal that notifies the system of a MUL instruction
--      clkIdx : clockIndex_t
--          How many clocks have passed since the start of the instruction
--
-- Outputs:
--      Rd0 : std_logic
--          Bit 0 of operand A, used by status register to compute flags
--      Rd3 : std_logic
--          Bit 3 of operand A, used by status register to compute flags
--      Rr3 : std_logic
--          Bit 3 of operand B, used by status register to compute flags
--      Rd7 : std_logic
--          Bit 7 of operand A, used by status register to compute flags
--      Rr7 : std_logic
--          Bit 7 of operand B, used by status register to compute flags
--      result : data_t
--          The result of the operation.
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library common;
use common.common.all;

--
-- entity ALU
--
-- Defines the inputs and outputs for the ALU of the Atmel AVR emulator
--
entity ALU is
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
end ALU;

--
-- architecture compute
--
-- Takes the opcode and computes the outputs, putting them on dataOut's low byte
-- (unless it is a 2-byte result).  Below are the insturctions included, their
-- instruction code, description, and number of clocks.  Here, we will use the
-- terminology:
--      Rd  - 4-bit register select corresponding to 'd' bits in instruction
--      Rr  - 4-bit register select corresponding to 'r' bits in instruction
--      K   - 8-bit Immediate value from 'K' bits in instruction
--      b   - 3-bit bit index into a register byte from 'b' bits in instruction
--      s   - 3-bit bit index into status register from 's' bits in instruction
--      R#  - 8-bit register number #
-- It handles the status register, which is comprised of 8 named bits, listed
-- from highest bit (7) to lowest (0) in the file header.  Instructions without
-- a LHS and RHS of '=' set the status as if the instruction were executed.
-- Instructions handled:
--      Opcode  Instruction         Clks    Description
--      ADC     000111rdddddrrrr    1       Rd = Rd + Rr + C
--      ADD     000011rdddddrrrr    1       Rd = Rd + Rr
--      ADIW    10010110KKddKKKK    2       Rd+1|Rd = Rd+1|Rd + K
--      AND     001000rdddddrrrr    1       Rd = Rd AND Rr
--      ANDI    0111KKKKddddKKKK    1       Rd = Rd AND K
--      ASR     1001010ddddd0101    1       Rd arithmetic shift right
--      BCLR    100101001sss1000    1       SREG(s) = 0
--      BSET    100101000sss1000    1       SREG(s) = 1
--      BST     1111101dddddXbbb    1       T = Rd(b)
--      COM     1001010ddddd0000    1       Rd = NOT Rd
--      CP      000101rdddddrrrr    1       Rd - Rr
--      CPC     000001rdddddrrrr    1       Rd - Rr - C
--      CPI     0011KKKKddddKKKK    1       Rd - K
--      DEC     1001010ddddd1010    1       Rd = Rd - 1
--      EOR     001001rdddddrrrr    1       Rd = Rd XOR Rr
--      INC     1001010ddddd0011    1       Rd = Rd + 1
--      LSR     1001010ddddd0110    1       Rd = logical shift right
--      MUL     100111rdddddrrrr    2       R1|R0 = Rd * Rr
--      NEG     1001010ddddd0001    1       Rd = -1 * Rd
--      OR      001010rdddddrrrr    1       Rd = Rd OR Rr
--      ORI     0110KKKKddddKKKK    1       Rd = Rd OR K
--      ROR     1001010ddddd0111    1       Rd = rotate right
--      SBC     000010rdddddrrrr    1       Rd = Rd - Rr - C
--      SBCI    0100KKKKddddKKKK    1       Rd = Rd - K - C
--      SBIW    10010111KKddKKKK    2       Rd+1|Rd = Rd+1|Rd - K
--      SUB     000110rdddddrrrr    1       Rd = Rd - Rr
--      SUBI    0101KKKKddddKKKK    1       Rd = Rd - K
--
architecture compute of ALU is

    -- Constants used for bit selection from the status register
    constant C : natural := 0;

    -- Define components used.  For this, we define a component for every possible
    -- set of inputs, then based on the opcode, mux which one we use
    -- n-bit adder

    -------------------------------------------------------------------------------------
    -- Adder
    -------------------------------------------------------------------------------------
    -- Signals used in the adder component
    signal a      : data_t              := "00000000";
    signal b      : data_t              := "00000000";
    signal Cin    : std_logic           := '0';
    signal Cout   : std_logic           := '0';
    signal sum    : data_t              := "00000000";
    component NBitAdder is
        generic (
            n       : natural           := dataBits         -- bits in adder
        );
        port (
            Cin     : in  std_logic;                        -- Cin
            a       : in  std_logic_vector(n-1 downto 0);   -- operand A
            b       : in  std_logic_vector(n-1 downto 0);   -- operant B

            Cout    : out std_logic;                        -- Cout
            result  : out std_logic_vector(n-1 downto 0)    -- sum of A and B
        );
    end component;

    -------------------------------------------------------------------------------------
    -- Shifter
    -------------------------------------------------------------------------------------
    -- Signals used in the shifter component
    signal shiftIn  : std_logic         := '0';
    signal shifted  : data_t            := "00000000";
    signal shiftOut : std_logic         := '0';
    component NBitShifter is
        generic (
            n           : natural       := dataBits             -- Number of bits
        );
        port (
            ShiftIn     : in  std_logic;                        -- Bit shifted in
            ToShift     : in  std_logic_vector(n-1 downto 0);   -- Value to shift right
            Shifted     : out std_logic_vector(n-1 downto 0);   -- Shifted value
            ShiftOut    : out std_logic                         -- Bit shifted out
        );
    end component;

    -------------------------------------------------------------------------------------
    -- F Blocks
    -------------------------------------------------------------------------------------
    -- Signals used in F blocks.
    signal aANDb : data_t               := "00000000";
    signal aXORb : data_t               := "00000000";
    component NBitFBlock is
        generic (
            n       : natural           := dataBits             -- Number of bits
        );
        port (
            a       : in  std_logic_vector(n-1 downto 0);       -- First operand
            b       : in  std_logic_vector(n-1 downto 0);       -- Second operand
            aANDb   : out std_logic_vector(n-1 downto 0);       -- a AND b
            aXORb   : out std_logic_vector(n-1 downto 0)        -- a XOR b
        );
    end component;

    -------------------------------------------------------------------------------------
    -- General correction signals
    -------------------------------------------------------------------------------------
    -- Product of two operands
    signal product      : dataWord_t    := "0000000000000000";
    signal productOut   : data_t        := "00000000"; -- depends on clock
    -- Signals used in result adder, summand is Cin
    signal summand      : std_logic     := '0';
    signal addToThis    : data_t        := "00000000";
    -- Corrected AND output, relevant if ANDing and inverting
    signal aANDbCorrect : data_t        := "00000000";
    -- We use an intermediate b, which contains either opB or immed
    signal bInter       : data_t        := "00000000";
    -- Result of ALU for anything that isn't multiplication
    signal ALURes       : data_t        := "00000000";

begin

    -- These signals always go to status and don't depend on control signals
    Rd0 <= opA(0);                                  -- bit 0 of operand A
    Rd3 <= opA(3);                                  -- bit 3 of operand A
    Rr3 <= opB(3) when ENImmed = '1' else immed(3); -- bit 3 of operand B
    Rd7 <= opA(7);                                  -- bit 7 of operand A
    Rr7 <= opB(7) when ENImmed = '1' else immed(7); -- bit 7 of operand B

    -------------------------------------------------------------------------------------
    --
    -- SET INPUTS
    --
    -- Use control signals to set inputs to set the inputs to the components, from which
    -- we will choose one as the result we use
    --
    -- Adder : Full adder on two inputs
    -- Shifter : Shifts one input.  Argue value to shift in and returns shifted out val
    -- FBlock : Returns AND and XOR of two inputs
    --

    -- Define components whose outputs will be muxed depending on control signals
    Adder   : NBitAdder   port map (Cin, opA, b, Cout, sum);
    Shifter : NBitShifter port map (shiftIn, opA, shifted, shiftOut);
    FBlock  : NBitFBlock  port map (a, b, aANDb, aXORb);

    -------------------------------------------------------------------------------------
    --
    -- ADDER SPECIFIC SIGNALS
    --
    -- Cin : Cin to n-bit full adder
    --
    -------------------------------------------------------------------------------------

    -- Set Cin to adder based on carry flag enabler.  When we are inverting operands
    -- i.e. subtracting, we never use Cin
    Cin <= SREG(C) when ENCarry = '0' and ENInvOp = '1' else '0';

    -------------------------------------------------------------------------------------
    --
    -- SHARED SIGNALS by adder and FBlock
    --
    -- bInter : Contains b input, either register or immediate
    -- b : The b input, either bInter or bInter inverted, based on control signals
    --
    -------------------------------------------------------------------------------------

    -- Set intermediate b to either operand or immediate based on control signals
    bInter <= immed when ENImmed = '0' else opB;

    -- The b input will reflect bInter (which is immediate or operand b).  Here, we
    -- invert based on control signal
    b <= not bInter when ENInvOp = '0' else bInter;

    -------------------------------------------------------------------------------------
    --
    -- FBLOCK SPECIFIC SIGNALS
    --
    -- a : FBlock is the only time we don't just propagate a
    -- aANDbCorrect : Corrects the aANDb output based on whether we want it inverted
    --
    -------------------------------------------------------------------------------------

    -- We want to invert a based on control signal.  The reason this is FBlock specific
    -- is because there are times when we invert b and not a, but that never happens
    -- for FBlock
    a <= not opA when ENInvOp = '0' else opA;

    -- Set the correct aANDb value based on whether we are inverting result
    aANDbCorrect <= not aANDb when ENInvRes = '0' else aANDb;

    -------------------------------------------------------------------------------------
    --
    -- SHIFTER SPECIFIC SIGNALS
    --
    -- ShiftIn : What is shifted into the top bit
    --
    -------------------------------------------------------------------------------------

    -- Set what is shifted in.  Sometimes it is 0, sometimes it is the sign bit and
    -- sometimes it is the carry flag
    shiftIn <= SREG(C) when ENCarry = '0' else opA(7) and not ENInvRes;

    -------------------------------------------------------------------------------------
    --
    -- MULTIPLIER SIGNALS
    --
    -- Product : The product of operands
    --
    -------------------------------------------------------------------------------------

    -- Get product and output high or low byte depending on clock index.
    product <= opA * opB;
    productOut <= product(15 downto 8) when clkIdx = 1 else product(7 downto 0);

    -------------------------------------------------------------------------------------
    --
    -- CHOOSE OUTPUT
    --
    -- Use control signals to choose which output we want to ultimately take out of the
    -- components' results from above.
    --
    -- FinalResult : Some results require 1 be added to it.  For example, anything where
    --               we are simulating subtraction by adding the negative of the second
    --               argument

    -- Choose what operation output we are going to use.  We load it into addToThis
    -- in case we also want to add 1 to it.
    with ENALU select addToThis <=
        sum          when "00",
        shifted      when "01",
        aANDbCorrect when "10",
        aXORb        when others;

    -- Select what we are adding to the result before clocking it
    with ENALU select summand <=
        (not SREG(C) or ENCarry) and not ENInvRes when "00",
        not ENInvRes                              when "11",
        '0'                                       when others;

    -- Here, we are either adding 0 or 1.  We will thus put it into Cin and add 0.  This
    -- way we can more easily use std_logic as the summand without converting it to
    -- an 8-bit std_logic_vector
    FinalResult  : NBitAdder port map (summand, addToThis, "00000000", open, ALURes);

    -- Finally, choose the output, either the result from the F, T and Adder, or from
    -- the product based on the multiply control signal
    result <= productOut when ENMul = '0' else ALURes;

end compute;
