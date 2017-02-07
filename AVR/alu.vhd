-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

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
--      opA : unsigned(7 downto 0)
--          The first operand.  Consistent with the docs on wolverine.caltech.edu,
--          this is operand 1.
--      opB : unsigned(7 downto 0)
--          The first operand.  Consistent with the docs on wolverine.caltech.edu,
--          this is operand 2.  In the case of an immediate value, this will contain
--          the immediate value.
--      immed : unsigned(7 downto 0)
--          The immediate value for those instructions which require it
--      SREG : std_logic_vector(7 downto 0)
--          The status register before operation.
--      ENALU : std_logic_vector(1 downto 0)
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
--      result : std_logic_vector(7 downto 0)
--          The result of the operation.
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity ALU
--
-- Defines the inputs and outputs for the ALU of the Atmel AVR emulator
--
entity ALU is
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
--      BLD     1111100ddddd0bbb    1       Rd(b) = T
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
--      SWAP    1001010ddddd0010    1       Swap nibbles of Rd
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
    signal a      : std_logic_vector(7 downto 0);
    signal b      : std_logic_vector(7 downto 0);
    signal Cin    : std_logic;
    signal Cout   : std_logic;
    signal sum    : std_logic_vector(7 downto 0);
    component NBitAdder is
        generic (
            n       : natural := 8                          -- bits in adder
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
    signal shiftIn  : std_logic;
    signal shifted  : std_logic_vector(7 downto 0);
    signal shiftOut : std_logic;
    component NBitShifter is
        generic (
            n           : natural := 8                          -- Number of bits
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
    signal aANDb : std_logic_vector(7 downto 0);
    signal aXORb : std_logic_vector(7 downto 0);
    component NBitFBlock is
        generic (
            n           : natural := 8                          -- Number of bits
        );
        port (
            a       : in  std_logic_vector(n-1 downto 0);       -- First operand
            b       : in  std_logic_vector(n-1 downto 0);       -- Second operand
            aANDb   : out std_logic_vector(n-1 downto 0);       -- a AND b
            aXORb   : out std_logic_vector(n-1 downto 0)        -- a XOR b
        );
    end component;

begin

    -- Define components whose outputs will be muxed depending on control signals
    Adder   : NBitAdder   port map (Cin, opA, b, Cout, sum);
    Shifter : NBitShifter port map (shiftIn, opA, shifted, shiftOut);
    FBlock  : NBitFBlock  port map (a, b, aANDb, aXORb);

    -- Process to set inputs to components
    setInputs : process (Cin, SREG, ENCarry, ENImmed, ENInvOp, immed, opA, opB) is
        variable bitCat : std_logic_vector(1 downto 0);
    begin
        -- Set inputs to adder (b also used in FBlock)
        case ENCarry is
            when '0' => Cin <= SREG(C);
            when others => Cin <= '0';
        end case;

        bitCat := ENImmed & ENInvOp;
        case bitCat is
            when "00" => b <= not immed;
            when "01" => b <= immed;
            when "10" => b <= not opB;
            when others => b <= opB;
        end case;

        -- Set inputs to shifter
        case ENCarry is
            when '0' => shiftIn <= SREG(C);
            when others => shiftIn <= not ENInvOp;
        end case;

        -- Set inputs to FBlock (b computed with adder)
        case ENInvOp is
            when '0' => a <= not opA;
            when others => a <= opA;
        end case;

    end process setInputs;

    -- Process to choose which output to report
    chooseOutput : process (sum, shiftOut, aANDb, aXORb) is
        variable bitCat : std_logic_vector(2 downto 0);
    begin

        -- Concatenate bits so we can make one case statement
        bitCat := ENALU & ENInvRes;
        case bitCat is
            when "000" => result <=
                std_logic_vector(to_unsigned(conv_integer(sum) + 1, result'length));
            when "001" => result <= sum;
            when "01-" => result <= shifted;
            when "100" => result <= not aANDb;
            when "101" => result <= aANDb;
            when "110" => result <=
                std_logic_vector(to_unsigned(conv_integer(aXORb) + 1, result'length));
            when others => result <= aXORb;
        end case;

    end process chooseOutput;

end compute;
