-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

-----------------------------------------------------------------------------------------
--
-- control.vhd
--
-- This is the control unit component of the Atmel AVR emulator.  It is responsible for
-- decoding an instruction and, from that, sending all of the relevant signals to the
-- rest of the system such that the instruction is executed.  For more information on
-- how instructions should be decoded, refer to EE 119b HW3, HW4, HW5 and HW6.  Also
-- included in this are the instruction register, the instruction pointer/program
-- counter and the stack pointer.  It is also responsible for fetching the instruction,
-- and when necessary, handling the stack and instruction pointer accordingly.  It
-- also detects when the instruction has another word and will fetch that, too, when
-- applicable.
--
-- Registers:
--      IR : std_logic_vector(15 downto 0)
--          The instruction register
--      IP : std_logic_vector(15 downto 0)
--          The instruction pointer
--      SP : std_logic_vector(15 downto 0)
--          The stack pointer
--
-- Inputs:
--      clk : std_logic
--          The global clock
--      reset : std_logic
--          Active low global reset signal
--      instruction : instruction_t
--          The instruction from memory that is to be decoded.
--
-- Outputs:
--      byte : std_logic
--          Tells us if we are on byte 0 or 1 of the write/read
--      BLD : std_logic
--          '1' when BLD instruction occurring.
--      BST : std_logic
--          '1' when BST instruction occurring.
--      sel : std_logic_vector(2 downto 0)
--          Selects flag index and register index for bit setting
--      flagMask : std_logic_vector(7 downto 0)
--          A mask showing which bits in the status register should be affected by the
--          instruction.  Affected bits are represented by '0'
--      ENALU : std_logic_vector(1 downto 0)
--          Describes type of operation.
--      regSelA : std_logic_vector(4 downto 0)
--          Represents which of the 32 registers to use as register A.  For more info
--          on register A, refer to registers component documentation.
--      regSelB : std_logic_vector(4 downto 0)
--          Represents which of the 32 registers to use as register B.  For more info
--          on register B, refer to registers component documentation.
--      immed : std_logic_vector(7 downto 0)
--          The immediate value to use if the opcode specifies one.
--      ENCarry : std_logic
--          Tells the ALU that the operation uses a carry.  This is only relevant on
--          adder operations
--      ENImmed : std_logic
--          Tells the system that the data on immed bus is to be used.
--      ENInvOp : std_logic
--          Tells ALU to invert the operand
--      ENInvRes : std_logic
--          Tells ALU to invert the result
--      ENMul : std_logic
--          Active low enable signal that tells register to write specifically to
--          registers 0 and 1
--      ENSwap : std_logic
--          Indicates that the nibbles in the register should be swapped
--      ENRegA : std_logic
--          Enable signal for register A in registers component.  For more info on
--          register A, refer to registers component documentation.
--      ENRegB : std_logic
--          Enable signal for register B in registers component.  For more info on
--          register B, refer to registers component documentation.
--      ENRegRd : std_logic
--          Enables (active low) reading of registers
--      ENRegWr : std_logic
--          Enables (active low) writing to registers
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity ControlUnit
--
-- Defines the inputs and outputs for the control unit of the Atmel AVR
-- emulator
--
entity ControlUnit is
    port (
        clk         : in  std_logic;                    -- system clock
        reset       : in  std_logic;                    -- system reset

        instruction : in  std_logic_vector(15 downto 0);-- instruction

        BLD         : out std_logic;                    -- '1' when BLD
        BST         : out std_logic;                    -- '1' when BST

        sel         : out std_logic_vector(2 downto 0); -- selects flag index
        flagMask    : out std_logic_vector(7 downto 0); -- status bits affected
        clkIdx      : out natural range 0 to 3;         -- clock counter
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
end ControlUnit;

--
-- architecture decoder of ControlUnit
--
-- Decodes the instruction and sends control signals
--
architecture decoder of ControlUnit is

    -- How many clocks an instruction requires
    signal numClks : natural range 0 to 3 := 0;

    -- How many clocks since instruction began
    signal clkCnt : natural range 0 to 3 := 0;

    -- All possible opcodes
    subtype instruction_t is std_logic_vector(15 downto 0);

 --  ALU opcodes

    constant OpADC    :  instruction_t := "000111----------";   -- ADC Rd, Rr
    constant OpADD    :  instruction_t := "000011----------";   -- ADD Rd, Rr
    constant OpADIW   :  instruction_t := "10010110--------";   -- ADIW Rdl, K
    constant OpAND    :  instruction_t := "001000----------";   -- AND Rd, Rr
    constant OpANDI   :  instruction_t := "0111------------";   -- ANDI Rd, K
    constant OpASR    :  instruction_t := "1001010-----0101";   -- ASR Rd
    constant OpBCLR   :  instruction_t := "100101001---1000";   -- BCLR s
    constant OpBLD    :  instruction_t := "1111100-----0---";   -- BLD Rd, b
    constant OpBSET   :  instruction_t := "100101000---1000";   -- BSET s
    constant OpBST    :  instruction_t := "1111101---------";   -- BST Rr, b
    constant OpCOM    :  instruction_t := "1001010-----0000";   -- COM Rd
    constant OpCP     :  instruction_t := "000101----------";   -- CP Rd, Rr
    constant OpCPC    :  instruction_t := "000001----------";   -- CPC Rd, Rr
    constant OpCPI    :  instruction_t := "0011------------";   -- CPI Rd, K
    constant OpDEC    :  instruction_t := "1001010-----1010";   -- DEC Rd
    constant OpEOR    :  instruction_t := "001001----------";   -- EOR Rd, Rr
    constant OpINC    :  instruction_t := "1001010-----0011";   -- INC Rd
    constant OpLSR    :  instruction_t := "1001010-----0110";   -- LSR Rd
    constant OpMUL    :  instruction_t := "100111----------";   -- MUL Rd, Rr
    constant OpNEG    :  instruction_t := "1001010-----0001";   -- NEG Rd
    constant OpOR     :  instruction_t := "001010----------";   -- OR Rd, Rr
    constant OpORI    :  instruction_t := "0110------------";   -- ORI Rd, K
    constant OpROR    :  instruction_t := "1001010-----0111";   -- ROR Rd
    constant OpSBC    :  instruction_t := "000010----------";   -- SBC Rd, Rr
    constant OpSBCI   :  instruction_t := "0100------------";   -- SBCI Rd, K
    constant OpSBIW   :  instruction_t := "10010111--------";   -- SBIW Rdl, K
    constant OpSUB    :  instruction_t := "000110----------";   -- SUB Rd, Rr
    constant OpSUBI   :  instruction_t := "0101------------";   -- SUBI Rd, K
    constant OpSWAP   :  instruction_t := "1001010-----0010";   -- SWAP Rd

 --  Load and Store Opcodes

    constant OpELPM   :  instruction_t := "1001010111011000";   -- ELPM
    constant OpELPMZ  :  instruction_t := "1001000-----0110";   -- ELPM Rd, Z
    constant OpELPMZI :  instruction_t := "1001000-----0111";   -- ELPM Rd, Z+
    constant OpLDX    :  instruction_t := "1001000-----1100";   -- LD Rd, X
    constant OpLDXI   :  instruction_t := "1001000-----1101";   -- LD Rd, X+
    constant OpLDXD   :  instruction_t := "1001000-----1110";   -- LD Rd, -X
    constant OpLDYI   :  instruction_t := "1001000-----1001";   -- LD Rd, Y+
    constant OpLDYD   :  instruction_t := "1001000-----1010";   -- LD Rd, -Y
    constant OpLDDY   :  instruction_t := "10-0--0-----1---";   -- LDD Rd, Y + q
    constant OpLDZI   :  instruction_t := "1001000-----0001";   -- LD Rd, Z+
    constant OpLDZD   :  instruction_t := "1001000-----0010";   -- LD Rd, -Z
    constant OpLDDZ   :  instruction_t := "10-0--0-----0---";   -- LDD Rd, Z + q
    constant OpLDI    :  instruction_t := "1110------------";   -- LDI Rd, k
    constant OpLDS    :  instruction_t := "1001000-----0000";   -- LDS Rd, m
    constant OpLPM    :  instruction_t := "1001010111001000";   -- LPM
    constant OpLPMZ   :  instruction_t := "1001000-----0100";   -- LPM Rd, Z
    constant OpLPMZI  :  instruction_t := "1001000-----0101";   -- LPM Rd, Z+
    constant OpMOV    :  instruction_t := "001011----------";   -- MOV Rd, Rr
    constant OpMOVW   :  instruction_t := "00000001--------";   -- MOVW Rd, Rr
    constant OpSPM    :  instruction_t := "1001010111101000";   -- SPM
    constant OpSTX    :  instruction_t := "1001001-----1100";   -- ST X, Rr
    constant OpSTXI   :  instruction_t := "1001001-----1101";   -- ST X+, Rr
    constant OpSTXD   :  instruction_t := "1001001-----1110";   -- ST -X, Rr
    constant OpSTYI   :  instruction_t := "1001001-----1001";   -- ST Y+, Rr
    constant OpSTYD   :  instruction_t := "1001001-----1010";   -- ST -Y, Rr
    constant OpSTDY   :  instruction_t := "10-0--1-----1---";   -- STD Y + q, Rr
    constant OpSTZI   :  instruction_t := "1001001-----0001";   -- ST Z+, Rr
    constant OpSTZD   :  instruction_t := "1001001-----0010";   -- ST -Z, Rr
    constant OpSTDZ   :  instruction_t := "10-0--1-----0---";   -- STD Z + q, Rr
    constant OpSTS    :  instruction_t := "1001001-----0000";   -- STS m, Rr

 --  Push and Pop Opcodes

    constant OpPOP    :  instruction_t := "1001000-----1111";   -- POP Rd
    constant OpPUSH   :  instruction_t := "1001001-----1111";   -- PUSH Rd

 --  Unconditional Branches

    constant OpEICALL :  instruction_t := "1001010100011001";   -- EICALL
    constant OpEIJMP  :  instruction_t := "1001010000011001";   -- EIJMP
    constant OpJMP    :  instruction_t := "1001010-----110-";   -- JMP a
    constant OpRJMP   :  instruction_t := "1100------------";   -- RJMP j
    constant OpIJMP   :  instruction_t := "10010100----1001";   -- IJMP
    constant OpCALL   :  instruction_t := "1001010-----111-";   -- CALL a
    constant OpRCALL  :  instruction_t := "1101------------";   -- RCALL j
    constant OpICALL  :  instruction_t := "10010101----1001";   -- ICALL
    constant OpRET    :  instruction_t := "100101010--01000";   -- RET
    constant OpRETI   :  instruction_t := "100101010--11000";   -- RETI

 --  Conditional Branches

    constant OpBRBC   :  instruction_t := "111101----------";   -- BRBC s, r
    constant OpBRBS   :  instruction_t := "111100----------";   -- BRBS s, r

 --  Skip Instructions

    constant OpCPSE   :  instruction_t := "000100----------";   -- CPSE Rd, Rr
    constant OpSBIC   :  instruction_t := "10011001--------";   -- SBIC p, b
    constant OpSBIS   :  instruction_t := "10011011--------";   -- SBIS p, b
    constant OpSBRC   :  instruction_t := "1111110---------";   -- SBRC Rr, b
    constant OpSBRS   :  instruction_t := "1111111---------";   -- SBRS Rr, b

 --  I/O Instructions

    constant OpCBI    :  instruction_t := "10011000--------";   -- CBI p, b
    constant OpIN     :  instruction_t := "10110-----------";   -- IN Rd, p
    constant OpOUT    :  instruction_t := "10111-----------";   -- OUT p, Rr
    constant OpSBI    :  instruction_t := "10011010--------";   -- SBI p, b

 --  Miscellaneous Instructions

    constant OpBREAK  :  instruction_t := "1001010110011000";   -- BREAK
    constant OpNOP    :  instruction_t := "0000000000000000";   -- NOP
    constant OpSLP    :  instruction_t := "10010101100-1000";   -- SLEEP
    constant OpWDR    :  instruction_t := "10010101101-1000";   -- WDR

begin

    --
    -- counter process
    --
    -- Increments the clock counter
    --
    counter: process (clk) is
    begin
        if (rising_edge(clk)) then
            if (clkCnt = numClks) then
                clkCnt <= 0;
            else
                clkCnt <= clkCnt + 1;
            end if;
        end if;
    end process counter;

    decode: process (reset, instruction, clkCnt) is
    begin
        -- Output the clock count in this instruction as the clock index
        clkIdx <= clkCnt;

        -----------------------------------------------------------------------------
        --
        -- General control signal default values
        --
        -----------------------------------------------------------------------------
        BLD      <= '0';            -- Not currently in BLD instruction
        BST      <= '0';            -- Not currently in BST instruction
        sel      <= "000";          -- Most common value
        flagMask <= "11111111";     -- Change no flags by default
        ENRes    <= '1';            -- Rare that ALU sets SREG

        immed    <= "00000000";     -- Arbitrary, for INC instructions
        ENALU    <= "00";           -- Arbitrary
        ENImmed  <= '1';            -- Usually not using immediate value
        ENCarry  <= '1';            -- Arbitrary
        ENInvOp  <= '1';            -- Usually not inverting ALU operands
        ENInvRes <= '1';            -- Usually not inverting ALU outputs

        regSelA  <= "00000";        -- Arbitrary
        regSelB  <= "00000";        -- Arbitrary
        ENRegA   <= '1';            -- By default, don't propagate data to output
        ENRegB   <= '1';            -- By default, don't propagate data to output
        ENMul    <= '1';            -- Only MUL enables this
        ENSwap   <= '1';            -- Only SWAP enables this
        ENRegRd  <= '1';            -- Don't access registers by default
        ENRegWr  <= '1';            -- Don't access registers by default

        numClks  <= 0;              -- Assume this is a 1-clock instruction

        -----------------------------------------------------------------------------
        --
        -- ALU instruction decoding
        --
        -----------------------------------------------------------------------------

        -- Check each individual opcode and update outputs accordingly.  Note each
        -- is unique, so we never have conflicting results and can do if's.  We only
        -- explicilty set values that differ from the default

        -- Add with carry
        if (std_match(instruction, OpADC))   then
            -- Set values used by ALU
            ENCarry  <= '0';            -- Arbitrary

            -- Set values used by registers
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Add without carry
        if (std_match(instruction, OpADD))   then
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Add immediate to word
        if (std_match(instruction, OpADIW))  then
            -- We will do two adds.  On the low byte, we add the immediate byte
            -- without carry.  We store the carry bit, and on the high byte, we add
            -- immediate again (now changed to 0) with carry to propagate that bit to
            -- the top bit if necessary.

            -- This is only two clocks, so we always set counter to 1 because either
            -- we want to switch to 1 or it no longer matters
            numClks <= 1;

            -- Values used by ALU
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Writing low byte (first) to regSelA and second byte to regSelA + 1
            -- Rd in bits 5-4, K in bits 7-6, 3-0
            case clkCnt is
                when 0 =>
                    -- On first clock:
                    --      immed: Add value from instruction
                    --      regSelA: From instruction
                    immed <= "00" & instruction(7 downto 6) & instruction(3 downto 0);
                    regSelA <= "000" & instruction(5 downto 4);
                when others =>
                    -- On second clock:
                    --      ENCarry: Use carry from low byte add
                    --      immed: Add 0 with carry to propagate add to high byte
                    --      regSelA: One greater than from instruction
                    ENCarry <= '0';
                    immed <= "00000000";
                    regSelA <= std_logic_vector(to_unsigned(
                        conv_integer(instruction(5 downto 4)) + 1, regSelA'length
                    ));
            end case;

            -- Always reading something from register A
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction, and on
            -- both clocks
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Bitwise AND
        if (std_match(instruction, OpAND))   then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Bitwise AND with immediate
        if (std_match(instruction, OpANDI))  then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);

            -- Using immediate value, want to enable it
            ENImmed <= '0';
            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Arithmetic shift right
        if (std_match(instruction, OpASR))   then
            -- Values used by status
            sel <= "001";

            -- Values used by ALU
            ENALU    <= "01";           -- Use shifter
            ENInvRes <= '0';            -- Tells ALU this is ASR not LSR

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Clear status bit
        if (std_match(instruction, OpBCLR))  then
            -- We will set mask to set only desired bit, and trick ALU into returning
            -- all 0's by making it think it is ANDI with "00000000".  Status will
            -- then clear the empty bit in the mask
            -- Values used by status
            ENRes <= '0';               -- ALU setting SREG

            -- Values used by ALU
            ENALU    <= "10";           -- ANDing
            ENImmed  <= '0';            -- Using immediate value (default 0) to AND
                                        -- with whatever arbitrary value appears on
                                        -- opA which will result in 0 sent to status.
                                        -- Status then sees this, masks out
                                        -- everything except that one bit which is 0

            -- Flags handled by status enabled
            flagMask(conv_integer(instruction(6 downto 4))) <= '0';
        end if;

        -- Load T into register bit
        if (std_match(instruction, OpBLD))   then
            -- Values used by registers
            BLD <= '1';
            sel <= instruction(2 downto 0);

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- Affects no flags
        end if;

        -- Set bit in status
        if (std_match(instruction, OpBSET))  then
            -- Trick ALU into sending all 1's to status, from which mask will take
            -- the one we care about
            -- Values used by status
            ENRes <= '0';               -- Setting bit

            -- Values used by ALU
            ENImmed  <= '0';            -- OR this immediate with opA to get all 0's,
                                        -- which will be inverted to be all 1's
                                        -- instead, then the status entity will then
                                        -- extract a bit based on the flag mask
                                        -- Note: immed default value is "00000000"
            ENALU    <= "10";           -- Want to AND
            ENInvRes <= '0';            -- Invert output 0's to output 1's

            -- Flags handled by status enabled
            flagMask(conv_integer(instruction(6 downto 4))) <= '0';
        end if;

        -- Store bit from register in T
        if (std_match(instruction, OpBST))   then
            -- Indicate BST instruction
            BST <= '1';

            -- Load into sel the bit index
            sel <= instruction(2 downto 0);

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- Explicitly sets T flag in status
        end if;

        -- Complement/invert
        if (std_match(instruction, OpCOM))   then
            -- Values used by status
            sel <= "010";

            -- Values used by ALU
            immed    <= "11111111";     -- a XOR 1 always inverts a
            ENALU    <= "11";           -- Use XOR
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Compare
        if (std_match(instruction, OpCP))    then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert to add a negative
            ENInvRes <= '0';            -- Add one to output to finish two's comp

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We only read registers on this
            ENRegA <= '0';
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Compare with carry
        if (std_match(instruction, OpCPC))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert operand to add a negative
            ENInvRes <= '0';            -- Invert result to finish two's comp
            ENCarry  <= '0';            -- Using carry flag

            -- Values used by registers
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We only read registers on compare
            ENRegA <= '0';
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Compare with immediate
        if (std_match(instruction, OpCPI))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENImmed  <= '0';            -- Use immediate value
            ENInvOp  <= '0';            -- Invert operand for adding negative
            ENInvRes <= '0';            -- Add one to result to finish two's comp

            -- Values used by registers
            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We only read on compares
            ENRegA <= '0';
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Decrement
        if (std_match(instruction, OpDEC))   then
            -- Trick machine into subtracting 1 (adding -1)

            -- Values used by ALU
            immed    <= "00000001";     -- Subracting 1
            ENImmed  <= '0';            -- Use immediate
            ENInvOp  <= '0';            -- Invert operand for adding negative
            ENInvRes <= '0';            -- Add one to result to finish two's comp

            -- Values used by registers
            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Bitwise XOR
        if (std_match(instruction, OpEOR))   then
            -- Values used by status
            sel <= "010";

            -- Values used by ALU
            ENALU <= "11";              -- Use XOR

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Increment
        if (std_match(instruction, OpINC))   then
            -- Values used by status
            sel <= "101";

            -- Values used by ALU
            immed    <= "00000001";     -- 1 for INC adder
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Logical shift right
        if (std_match(instruction, OpLSR))   then
            ENALU <= "01"; -- use shifter

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Multiply
        if (std_match(instruction, OpMUL))   then
            -- This is only two clocks, so we always set counter to 1 because either
            -- we want to switch to 1 or it no longer matters
            numClks <= 1;

            -- Writing low byte (first) to regSelA and second byte to regSelA + 1
            -- Rd in bits 5-4, K in bits 7-6, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- Values used by status
            sel <= "101";

            -- Special register read/write signal for this instruction
            ENMul <= '0';

            -- Affects C Z flags
            flagMask <= "11111100";
        end if;

        -- Negate
        if (std_match(instruction, OpNEG))   then
            -- Values used by status
            sel <= "110";

            -- Values used by ALU
            immed    <= "11111111";     -- a XOR 1 inverts a
            ENALU    <= "11";           -- Use XOR
            ENImmed  <= '0';            -- Use immediate
            ENInvRes <= '0';            -- Adds one to output of XOR

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Bitwise OR
        if (std_match(instruction, OpOR))    then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND for !(!a AND !b)
            ENInvOp  <= '0';            -- Inverting both inputs and outputs
            ENInvRes <= '0';            -- Inverting both inputs and outputs

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Bitwise OR with immediate
        if (std_match(instruction, OpORI))   then
            -- Values used by ALU
            ENALU    <= "10";           -- a OR b = !(!a AND !b)
            ENInvOp  <= '0';            -- Usually not inverting ALU operands
            ENInvRes <= '0';            -- Usually not inverting ALU outputs

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            immed <= instruction(11 downto 8) & instruction(3 downto 0);
            ENImmed  <= '0';            -- Instruction uses immediate

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        -- Rotate right through carry
        if (std_match(instruction, OpROR))   then
            -- Values used by status
            sel <= "001";

            -- Values used by ALU
            ENALU    <= "01";           -- Shifter
            ENCarry  <= '0';            -- Rotating through carry

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Subtract with carry
        if (std_match(instruction, OpSBC))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert operand to add a negative
            ENInvRes <= '0';            -- Invert result to finish two's comp
            ENCarry  <= '0';            -- Using carry flag

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Subtract immediate with carry
        if (std_match(instruction, OpSBCI))  then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert operand to add a negative
            ENInvRes <= '0';            -- Invert result to finish two's comp
            ENCarry  <= '0';            -- Using carry flag

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            immed <= instruction(11 downto 8) & instruction(3 downto 0);
            ENImmed  <= '0';            -- Subtract immediate

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Subtract immediate from word
        if (std_match(instruction, OpSBIW))  then
            -- Like ADIW, we use the second clock to subtract 0 with carry to
            -- propagate the carry bit through the high byte
            -- Values used by status

            -- This is only two clocks, so we always set counter to 1 because either
            -- we want to switch to 1 or it no longer matters
            numClks <= 1;

            -- Values used by ALU
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Writing low byte (first) to regSelA and second byte to regSelA + 1
            -- Rd in bits 5-4, K in bits 7-6, 3-0
            case clkCnt is
                when 0 =>
                    -- On first clock:
                    --      immed: Add value from instruction
                    --      regSelA: From instruction
                    --      ENInvOp: We add two's complement on low byte
                    --      ENInvRes: Finish two's complement
                    immed <= "00" & instruction(7 downto 6) & instruction(3 downto 0);
                    regSelA <= "000" & instruction(5 downto 4);
                    ENInvOp <= '0';
                    ENInvRes <= '0';
                when others =>
                    -- On second clock:
                    --      ENCarry: Use carry from low byte add
                    --      immed: Add 0 with carry to propagate add to high byte
                    --      regSelA: One greater than from instruction
                    ENCarry <= '0';
                    immed <= "00000000";
                    regSelA <= "00" & std_logic_vector(to_unsigned(
                        conv_integer(instruction(5 downto 4)) + 1, 3
                    ));
            end case;

            sel <= "011";

            -- Always reading from register A
            ENRegA <= '0';

            -- We both read from and write to registers on both clocks
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        -- Subtract without carry
        if (std_match(instruction, OpSUB))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert to add a negative
            ENInvRes <= '0';            -- Add one to output to finish two's comp

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Subtract immediate without carry
        if (std_match(instruction, OpSUBI))  then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert to add a negative
            ENInvRes <= '0';            -- Add one to output to finish two's comp

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            immed <= instruction(11 downto 8) & instruction(3 downto 0);
            ENImmed  <= '0';            -- Using immediate

            -- We both read from and write to registers in this instruction
            ENRegA <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpSWAP))  then
            -- Tell registers to SWAP
            ENSwap <= '0';

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- Affects no flags
        end if;

    end process decode;

end architecture;
