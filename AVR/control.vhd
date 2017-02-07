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
--      ENReg01 : std_logic
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
end ControlUnit;

--
-- architecture decoder of ControlUnit
--
-- Decodes the instruction and sends control signals
--
architecture decoder of ControlUnit is
    -- Register the byte output for internal use
    signal byteReg : std_logic;

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

    decode: process (clk, instruction) is
    begin

        ---------------------------------------------------------------------------------
        --
        -- General control signal default values
        --
        ---------------------------------------------------------------------------------
        -- The instruction is always immediately after a clock.  So we can obviously
        -- call the first byte the start of the instruction, but know that we have
        -- almost an entire clock to compute.  By the time the  next clock comes, we
        -- should be working on the next byte, unless it is a one-byte instruction
        -- in which case the value will be ignored altogether.
        byte <= byteReg;

        BLD      <= '0';            -- Not currently in BLD instruction
        sel      <= "000";          -- Most common value
        flagMask <= "11111111";     -- Change no flags by default
        ENRes    <= '1';            -- Rare that ALU sets SREG

        immed    <= "00000001";     -- Arbitrary, for INC instructions
        ENALU    <= "00";           -- Arbitrary
        ENImmed  <= '1';            -- Usually not using immediate value
        ENCarry  <= '1';            -- Arbitrary
        ENInvOp  <= '1';            -- Usually not inverting ALU operands
        ENInvRes <= '1';            -- Usually not inverting ALU outputs

        regSelA  <= "00000";        -- Arbitrary
        regSelB  <= "00000";        -- Arbitrary
        ENRegA   <= '1';            -- By default, don't propagate data to output
        ENRegB   <= '1';            -- By default, don't propagate data to output
        ENReg01  <= '1';            -- Only MUL enables this
        ENSwap   <= '1';            -- Only SWAP enables this
        ENRegRd  <= '1';            -- Don't access registers by default
        ENRegWr  <= '1';            -- Don't access registers by default

        ---------------------------------------------------------------------------------
        --
        -- ALU instruction decoding
        --
        ---------------------------------------------------------------------------------

        -- Check each individual opcode and update outputs accordingly.  Note each
        -- is unique, so we never have conflicting results and can do if's.  We only
        -- explicilty set values that differ from the default
        if (std_match(instruction, OpADC))   then
            byteReg <= '0';

            -- Set values used by ALU
            ENCarry  <= '0';            -- Arbitrary

            -- Set values used by registers
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';
            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpADD))   then
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpADIW))  then
            -- We will do two adds.  On the low byte, we add the immediate byte without
            -- carry.  We store the carry bit, and on the high byte, we add immediate
            -- again (now changed to 0) with carry to propagate that bit to the top
            -- bit if  necessary.

            -- Values used by ALU
            ENImmed  <= '0';            -- Usually not using immediate value

            -- No carry on first clock, carry on second
            ENCarry  <= not byteReg;

            -- Immediate is from source on first clock.  Second clock is just to
            -- propagate carry, but the high byte is 0
            case byteReg is
                when '0' =>
                    immed <= "00" & instruction(7 downto 6) & instruction(3 downto 0);
                when others => immed <= "00000000";
            end case;

            -- Writing low byte (first) to regSelA and second byte to regSelA + 1
            -- Rd in bits 5-4, K in bits 7-6, 3-0
            regSelA <= "000" & instruction(5 downto 4);
            regSelA <= std_logic_vector(to_unsigned(
                conv_integer(instruction(5 downto 4)) + 1, regSelA'length
            ));
            ENRegA <= byteReg;
            ENRegB <= not byteReg;

            -- We both read from and write to registers in this instruction
            ENRegRd <= byteReg;
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpAND))   then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpANDI))  then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            ENRegA <= '0';

            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpASR))   then
            -- Values used by status
            sel <= "001";

            -- Values used by ALU
            ENALU    <= "01";           -- Use shifter

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpBCLR))  then
            -- We will set mask to set only desired bit, and trick ALU into returning
            -- all 0's by making it think it is ANDI with "00000000".  Status will
            -- then clear the empty bit in the mask
            -- Values used by status
            ENRes <= '0';               -- ALU setting SREG
            flagMask(conv_integer(instruction(6 downto 4))) <= '0';

            -- Values used by ALU
            ENALU    <= "10";           -- ANDing
            ENImmed  <= '0';            -- Using immediate value (default 0)

            -- Flags handled by status enabled
        end if;

        if (std_match(instruction, OpBLD))   then
            -- Values used by ALU
            BLD <= '1';
            sel <= instruction(2 downto 0);

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- Affects no flags
        end if;

        if (std_match(instruction, OpBSET))  then
            -- Trick ALU into sending all 1's to status, from which mask will take
            -- the one we care about
            -- Values used by status
            ENRes <= '0';               -- Setting bit
            flagMask(conv_integer(instruction(6 downto 4))) <= '0';

            -- Values used by ALU
            immed    <= "11111111";     -- OR with operand to get 1's
            ENALU    <= "10";           -- Want to AND
            ENImmed  <= '0';            -- Use immediate
            ENInvOp  <= '0';            -- Invert inputs to compute !(!a AND !b)
            ENInvRes <= '0';            -- Invert outputs to compute !(!a AND !b)

            -- Flags handled by status enabled
        end if;

        if (std_match(instruction, OpBST))   then
            -- Select bit b
            sel <= instruction(2 downto 0);

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);

            -- Affects T flag
            flagMask <= "10111111";
        end if;

        if (std_match(instruction, OpCOM))   then
            -- Values used by status
            sel <= "010";

            -- Values used by ALU
            immed    <= "11111111";     -- a XOR 1 always inverts a
            ENALU    <= "11";           -- Use XOR
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpCP))    then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Invert to add a negative
            ENInvRes <= '1';            -- Usually not inverting ALU outputs

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We only read registers on this
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpCPC))   then
            -- Values used by status
            sel <= "011";               -- Most common value

            -- Values used by ALU
            ENCarry  <= '0';            -- Using carry flag
            ENInvOp  <= '0';            -- Invert operand to add a negative

            -- Values used by registers
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We only read registers on compare
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpCPI))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENImmed  <= '0';            -- Usually not using immediate value
            ENInvOp  <= '0';            -- Usually not inverting ALU operands

            -- Values used by registers
            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            ENRegA <= '0';

            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We only read on compares
            ENRegRd <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpDEC))   then
            -- Trick machine into subtracting 1 (adding -1)

            -- Values used by ALU
            immed    <= "00000001";     -- Subracting 1
            ENImmed  <= '0';            -- Use immediate
            ENInvOp  <= '0';            -- Invert operand for adding negative

            -- Values used by registers
            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpEOR))   then
            -- Values used by status
            sel <= "010";               -- Most common value

            -- Values used by ALU
            ENALU    <= "11";           -- Use XOR

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpINC))   then
            -- Values used by status
            sel <= "101";               -- Most common value

            -- Values used by ALU
            immed    <= "00000001";     -- 1 for INC adder
            ENImmed  <= '0';            -- Usually not using immediate value

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpLSR))   then
            ENALU <= "01"; -- use shifter

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpMUL))   then
            -- Values used by status
            sel <= "000";               -- Most common value
            ENRes <= '1';               -- Rare that ALU sets SREG

            -- Values used by ALU
            immed    <= "00000001";     -- Arbitrary, for INC instructions
            ENALU    <= "00";           -- Arbitrary
            ENImmed  <= '1';            -- Usually not using immediate value
            ENCarry  <= '1';            -- Arbitrary
            ENInvOp  <= '1';            -- Usually not inverting ALU operands
            ENInvRes <= '1';            -- Usually not inverting ALU outputs

            -- Values used by registers
            regSelA  <= "00000";        -- Arbitrary
            regSelB  <= "00000";        -- Arbitrary
            ENRegA   <= '1';            -- By default, don't propagate data to output
            ENRegB   <= '1';            -- By default, don't propagate data to output
            ENReg01  <= '1';            -- Only MUL enables this
            ENRegRd  <= '1';            -- Don't access registers by default
            ENRegWr  <= '1';            -- Don't access registers by default
            -- Enable ALU and keep status disabled

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '1';

            -- Affects C Z flags
            flagMask <= "11111100";
        end if;

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
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpOR))    then
            -- Values used by ALU
            ENALU    <= "10";           -- Use AND for !(!a AND !b)
            ENInvOp  <= '0';            -- Inverting both inputs and outputs
            ENInvRes <= '0';            -- Inverting both inputs and outputs

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpORI))   then
            -- Values used by ALU
            ENALU    <= "10";           -- a OR b = !(!a AND !b)
            ENImmed  <= '0';            -- Instruction uses immediate
            ENInvOp  <= '0';            -- Usually not inverting ALU operands
            ENInvRes <= '0';            -- Usually not inverting ALU outputs

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            ENRegA <= '0';

            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects Z N V S flags
            flagMask <= "11100001";
        end if;

        if (std_match(instruction, OpROR))   then
            -- Values used by status
            sel <= "001";

            -- Values used by ALU
            ENALU    <= "01";           -- Shifter
            ENCarry  <= '0';            -- Rotating through carry

            -- Rd in bits 8-4
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpSBC))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENCarry  <= '0';            -- Subtract with carry
            ENInvOp  <= '0';            -- Invert one input to add a negative

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpSBCI))  then
            -- Values used by status
            sel <= "011";
            ENRes <= '1';               -- Rare that ALU sets SREG

            -- Values used by ALU
            ENImmed  <= '0';            -- Subtract immediate
            ENCarry  <= '0';            -- Subtract with carry
            ENInvOp  <= '0';            -- Negate operand to add a negative

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            ENRegA <= '0';

            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpSBIW))  then
            -- Like ADIW, we use the second clock to subtract 0 with carry to propagate
            -- the carry bit through the high byte
            -- Values used by status
            sel <= "011";               -- Most common value

            -- Values used by ALU
            ENImmed  <= '0';            -- Using immediate
            ENCarry  <= not byteReg;    -- Only carry on second byte (byte = '1')
            ENInvOp  <= '0';            -- Adding negative

            -- Rd in bits 5-4, K in bits 7-6, 3-0
            regSelA <= "000" & instruction(5 downto 4);
            regSelA <= std_logic_vector(to_unsigned(
                conv_integer(instruction(5 downto 4)) + 1, regSelA'length
            ));
            ENRegA <= byteReg;
            ENRegB <= not byteReg;

            case byteReg is
                when '0' =>
                    immed <= "00" & instruction(7 downto 6) & instruction(3 downto 0);
                when others => immed <= "00000000";
            end case;

            -- We both read from and write to registers in this instruction
            ENRegRd <= byteReg;
            ENRegWr <= '0';

            -- Affects C Z N V S flags
            flagMask <= "11100000";
        end if;

        if (std_match(instruction, OpSUB))   then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENInvOp  <= '0';            -- Negate operand to add a negative

            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            ENRegA <= '0';

            regSelB <= instruction(9) & instruction(3 downto 0);
            ENRegB <= '0';

            -- We both read from and write to registers in this instruction
            ENRegRd <= '0';
            ENRegWr <= '0';

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        if (std_match(instruction, OpSUBI))  then
            -- Values used by status
            sel <= "011";

            -- Values used by ALU
            ENImmed  <= '0';            -- Using immediate
            ENInvOp  <= '0';            -- Negate operand to add a negative

            -- Rd in bits 7-4, K in bits 11-8, 3-0
            regSelA <= "0" & instruction(7 downto 4);
            ENRegA <= '0';

            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- We both read from and write to registers in this instruction
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
            ENRegA <= '0';

            -- Affects no flags
        end if;

    end process decode;

end architecture;
