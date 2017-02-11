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
--      IP : std_logic_vector(15 downto 0)
--          The instruction pointer
--
-- Inputs:
--      clk : std_logic
--          The global clock
--      instruction : instruction_t
--          The instruction from memory that is to be decoded.
--
-- Outputs:
--      BLD : std_logic
--          '1' when BLD instruction occurring.
--      BST : std_logic
--          '1' when BST instruction occurring.
--      sel : flagSelector_t
--          Selects flag index and register index for bit setting
--      flagMask : status_t
--          A mask showing which bits in the status register should be affected by the
--          instruction.  Affected bits are represented by '0'
--      ENALU : ALUSelector_t
--          Describes type of operation.
--      regSelA : regSelector_t
--          Represents which of the 32 registers to use as register A.  For more info
--          on register A, refer to registers component documentation.
--      regSelB : regSelector_t
--          Represents which of the 32 registers to use as register B.  For more info
--          on register B, refer to registers component documentation.
--      immed : data_t
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
--      ENRegWr : std_logic
--          Enables (active low) writing to registers
--      sourceSel : regInSelector_t
--          Used to choose which data input to pay attention to when writing to
--          registers
--      wordReg : wordSelector_t
--          Used to choose which word register we are accessing, X Y or Z
--      memRW : std_logic
--          Read / not Write signal to memory, denotes if writing to actual memory
--      addrSel : addrSelector_t
--          Selects which address the memory unit should pay attention to
--      addBefore : std_logic
--          When low, tells the memory unit to add the offset to the address before
--          accessing data.
--      decrement : std_logic
--          When low, the offset should be a decrement.  We use this because it is
--          one bit instead of a 16-bit -1
--      SPWr : std_logic
--          Active low, indicates that the stack pointer should latch
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--      01 Feb 17  Tim Menninger     Added signals for ALU and registers
--      09 Feb 17  Tim Menninger     Implemented memory addressing unit
--
-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

library common;
use common.common.all;

--
-- entity ControlUnit
--
-- Defines the inputs and outputs for the control unit of the Atmel AVR
-- emulator
--
entity ControlUnit is
    port (
        clk         : in  std_logic;        -- system clock

        instruction : in  instruction_t;    -- instruction

        BLD         : out std_logic;        -- '1' when BLD
        BST         : out std_logic;        -- '1' when BST

        -- Status control
        sel         : out flagSelector_t;   -- selects flag index
        flagMask    : out status_t;         -- status bits affected
        clkIdx      : out clockIndex_t;     -- clock counter
        ENRes       : out std_logic;        -- set SREG to R

        -- ALU control
        immed       : out data_t;           -- immediate value
        ENALU       : out ALUSelector_t;    -- ALU operation type
        ENImmed     : out std_logic;        -- enable immed
        ENCarry     : out std_logic;        -- enable carry
        ENInvOp     : out std_logic;        -- negate operand in ALU
        ENInvRes    : out std_logic;        -- negate result in ALU

        -- Registers control
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
        SPWr        : out std_logic                     -- write to stack ptr
    );
end ControlUnit;

--
-- architecture decoder of ControlUnit
--
-- Decodes the instruction and sends control signals
--
architecture decoder of ControlUnit is

    -- How many clocks an instruction requires
    signal numClks  : clockIndex_t  := 0;

    -- How many clocks since instruction began
    signal clkCnt   : clockIndex_t  := 0;

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

    --
    -- decode process
    --
    -- Decodes the signal and sends control signals accordingly
    --
    decode: process (instruction, clkCnt) is
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
        ENRegWr  <= '1';            -- Don't access registers by default
        sourceSel<= "00";           -- Choose data from ALU

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

            -- For this to work properly, the ALU has to put the contents of register
            -- A onto its result bus.  We will AND it with 1's
            immed <= "11111111";
            ENImmed <= '0';
            ENALU <= "10";

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

            -- Affects C Z N V S H flags
            flagMask <= "11000000";
        end if;

        -- Decrement
        if (std_match(instruction, OpDEC))   then
            -- Trick machine into subtracting 1 (adding -1)
            sel <= "101";

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
                    ENInvOp <= '0';
                    ENInvRes <= '0';
                    immed <= "00000000";
                    regSelA <= "00" & std_logic_vector(to_unsigned(
                        conv_integer(instruction(5 downto 4)) + 1, 3
                    ));
            end case;

            sel <= "011";

            -- Always reading from register A
            ENRegA <= '0';

            -- We both read from and write to registers on both clocks
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

        -----------------------------------------------------------------------------
        --
        -- Load data instruction decoding
        --
        -----------------------------------------------------------------------------

        -- Data memory control
        decrement <= '1';               -- don't decrement
        memRW <= '1';                   -- arbitrary, read/write to memory
        addrSel <= "00";                -- address mux: 0 = none, 1 = regs
                                        --              2 = SP, 3 = instruction
        wordReg <= "000";               -- 1 = X, 2 = Y, 3 = Z, 0 = none
                                        -- bit 2 is 1 when writing new addr
        addBefore <= '0';               -- assume the address we want includes
                                        -- the addition of immed (which is 0
                                        -- by default)

        -- Stack pointer control
        SPWr <= '1';        -- active low write to stack ptr

        -- LD Rd, X
        if (std_match(instruction, OpLDX))  then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The destination register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select X word register and don't write new address to it after
            wordReg <= "001";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, X+
        if (std_match(instruction, OpLDXI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select X word register and write new address to it after
            wordReg <= "101";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, -X
        if (std_match(instruction, OpLDXD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select X word register and write new address to it after
            wordReg <= "101";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, Y+
        if (std_match(instruction, OpLDYI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select Y word register and write new addr to it after
            wordReg <= "110";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, -Y
        if (std_match(instruction, OpLDYD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select Y word register and write new addr to it after
            wordReg <= "110";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LDD Rd, Y + q
        if (std_match(instruction, OpLDDY)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- Use the immediate value as address offset (high bits are 0)
            immed(5 downto 0) <= instruction(13) & instruction(11 downto 10) &
                                 instruction(2 downto 0);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Select Y word register and don't write new addr to it after
            wordReg <= "010";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, Z+
        if (std_match(instruction, OpLDZI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select Z word register and write new addr to it after
            wordReg <= "111";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LD Rd, -Z
        if (std_match(instruction, OpLDZD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Select Z word register and write new addr to it after
            wordReg <= "111";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LDD Rd, Z + q
        if (std_match(instruction, OpLDDZ)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- Use the immediate value as address offset (high bits are 0)
            immed(5 downto 0) <= instruction(13) & instruction(11 downto 10) &
                                 instruction(2 downto 0);

            -- Writing to register on second clock
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Select Z word register and don't write new addr to it after
            wordReg <= "011";

            -- Using address from registers
            addrSel <= "01";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- LDI Rd, k
        if (std_match(instruction, OpLDI))  then
            -- The register is implied by bits 4-7
            regSelA <= "0" & instruction(7 downto 4);

            -- Writing to register A
            ENRegWr <= '0';
            ENRegA <= '0';

            -- Put k into immediate value
            immed <= instruction(11 downto 8) & instruction(3 downto 0);

            -- The data source for registers is from the immediate
            sourceSel <= "10";
        end if;

        -- LDS Rd, m
        if (std_match(instruction, OpLDS))  then
            -- This is a 3-clock instruction
            numClks <= 2;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on third clock
            if (clkCnt = 2) then
                ENRegWr <= '0';
                ENRegA <= '0';
            end if;

            -- Using address from instruction
            addrSel <= "11";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- MOV Rd, Rr
        if (std_match(instruction, OpMOV))  then
            -- Rd in bits 8-4, Rr in bits 9, 3-0
            regSelA <= instruction(8 downto 4);
            regSelB <= instruction(9) & instruction(3 downto 0);

            -- Send enable signals that will tell registers to move what's in register
            -- B into register A
            ENRegB <= '0';
            ENRegWr <= '0';
        end if;

        -- ST X, Rr
        if (std_match(instruction, OpSTX))  then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Select X word register and don't write to it after
            wordReg <= "001";

            -- Using address from registers
            addrSel <= "01";

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST X+, Rr
        if (std_match(instruction, OpSTXI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select X word register and write to it after
            wordReg <= "101";

            -- Using address from registers
            addrSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST -X, Rr
        if (std_match(instruction, OpSTXD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select X word register and write to it after
            wordReg <= "101";

            -- Using address from registers
            addrSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST Y+, Rr
        if (std_match(instruction, OpSTYI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select Y word register and write to it after
            wordReg <= "110";

            -- Using address from registers
            addrSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST -Y, Rr
        if (std_match(instruction, OpSTYD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select Y word register and write to it after
            wordReg <= "110";

            -- Using address from registers
            addrSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST Z+, Rr
        if (std_match(instruction, OpSTZI)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select Z word register and write new addr to it after
            wordReg <= "111";

            -- Using address from registers
            addrSel <= "01";

            -- Increment after
            immed(0) <= '1';
            addBefore <= '1';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- ST -Z, Rr
        if (std_match(instruction, OpSTZD)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing new value to word register
            if (clkCnt = 1) then
                ENRegWr <= '0';
            end if;

            -- Select Z word register and write new addr to it after
            wordReg <= "111";

            -- Using address from registers
            addrSel <= "01";

            -- Decrement before
            decrement <= '0';

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- STD Y + q, Rr
        if (std_match(instruction, OpSTDY)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- Use the immediate value as address offset (high bits are 0)
            immed(5 downto 0) <= instruction(13) & instruction(11 downto 10) &
                                 instruction(2 downto 0);

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Select Y word register and don't write to it after
            wordReg <= "010";

            -- Using address from registers
            addrSel <= "01";

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- STD Z + q, Rr
        if (std_match(instruction, OpSTDZ)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- Use the immediate value as address offset (high bits are 0)
            immed(5 downto 0) <= instruction(13) & instruction(11 downto 10) &
                                 instruction(2 downto 0);

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Select Z word register and don't write new addr to it after
            wordReg <= "011";

            -- Using address from registers
            addrSel <= "01";

            -- Writing to memory here.
            memRW <= '0';
        end if;

        -- STS m, Rr
        if (std_match(instruction, OpSTS))  then
            -- This is a 3-clock instruction
            numClks <= 2;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Using address from instruction
            addrSel <= "11";

            -- Writing to memory here.
            memRW <= '0';
        end if;


        -----------------------------------------------------------------------------
        --
        -- Push/pop instruction decoding
        --
        -----------------------------------------------------------------------------

        --  Push and Pop instructions
        -- POP Rd
        if (std_match(instruction, OpPOP))  then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing to register on second clock, as well as new stack pointer
            if (clkCnt = 1) then
                ENRegWr <= '0';
                ENRegA <= '0';

                SPWr <= '0';
            end if;

            -- Using address from stack pointer
            addrSel <= "10";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Increment before
            immed(0) <= '1';

            -- Reading from memory here.  Use default memRW <= '1'
        end if;

        -- PUSH Rd
        if (std_match(instruction, OpPUSH)) then
            -- This is a 2-clock instruction
            numClks <= 1;

            -- The register is implied by bits 4-8
            regSelA <= instruction(8 downto 4);

            -- Writing over stack pointer value
            if (clkCnt = 1) then
                SPWr <= '0';
            end if;

            -- Using memory from stack pointer
            addrSel <= "10";

            -- Tell registers to get data from memory
            sourceSel <= "01";

            -- Decrement after
            decrement <= '0';
            addBefore <= '1';

            -- Writing to stack here.
            memRW <= '0';
        end if;

    end process decode;

end architecture;
