-----------------------------------------------------------------------------------------
--
-- Atmel AVR CPU Entity Declaration
--
-- This is the entity declaration for the complete AVR CPU.  The design
-- should implement this entity to make testing possible.
--
-- Entities:
--      Instruction:
--          Reads from program memory the instruction, incrementing the instruction
--          pointer when no explicit next IP is given.  This utilizes the adder
--          in the memory unit.
--      ControlUnit:
--          Sends control signals to the rest of the system based on the system state
--          and instruction.
--      ALU:
--          Performs all arithmetic operations.  This performs any possible operation
--          all the time and relies on control signals to produce the correct outputs
--      Status:
--          Contains the status register and all of the logic to set bits according
--          to the inputs and outputs of an operation.  To do this, it uses a flag
--          mask that indicates which flags should be affected.
--      Registers:
--          Contains all of the general purpose registers.  This also performs operations
--          that are register-specific, including SWAP and BLD.
--      MemoryUnit:
--          Sends control signals to data memory.  This contains an adder for its
--          addressing that is utilized by the instruction component when applicable,
--          which is mainly JMP and CALL instructions.
--      Stack:
--          This contains the stack pointer and allows for it to be updated.
--
-- Revision History:
--     11 May 98  Glen George       Initial revision.
--      9 May 00  Glen George       Updated comments.
--      7 May 02  Glen George       Updated comments.
--     21 Jan 08  Glen George       Updated comments.
--
-----------------------------------------------------------------------------------------


--
--  AVR_CPU
--
--  This is the complete entity declaration for the AVR CPU.  It is used to
--  test the complete design.
--
--  Inputs:
--    ProgDB - program memory data bus (16 bits)
--    Reset  - active low reset signal
--    INT0   - active low interrupt
--    INT1   - active low interrupt
--    clock  - the system clock
--
--  Outputs:
--    ProgAB - program memory address bus (16 bits)
--    DataAB - data memory address bus (16 bits)
--    DataWr - data write signal
--    DataRd - data read signal
--
--  Inputs/Outputs:
--    DataDB - data memory data bus (8 bits)
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.numeric_std.all;

library opcodes;
use opcodes.opcodes.all;

library common;
use common.common.all;


entity  AVR_CPU  is

    port (
        ProgDB  :  in     std_logic_vector(15 downto 0);   -- program memory data bus
        Reset   :  in     std_logic;                       -- reset signal (active low)
        INT0    :  in     std_logic;                       -- interrupt signal (active low)
        INT1    :  in     std_logic;                       -- interrupt signal (active low)
        clock   :  in     std_logic;                       -- system clock
        ProgAB  :  out    std_logic_vector(15 downto 0);   -- program memory address bus
        DataAB  :  out    std_logic_vector(15 downto 0);   -- data memory address bus
        DataWr  :  out    std_logic;                       -- data memory write enable (active low)
        DataRd  :  out    std_logic;                       -- data memory read enable (active low)
        DataDB  :  inout  std_logic_vector(7 downto 0)     -- data memory data bus
    );

end  AVR_CPU;

architecture testAVR of AVR_CPU is

    -------------------------------------------------------------------------------------
    --
    -- COMPONENTS USED
    --
    -------------------------------------------------------------------------------------

    --
    -- Instruction component
    --
    -- Defines Instruction Register and Instruction Pointer and program DB
    --
    component Instruction is
        port (
            clk         : in  std_logic;        -- System clock
            clkIdx      : in  clockIndex_t;     -- Num clocks since instruction start

            fetch       : in  std_logic;        -- Get new instruction on rising edges

            ROMIn       : in  address_t;        -- Input read from ROM
            memIn       : in  address_t;        -- Address from memory
            regIn       : in  address_t;        -- Address from registers
            IPSel       : in  IPSelector_t;     -- Selects the source of the next IP
            memInByte   : in  data_t;           -- A byte off of the stack

            instruction : out instruction_t;    -- Instruction to execute
            nextIP      : out address_t;        -- The next IP, usually incremented IP
            ProgAB      : out address_t         -- Address to read in ROM / IP
        );
    end component;

    --
    -- ControlUnit component
    --
    -- The control unit sends control signals to the rest of the system based on the
    -- instruction and current state of the system
    --
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

            -- Status control
            sel         : out flagSelector_t;   -- selects flag index
            flagMask    : out status_t;         -- status bits affected
            clkIdx      : out clockIndex_t;     -- clock counter
            ENRes       : out std_logic;        -- set SREG to R

            -- ALU control
            immed       : out immediate_t;      -- immediate value
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
            SPWr        : out std_logic;        -- write to stack ptr

            -- Instruction register and program counter control
            IPSel       : out IPSelector_t;     -- Tells which IP source is next IP
            fetch       : out std_logic         -- Tells us when to fetch instruction
        );
    end component;

    --
    -- ALU component
    --
    -- Defines the inputs and outputs for the ALU of the Atmel AVR emulator
    --
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

    --
    -- Status component
    --
    -- Defines the status register
    --
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

            BST         : in  std_logic;        -- 1 when BST instruction
            CPC         : in  std_logic;        -- 1 when CPC instruction
            sel         : in  flagSelector_t;   -- selects flag index
            mask        : in  status_t;         -- masks unaffected flags
            clkIdx      : in  clockIndex_t;     -- clocks since instrctn
            ENRes       : in  std_logic;        -- set SREG to R

            Eq          : out std_logic;        -- '0' when R = 0
            SREG        : out status_t          -- status register
        );
    end component;

    --
    -- Registers component
    --
    -- Defines the inputs and outputs for the general registers the Atmel AVR
    -- emulator
    --
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

    --
    -- MemoryUnit component
    --
    -- Defines the inputs and outputs for the memory unit of the Atmel AVR
    -- emulator
    --
    component MemoryUnit is
        port (
            clk         : in  std_logic;        -- system clock
            clkIdx      : in  clockIndex_t;     -- number of clocks passed

            regAddr     : in  address_t;        -- address from registers
            SPAddr      : in  address_t;        -- address from stack ptr
            IPAddr      : in  address_t;        -- address from instruction
            ProgAddr    : in  address_t;        -- address from program data bus
            immed       : in  immediate_t;      -- memory address offset
            decrement   : in  std_logic;        -- when low, decrement

            addrSel     : in  addrSelector_t;   -- chooses which address
            RW          : in  std_logic;        -- read/not write
            addBefore   : in  std_logic;        -- when low, add offset before output
            dataIn      : in  data_t;           -- input data
            addrOut     : out address_t;        -- address after inc/dec

            DataAB      : out address_t;        -- address to memory
            DataDB      : inout data_t;         -- data bus in and out
            DataRd      : out std_logic;        -- read signal to memory
            DataWr      : out std_logic         -- write signal to memory
        );
    end component;

    --
    -- Stack
    --
    -- Defines the status register
    --
    component Stack is
        port (
            clk         : in  std_logic;        -- system clock
            reset       : in  std_logic;        -- active low reset

            dataIn      : in  address_t;        -- new stack pointer
            ENWr        : in  std_logic;        -- when low, look at data

            SP          : out address_t         -- stack pointer
        );
    end component;

    -------------------------------------------------------------------------------------
    --
    -- SIGNALS REQUIRED
    --
    -------------------------------------------------------------------------------------

    -- General signals
    signal clkIdx       : clockIndex_t      := 0; -- Num clocks since instruction
    signal IR           : instruction_t     := "0000000000000000"; -- Instruction register
    signal immed        : immediate_t       := "000000000000"; -- Immediate value
    signal SREG         : status_t          := "00000000"; -- Status register

    -- Control signals
    signal IPSel        : IPSelector_t      := "000"; -- Selects next IP source
    signal BLD          : std_logic         := '0'; -- Active for BLD instruction
    signal BST          : std_logic         := '0'; -- Active for BST instruction
    signal CPC          : std_logic         := '0'; -- Active for CPC instruction
    signal flagSel      : flagSelector_t    := "000"; -- Selects bit in flags
    signal flagMask     : status_t          := "00000000"; -- Masks unaffected flags
    signal ENRes        : std_logic         := '0'; -- Enables ALU to set flags
    signal ENALU        : ALUSelector_t     := "00"; -- Selects ALU output
    signal ENImmed      : std_logic         := '0'; -- Enables ALU to use immediate
    signal ENCarry      : std_logic         := '0'; -- Enables ALU to respect carry flag
    signal ENInvOp      : std_logic         := '0'; -- Invert operands in ALU
    signal ENInvRes     : std_logic         := '0'; -- Invert ALU result
    signal ENMul        : std_logic         := '0'; -- Active for MUL instruction
    signal ENSwap       : std_logic         := '0'; -- SWAP instruction
    signal ENRegA       : std_logic         := '0'; -- enable register A
    signal ENRegB       : std_logic         := '0'; -- enable register B
    signal ENRegWr      : std_logic         := '0'; -- enable register writing
    signal regInSel     : regInSelector_t   := "00"; -- Selects register input source
    signal wordRegSel   : wordSelector_t    := "000"; -- Selects word register
    signal memRW        : std_logic         := '0'; -- Read/write to data memory
    signal addBefore    : std_logic         := '0'; -- Tells mem to perform add first
    signal decrement    : std_logic         := '0'; -- Tells mem to decrement
    signal SPWr         : std_logic         := '0'; -- Write signal for stack pointer

    -- Instruction register signals
    signal fetch        : std_logic         := '0'; -- Fetch new instruction on '1'
    signal IPOut        : address_t         := "0000000000000000"; -- IP on next fetch

    -- ALU signals
    signal Rd0          : std_logic         := '0'; -- bit 0 of operand A in ALU
    signal Rd3          : std_logic         := '0'; -- bit 3 of operand A in ALU
    signal Rr3          : std_logic         := '0'; -- bit 3 of operand B in ALU
    signal Rd7          : std_logic         := '0'; -- bit 7 of operand A in ALU
    signal Rr7          : std_logic         := '0'; -- bit 7 of operand B in ALU
    signal ALUResult    : data_t            := "00000000"; -- Result from ALU computation

    -- General register signals
    signal regWordOut   : dataWord_t        := "0000000000000000"; -- X Y or Z reg
    signal Rdb          : std_logic         := '0'; -- b'th bit of register A
    signal Eq           : std_logic         := '0'; -- '0' when regA = regB
    signal regSelA      : regSelector_t     := "00000"; -- Selects register A index
    signal regSelB      : regSelector_t     := "00000"; -- Selects register B index
    signal regOutA      : data_t            := "00000000"; -- Output from register A
    signal regOutB      : data_t            := "00000000"; -- Output from register B

    -- Data memory signals
    signal memAddrOut   : address_t         := "0000000000000000"; -- Out from data mem
    signal memAddrSel   : addrSelector_t    := "00"; -- Selects input source for memory

    -- Stack pointer signals
    signal SP           : address_t         := "0000000000000000"; -- Stack pointer

begin

    InstructionComponent    : Instruction   port map (
        clock,              -- IN       System clock
        clkIdx,             --          Num clocks since instruction start
        fetch,              --          Fetch new instruction
        ProgDB,             -- IN       Data from program memory
        memAddrOut,         --          Address from memory adder
        regWordOut,         --          Data word from registers (always Z here)
        IPSel,              --          Selects IP source
        DataDB,             -- INOUT    Data read off stack (from memory)
        IR,                 --          Instruction register contents
        IPOut,              --          New IP address on next fetch
        ProgAB              -- OUT      Address bus to program memory
    );

    ControlComponent        : ControlUnit   port map (
        clock,              -- IN       System clock
        IR,                 --          Instruction register contents
        SREG,               --          Status register
        Rdb,                --          b'th bit of register A
        Eq,                 --          '0' when register outs are equal
        BLD,                --          '1' when BLD instruction occurring
        BST,                --          '1' when BST instruction occurring
        CPC,                --          '1' when CPC instruction occurring
        flagSel,            --          Selects bit in status register
        flagMask,           --          Masks unaffected flags
        clkIdx,             --          Num clocks since instruction start
        ENRes,              --          Allows ALU to set flags
        immed,              --          Immediate value from instruction
        ENALU,              --          Selects ALU output
        ENImmed,            --          Enables ALU to use immediate
        ENCarry,            --          Tells ALU to respect carry flag
        ENInvOp,            --          Invert operands in ALU
        ENInvRes,           --          Invert ALU result
        regSelA,            --          Selects register A
        regSelB,            --          Selects register B
        ENMul,              --          Active for MUL instruction
        ENSwap,             --          Active for SWAP instruction
        ENRegA,             --          Enables writing to register A
        ENRegB,             --          Enables writing to register B
        ENRegWr,            --          Enables writing to registers at all
        regInSel,           --          Selects data input source for register
        wordRegSel,         --          Selects word register
        memRW,              --          Read/write to data memory
        memAddrSel,         --          Selects memory input source
        addBefore,          --          Tells memory to add before mem access
        decrement,          --          Tells memory to decrement instead of add
        SPWr,               --          Write signal for stack pointer
        IPSel,              --          Selects IP source
        fetch               --          Fetch new instruction
    );

    ALUComponent            : ALU           port map (
        regOutA,            --          Output from register A
        regOutB,            --          Output from register B
        immed(7 downto 0),  --          Immediate value from instruction
        SREG,               --          Status register
        ENALU,              --          Selects ALU output
        ENCarry,            --          Tells ALU to respect carry flag
        ENImmed,            --          Enables ALU to use immediate
        ENInvOp,            --          Invert operands in ALU
        ENInvRes,           --          Invert ALU result
        ENMul,              --          Active for MUL instruction
        clkIdx,             --          Num clocks since instruction start
        Rd0,                --          bit 0 of operand A in ALU
        Rd3,                --          bit 3 of operand A in ALU
        Rr3,                --          bit 3 of operand B in ALU
        Rd7,                --          bit 7 of operand A in ALU
        Rr7,                --          bit 7 of operand B in ALU
        ALUResult           --          Result from ALU computation
    );

    StatusComponent         : Status        port map (
        clock,              -- IN       System clock
        ALUResult,          --          Result from ALU computation
        Rd0,                --          bit 0 of operand A in ALU
        Rd3,                --          bit 3 of operand A in ALU
        Rr3,                --          bit 3 of operand B in ALU
        Rd7,                --          bit 7 of operand A in ALU
        Rr7,                --          bit 7 of operand B in ALU
        Rdb,                --          b'th bit of register A, for T flag
        BST,                --          '1' when BST instruction occurring
        CPC,                --          '1' when CPC instruction occurring
        flagSel,            --          Selects bit in status register (for BST)
        flagMask,           --          Masks unaffected flags
        clkIdx,             --          Num clocks since instruction start
        ENRes,              --          Allows ALU to set flags
        Eq,                 --          '0' when register outs are equal
        SREG                --          Status register
    );

    RegisterComponent       : Registers     port map (
        clock,              -- IN       System clock
        clkIdx,             --          Num clocks since instruction start
        ALUResult,          --          Output from ALU computation
        DataDB,             -- INOUT    Data byte from data memory
        immed(7 downto 0),  --          Immediate value from instruction
        regInSel,           --          Selects data input source for register
        memAddrOut,         --          Address from memory adder
        wordRegSel,         --          Selects word register
        BLD,                --          '1' when BLD instruction occurring
        flagSel,            --          Selects bit in status register (for BLD)
        SREG(6),            --          T flag
        regSelA,            --          Selects register A
        regSelB,            --          Selects register B
        ENMul,              --          Active for MUL instruction
        ENSwap,             --          Active for SWAP instruction
        ENRegA,             --          Enables writing to register A
        ENRegB,             --          Enables writing to register B
        ENRegWr,            --          Enables writing to registers at all
        Rdb,                --          b'th bit of register A
        regOutA,            --          Output from register A
        regOutB,            --          Output from register B
        regWordOut          --          Data word from registers
    );

    MemoryComponent         : MemoryUnit    port map (
        clock,              -- IN       System clock
        clkIdx,             --          Num clocks since instruction start

        regWordOut,         --          Data word from registers
        SP,                 --          Stack pointer
        IPOut,              --          New IP address on next fetch
        ProgDB,             -- IN       Data from program memory
        immed,              --          Immediate value from instruction
        decrement,          --          Tells memory to decrement instead of add
        memAddrSel,         --          Selects memory input source
        memRW,              --          Read/write to data memory
        addBefore,          --          Tells memory to add before mem access
        regOutA,            --          Data output to memory from registers
        memAddrOut,         --          Address from memory adder

        DataAB,             -- OUT      Address to memory
        DataDB,             -- INOUT    Data byte from data memory
        DataRd,             -- OUT      Read signal to memory
        DataWr              -- OUT      Write signal to memory
    );

    StackComponent          : Stack         port map (
        clock,              -- IN       System clock
        Reset,              -- IN       System reset
        memAddrOut,         --          Address from memory adder
        SPWr,               --          Write signal for stack pointer
        SP                  --          Stack pointer
    );

end architecture;
