

-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library common;
use common.common.all;

--
-- Instruction entity
--
-- Defines Instruction Register and Instruction Pointer and program DB
--
entity Instruction is
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
        ProgAB      : out address_t;        -- Address to read in ROM / IP
        ProgDB      : out address_t         -- Data output
    );
end Instruction;

--
-- Dataflow architecture
--
-- Controls the instruction pointer
--
architecture dataflow of Instruction is

    -- Use an incrementer to generate next IP
    component NBitIncrementer is
        generic (
            n       : natural := addrBits
        );
        port (
            a       : in  std_logic_vector(n-1 downto 0);
            result  : out std_logic_vector(n-1 downto 0)
        );
    end component;

    -- The instruction pointer and the next instruction pointer
    signal IP       : address_t     := "0000000000000000";
    signal IPInc    : address_t     := "0000000000000000";

    -- The instruction register
    signal IR       : instruction_t := "0000000000000000";

    -- This contains the next value of the instruction pointer.  It will usually
    -- be the incremented value, but may sometimes be set from a JMP CALL or RET
    signal nextIPReg: address_t     := "0000000000000000";

    -- When the IP comes from the stack (any RET call), it comes in bytes, so we
    -- register the low byte
    signal memInReg : data_t        := "00000000";

begin

    -- Increment the instruction pointer to get the next one.
    IncrementIP : NBitIncrementer port map (IP, IPInc);

    -- Always output IP as the program address bus
    ProgAB <= IP;

    -- We output ROMIn as instruction unless a control signal has told us to latch
    -- the instruction, in which case we read from the latch.
    instruction <= ROMIn when clkIdx = 0 else IR;

    -- We always output the ROM value on the program data bus.  It is up to the
    -- readers to decide if they want to use it
    ProgDB <= ROMIn;

    -- Load the next IP address
    with IPSel   select nextIPReg <=
        IPInc                when "000",
        IPInc( 7 downto 0) &
        IPInc(15 downto 8)   when "100",
        ROMIn                when "001",
        memIn                when "010",
        regIn                when "011",
        memInByte & memInReg when others;

    -- This process handles the instruction regiser and program counter/instruction
    -- pointer.
    fetchInst: process (clk) is
    begin
        -- Fetch the next instruction by loading the incremented (or explicit) IP into IP
        if (rising_edge(clk)) then
            -- If fetching, load the next IP into the instruction pointer
            if (fetch = '1') then
                -- Update the instruction pointer
                IP <= nextIPReg;
            end if;

            if (clkIdx = 0) then
                -- The first clock of an instruction always has the instruction; latch it
                IR <= ROMIn;
            end if;

            if (clkIdx = 1) then
                -- If this happens to be a RET call, we will latch the byte from stack
                -- on second clk.  Otherwise, this will ge ignored so do it anyway.
                memInReg <= memInByte;
            end if;

        end if;
    end process;

end architecture;
