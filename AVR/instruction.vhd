

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

        ROMIn       : in  instruction_t;    -- Input read from ROM
        memInHigh   : in  data_t;           -- High byte of address from memory
        memInLow    : in  data_t;           -- Low byte of address from memory
        addrSel     : in  addrSelector_t;   -- Selects the source of the next IP

        instruction : out instruction_t;    -- Instruction to execute
        ProgAB      : out address_t;        -- Address to read in ROM
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

    -- This tells us we are using register.  We need this so that instruction can
    -- change when ROM input comes but also retain registered value if
    -- ROM input is not a valid instruction
    signal useReg   : std_logic     := '0';

    -- Latches a byte from memory when we need a two-byte value
    signal memInReg : data_t        := "00000000";

    -- This contains the next value of the instruction pointer.  It will usually
    -- be the incremented value, but may sometimes be set from a JMP CALL or RET
    signal nextIP   : address_t     := "0000000000000000";

begin

    -- Increment the instruction pointer to get the next one.
    IncrementIP : NBitIncrementer port map (IP, IPInc);

    -- Always output IP as the program address bus
    ProgAB <= IP;

    -- We output ROMIn as instruction unless a control signal has told us to latch
    -- the instruction, in which case we read from the latch.
    instruction <= ROMIn when useReg = '0' else IR;

    -- We always output the ROM value on the program data bus.  It is up to the
    -- readers to decide if they want to use it
    ProgDB <= ROMIn;

    -- Load the next IP address
    with addrSel select nextIP <=
        ROMIn                   when "01",
        "00000000" & memInLow   when "10",
        memInHigh  & memInReg   when "11",
        IPInc                   when others;

    -- This process handles the instruction regiser and program counter/instruction
    -- pointer.
    fetchInst: process (fetch) is
    begin
        -- Fetch the next instruction by loading the incremented IP into IP
        if (rising_edge(fetch)) then
            -- Assume we don't use register.
            useReg <= '0';

            -- Update the instruction pointer
            IP <= nextIP;

            -- If the fetch occurs on the first clock, then we need to latch
            -- the instruction because the next value will be a memory address
            if (clkIdx = 0) then
                IR <= ROMIn;
                useReg <= '1';
            end if;
        end if;
    end process;

    -- This process latches a byte from memory, for use when the instruction pointer
    -- should be changed to a value from memory, which comes in two byte-long
    -- parts
    latchMemIn: process (clk) is
    begin
        -- Latch when addrSel indicates it is active
        if (addrSel = "11") then
            memInReg <= memInLow;
        end if;
    end process;

end architecture;
