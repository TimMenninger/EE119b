-----------------------------------------------------------------------------------------
--
-- isntruction.vhd
--
-- This contains logic that controls the instruction register.  It is responsible for
-- updating the instruction pointer according to control signals and registering
-- the instruction, which is particularly important on two-word instructions.  It
-- has an incrementer that increments the IP, but any IP that is not simply incremented
-- must be input to the component.  An incremented IP is always output, too, because
-- other components in the system commonly need an incremented version (for example,
-- when CALL pushes, it pushes the incremented IP); no need to increment it twice in
-- parallel.
--
-- Inputs:
--      clk : std_logic
--          System clock
--      clkIdx : clockIndex_t
--          Num clocks since instruction start.  When clkIdx = 0 on the clock edge,
--          the instruction is registered in case the next instruction is a memory
--          address.
--      fetch : std_logic
--          When fetch is '1' on a rising clock edge, IP is overwritten with either
--          itself incremented or an input address, depending on control signals.
--      ROMIn : address_t
--          Input read from ROM.  This is the instruction at address (IP)
--      memIn : address_t
--          Address from memory.  An address would come from here because we utilize
--          the adder in data memory when possible.
--      regIn : address_t
--          The Z register from registers, used for indirect operations.
--      IPSel : IPSelector_t
--          Selects the source of the next IP address.  This can be the incremented IP,
--          ROM, data memory, register or stack.
--      memInByte : data_t
--          A byte off of the stack.  When RET is called, we must restore the IP from
--          the stack, which gives one byte at a time.
--
-- Outputs:
--      instruction : instruction_t
--          Instruction to execute.  This will either come from the instruction register
--          or directly from ROM, depending on the clock.
--      nextIP : address_t
--          The next IP, usually incremented IP.  The value contained here will be
--          loaded into IP on rising edges of the clock if fetch is active.
--      ProgAB : address_t
--          Address to read in from ROM.  This is the instruction pointer.
--
-- Revision History:
--      20 Feb 17  Tim Menninger    Created
--
-----------------------------------------------------------------------------------------

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
        reset       : in  std_logic;        -- System reset
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

    -- When the IP comes from the stack (any RET call), it comes in bytes, so we
    -- register the low byte
    signal memInLow  : data_t       := "00000000";
    signal memInHigh : data_t       := "00000000";

begin

    -- Increment the instruction pointer to get the next one.
    IncrementIP : NBitIncrementer port map (IP, IPInc);

    -- Always output IP as the program address bus
    ProgAB <= IP;

    -- We output ROMIn as instruction unless a control signal has told us to latch
    -- the instruction, in which case we read from the latch.
    instruction <= ROMIn when clkIdx = 0 else IR;

    -- Data uses incremented IP in adder, but when it is using it to push, we
    -- push in two bytes, so we swap the bytes
    nextIP <= "00000000" & IPInc(15 downto 8) when IPSel = "100" else IPInc;

    -- This process handles the instruction regiser and program counter/instruction
    -- pointer.
    fetchInst: process (clk, reset) is
    begin
        -- On reset, instruction pointer should go to 0, or start
        if (reset = '0') then
            IP <= "0000000000000000";
        -- Fetch the next instruction by loading the incremented (or explicit) IP into IP
        elsif (rising_edge(clk)) then
            -- If fetching, load the next IP into the instruction pointer
            if (fetch = '1') then
                -- Update the instruction pointer
                case IPSel is
                    when "000" =>
                        IP <= IPInc;
                    when "001" =>
                        IP <= ROMIn;
                    when "010" =>
                        IP <= memIn;
                    when "011" =>
                        IP <= regIn;
                    when "110" =>
                        IP <= memInHigh & memInLow;
                    when others =>
                        IP <= IP;
                end case;
            end if;

            if (clkIdx = 1) then
                -- If this happens to be a RET call, we will latch low byte from stack
                -- on second clk.  Otherwise, this will ge ignored so do it anyway.
                memInLow <= memInByte;
            end if;

            if (clkIdx = 2) then
                -- If this happens to be a RET call, we will latch the high byte from
                -- stack on third clk.  Otherwise, this will ge ignored so do it anyway.
                memInHigh <= memInByte;
            end if;

        end if;
    end process;

end architecture;
