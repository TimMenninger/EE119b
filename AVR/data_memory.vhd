-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-----------------------------------------------------------------------------------------
--
-- memory.vhd
--
-- This contains logic to read and write to memory.  When RW indicates a read, data
-- is asserted on the output.  Otherwise, it is high impedance while data from the
-- input is written.  When enable is inactive, no read or write occurs and the output
-- is on high impedance.
--
-- Inputs:
--
-- Outputs:
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity MemoryUnit
--
-- Defines the inputs and outputs for the memory unit of the Atmel AVR
-- emulator
--
entity MemoryUnit is
    port (
        clk         : in  std_logic;                        -- system clock
        clkIdx      : in  natural range 0 to 3;             -- number of clocks passed

        regAddr     : in  std_logic_vector(15 downto 0);    -- address from registers
        SPAddr      : in  std_logic_vector(15 downto 0);    -- address from stack ptr
        IRAddr      : in  std_logic_vector(15 downto 0);    -- address from instruction
        immed       : in  std_logic_vector(5 downto 0);     -- memory address offset
        decrement   : in  std_logic;                        -- when low, decrement

        addrSel     : in  std_logic_vector(1 downto 0);     -- chooses which address
        RW          : in  std_logic;                        -- read/not write
        addBefore   : in  std_logic;                        -- when low, add offset
                                                            -- to address before output
        dataIn      : in  std_logic_vector(7 downto 0);     -- input data
        addrOut     : out std_logic_vector(15 downto 0);    -- address after inc/dec

        DataAB      : out std_logic_vector(15 downto 0);    -- address to memory
        DataDB      : inout std_logic_vector(7 downto 0);   -- data bus in and out
        DataRd      : out std_logic;                        -- read signal to memory
        DataWr      : out std_logic                         -- write signal to memory
    );
end MemoryUnit;

--
-- architecture
--
-- Handles the memory addressing
--
architecture workflow of MemoryUnit is

    -- We need an adder component to add offsets to the addresses and to handle
    -- increments and decrements.  Related signals:
    --      address: The original address, muxed from inputs
    --      offset: The offset, either 1, -1, or from immed
    --      nextAddress: The address with offset
    signal address      : std_logic_vector(15 downto 0) := "0000000000000000";
    signal offset       : std_logic_vector(15 downto 0) := "0000000000000000";
    signal nextAddress  : std_logic_vector(15 downto 0) := "0000000000000000";
    component NBitAdder is
        generic (
            n       : natural := 16                         -- bits in adder
        );
        port (
            Cin     : in  std_logic;                        -- Cin
            a       : in  std_logic_vector(n-1 downto 0);   -- operand A
            b       : in  std_logic_vector(n-1 downto 0);   -- operant B

            Cout    : out std_logic;                        -- Cout
            result  : out std_logic_vector(n-1 downto 0)    -- sum of A and B
        );
    end component;

begin

    AddrAdder : NBitAdder
        port map ('0', address, offset, open, nextAddress);

    -- We use the input select signal to choose which address input we should be
    -- using
    with addrSel    select address <=
        regAddr                 when "01",
        SPAddr                  when "10",
        IRAddr                  when others;

    -- We have a decrement signal which tells us to add all 1s.  Otherwise,
    -- we can just use the immediate and fill the high bits with 0s
    with decrement  select offset <=
        "1111111111111111"      when '0',
        "0000000000" & immed    when others;

    -- When adding before, use the adder output for memory access.  Otherwise,
    -- use the muxed input
    with addBefore  select DataAB <=
        nextAddress             when '0',
        address                 when others;

    -- No matter what, we output the new address in case someone needs to update
    -- something somewhere
    addrOut <= nextAddress;

    -- Set the data inout to high impedance or pass data through
    with RW         select DataDB <=
        dataIn                  when '0',
        "ZZZZZZZZ"              when others;

    --
    -- control process
    --
    -- This looks at the clock index and the control signals and decides when and how
    -- to send control signals to memory.  When we are using an IR address, it is
    -- a three-clock instruction so we go on clkIdx = 2.  Otherwise, clkIdx = 1.
    -- Read and write signals only go active for half a clock cycle when it is
    -- low.
    --
    control : process (clk) is
    begin
        -- Default values
        DataRd <= '1';
        DataWr <= '1';

        if (clk = '0') then
            if ((clkIdx = 2 and addrSel = "11") or
                (clkIdx = 1 and (addrSel = "10" or addrSel = "01"))) then
                DataRd <= not RW;
                DataWr <= RW;
            end if;
        end if;
    end process;

end workflow;
