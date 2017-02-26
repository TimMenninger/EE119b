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
--      clk : std_logic
--          System clock
--      regAddr : address_t
--          The address from the register unit, will be muxed
--      SPAddr : address_t
--          The address from the stack pointer, will be muxed
--      IPAddr : address_t
--          The address from the instruction, will be muxed
--      immed : data_t
--          An immediate value that will be added to the address either before
--          or after accessing memory
--      decrement : std_logic
--          When low, decrement the address instead of adding the immediate
--      addrSel : addrSelector_t
--          The selector for which address should be used to access memory
--      RW : std_logic
--          Read/not write to memory
--      addBefore : std_logic
--          When low, the immediate value or decrement should occur before the
--          memory access
--      dataIn : data_t
--          The data input to be written to memory if applicable.
--      DataDB : data_t
--          The data bus to memory.  Note that this is an INOUT and can be hi-Z
--      useIP : std_logic
--          When 1, we should use the IP address instead of dataIn
--
-- Outputs:
--      addrOut: address_t
--          The output address.  This is used by other entities when the address
--          source needs an updated address, e.g., the stack pointer
--      DataAB: address_t
--          The address bus to external memory
--      DataDB: data_t
--          The data bus to/form external memory
--      DataRd: out std_logic
--          Active low read signal to external memory.  This is to go low on the last
--          clock of an instruction that reads when the clock is low.
--      DataWr: out std_logic
--          Active low write signal to external memory.  This is to go low on the
--          low portion of the last clock of the instruction.
--
-- Revision History:
--      26 Jan 17  Tim Menninger    Entity declaration
--      07 Feb 17  Tim Menninger    Implemented memory unit
--
-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

library common;
use common.common.all;

--
-- entity MemoryUnit
--
-- Defines the inputs and outputs for the memory unit of the Atmel AVR
-- emulator
--
entity MemoryUnit is
    port (
        clk         : in  std_logic;        -- system clock

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
        useIP       : in  std_logic;        -- use IP for writing when '1'
        EN          : in  std_logic;        -- active low enable
        addrOut     : out address_t;        -- address after inc/dec

        DataAB      : out address_t;        -- address to memory
        DataDB      : inout data_t;         -- data bus in and out
        DataRd      : out std_logic;        -- read signal to memory
        DataWr      : out std_logic         -- write signal to memory
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
    signal address      : address_t := "0000000000000000";
    signal offset       : address_t := "0000000000000000";
    signal nextAddress  : address_t := "0000000000000000";
    component NBitAdder is
        generic (
            n       : natural := addrBits                   -- bits in adder
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

    -- Frequently, the address needs to be offset.  This adder will do the offset
    -- and output the new address
    AddrAdder : NBitAdder
        port map ('0', address, offset, open, nextAddress);

    -- We use the input select signal to choose which address input we should be
    -- using
    with addrSel    select address <=
        IPAddr                  when "00", -- Used when pushing/popping IP
        regAddr                 when "01",
        SPAddr                  when "10",
        ProgAddr                when others;

    -- We have a decrement signal which tells us to add all 1s.  Otherwise,
    -- we can just use the immediate and fill the high bits with 0s.  The only
    -- way for the high bit of immed to be set is if we are using a 12-bit signed
    -- value, in which case it is negative and we want to propagate the negative
    -- flag.
    with decrement  select offset <=
        "1111111111111111"                  when '0',
        (15 downto 12 => immed(11)) & immed when others;

    -- When adding before, use the adder output for memory access.  Otherwise,
    -- use the muxed input
    with addBefore  select DataAB <=
        nextAddress             when '0',
        address                 when others;

    -- No matter what, we output the new address in case someone needs to update
    -- something somewhere
    addrOut <= nextAddress;

    -- Set the data inout to high impedance or pass data through
    DataDB <=
        IPAddr(7 downto 0)      when RW = '0' and useIP = '1' else
        dataIn                  when RW = '0'                 else
        "ZZZZZZZZ";

    -- Using the enable signal and the clock, we can set the read and write outputs
    DataRd <= not RW when clk = '0' and EN = '0' else '1';
    DataWr <=     RW when clk = '0' and EN = '0' else '1';

end workflow;
