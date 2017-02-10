-- bring in the necessary packages
library  ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

-----------------------------------------------------------------------------------------
--
-- registers.vhd
--
-- This contains code to read and write to registers.  When writing to registers,
-- data appears on ALUIn if it is coming from the ALU and from memIn if it is coming
-- from memory.
--
-- Inputs:
--      clk : std_logic
--          The global clock
--      clkIdx : natural range 0 to 3
--          The number clock rising edges since the beginning of the instruction
--      ALUIn : std_logic_vector(7 downto 0)
--          Data written to a register in the appropriate scenarios
--      BLD : std_logic
--          '1' when BLD executing
--      sel : std_logic_vector(2 downto 0)
--          Bit index to set in BLD
--      T : std_logic
--          Value of T flag to set in register for BLD
--      regSelA : std_logic_vector(4 downto 0)
--          Selects which output to read or write to for register A
--      regSelB : std_logic_vector(4 downto 0)
--          Selects which output to read or write to for register B
--      ENMul : std_logic
--          Active low to indicate that result should be written to word comprised
--          of registers 0 and 1
--      ENSwap : std_logic
--          Active low to indicate nibbles of register should be swapped
--      ENRegA : std_logic
--          When active (active low), register A is enabled for output.  Otherwise
--          it is hi-Z
--      ENRegB : std_logic
--          When active (active low), register B is enabled for output.  Otherwise
--          it is hi-Z
--      ENWrite : std_logic
--          When active (active low), registers will be written to on rising edge of
--          clock.  Data will be taken from ALUIn and written according to guidelines
--          above.
--
-- Outputs:
--      dataOutA : unsigned(7 downto 0)
--          Reflects register A when RW indicates read.
--      dataOutB : unsigned(7 downto 0)
--          Reflects register B when RW indicates read.  This should only be used
--          when accessing a double register, and would be the high byte.
--
-- Revision History:
--      26 Jan 17  Tim Menninger     Entity declaration
--
-----------------------------------------------------------------------------------------

--
-- entity Registers
--
-- Defines the inputs and outputs for the general registers the Atmel AVR
-- emulator
--
entity Registers is
    port (
        clk         : in  std_logic;                    -- system clock
        clkIdx      : in  natural range 0 to 3;         -- number of clocks since instr

        ALUIn       : in  std_logic_vector(7 downto 0); -- data input from ALU
        memIn       : in  std_logic_vector(7 downto 0); -- data input from memory
        immedIn     : in  std_logic_vector(7 downto 0); -- immediate value from instr
        sourceSel   : in  std_logic_vector(1 downto 0); -- used to choose data source

        wordRegIn   : in  std_logic_vector(15 downto 0);-- new value for word register
        wordRegSel  : in  std_logic_vector(2 downto 0); -- selects which word register

        BLD         : in  std_logic;                    -- true when BLD occurring
        sel         : in  std_logic_vector(2 downto 0); -- bit select for BLD
        T           : in  std_logic;                    -- T flag

        regSelA     : in  std_logic_vector(4 downto 0); -- register select
        regSelB     : in  std_logic_vector(4 downto 0); -- register select
        ENMul       : in  std_logic;                    -- write to registers 0 and 1
        ENSwap      : in  std_logic;                    -- swap nibbles
        ENRegA      : in  std_logic;                    -- active low enable reg A
        ENRegB      : in  std_logic;                    -- active low enable reg B
        ENWrite     : in  std_logic;                    -- active low enable write

        Rdb         : out std_logic;                    -- b'th bit of reg A
        dataOutA    : out std_logic_vector(7 downto 0); -- low byte of output
        dataOutB    : out std_logic_vector(7 downto 0); -- high byte of output
        wordRegOut  : out std_logic_vector(15 downto 0) -- word register output
    );
end Registers;

--
-- architecture selReg of registers
--
-- Uses the instruction to choose which registers to use.  For more information
-- about what each opcode does, refer to instruction.vhd
--
-- Types:
--      reg : std_logic_vector(7 downto 0)
--          Allows us to easily select registers
--      regArray : array (0 to 31) of reg
--          Array of all 32 general purpose registers
--
architecture selReg of Registers is
    -- Array of registers
    type regArray is array (0 to 31) of std_logic_vector(7 downto 0);

    -- Define the general purpose registers
    signal regs : regArray := (others => (others => '0'));

    -- The data that will be written to register
    signal dataIn : std_logic_vector(7 downto 0);

begin

    -- Set Rdb for BLD case
    Rdb <= regs(conv_integer(regSelA))(conv_integer(sel));

    -- Propagate register A data to output
    dataOutA <= regs(conv_integer(regSelA));

    -- Propagate register B data to output
    dataOutB <= regs(conv_integer(regSelB));

    -- Choose which data input to use
    with sourceSel select dataIn <=
        ALUIn               when "00",
        memIn               when "01",
        immedIn             when others;

    -- Choose which word register to output
    with wordRegSel(1 downto 0) select wordRegOut <=
        regs(27) & regs(26) when "01",
        regs(29) & regs(28) when "10",
        regs(31) & regs(30) when others;

    -------------------------------------------------------------------------------------
    -- process writeRegs
    --
    -- This process is responsible for writing the data on the input port to the
    -- register corresponding to the instruction's destination.
    --
    writeRegs: process (clk) is

    begin
        -- Only write to registers on rising clock edge
        if (rising_edge(clk)) then
            -- If we are writing, then blindly write from ALUIn to any enabled
            -- registers.  Some opcodes are special cases where we have to fix the write.
            -- This handles ADIW and SBIW by adding in the clock number, which will be
            -- 0 for everything except the second clock where we write to Rd+1.  If
            -- we have a word register select signal, we will write that, too.
            --
            -- Special cases:
            --      MUL always writes to R1 and R0
            --      SWAP done here, not somewhere else to be written here
            --      BLD done here
            if (ENWrite = '0') then
                -- Handle word registers
                case wordRegSel is
                    when "101" => -- X
                        regs(26) <= wordRegIn(7 downto 0);
                        regs(27) <= wordRegIn(15 downto 8);
                        regs(28) <= regs(28);
                        regs(29) <= regs(29);
                        regs(30) <= regs(30);
                        regs(31) <= regs(31);
                    when "110" => -- Y
                        regs(26) <= regs(26);
                        regs(27) <= regs(27);
                        regs(28) <= wordRegIn(7 downto 0);
                        regs(29) <= wordRegIn(15 downto 8);
                        regs(30) <= regs(30);
                        regs(31) <= regs(31);
                    when "111" => -- Z
                        regs(26) <= regs(26);
                        regs(27) <= regs(27);
                        regs(28) <= regs(28);
                        regs(29) <= regs(29);
                        regs(30) <= wordRegIn(7 downto 0);
                        regs(31) <= wordRegIn(15 downto 8);
                    when others =>
                        regs(26) <= regs(26);
                        regs(27) <= regs(27);
                        regs(28) <= regs(28);
                        regs(29) <= regs(29);
                        regs(30) <= regs(30);
                        regs(31) <= regs(31);
                end case;

                -- Handle general registers
                if (ENRegA = '0') then
                    regs(conv_integer(regSelA)) <= dataIn;
                end if;
                if (ENRegB = '0') then
                    regs(conv_integer(regSelA)) <= regs(conv_integer(regSelB));
                end if;
            end if;

            if (ENMul = '0') then
                -- On the first clock, we write to register 0.  On second clock, we
                -- write to register 1.  Use clkIdx as offset.
                regs(clkIdx) <= dataIn;
            end if;

            if (ENSwap = '0') then
                -- Swap nibbles
                regs(conv_integer(regSelA))(7 downto 4) <=
                    regs(conv_integer(regSelA))(3 downto 0);
                regs(conv_integer(regSelA))(3 downto 0) <=
                    regs(conv_integer(regSelA))(7 downto 4);
            end if;

            if (BLD = '1') then
                regs(conv_integer(regSelA))(conv_integer(sel)) <= T;
            end if;

        end if;
    end process writeRegs;

end selReg;
