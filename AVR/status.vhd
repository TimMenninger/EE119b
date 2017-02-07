-- bring in the necessary packages
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

-----------------------------------------------------------------------------------------
--
-- status.vhd
--
-- This component is responsible for the status register, which is comprised of 8
-- flags, listed here in order from high bit (7) to low bit (0):
--      I   - interrupt enable(1)/disable(0)
--      T   - transfer flag
--      H   - half-carry flag (carry from bit 3 to 4)
--      S   - corrected sign flag
--      V   - signed overflow flag
--      N   - sign flag (high bit of result)
--      Z   - zero flag
--      C   - carry flag
-- The approach here is to set each relevant bit based on what the instruction opcode
-- is.  Note that only ALU instructions affect flags, other than SWAP and BLD.
--
-- Inputs:
--      clk : std_logic
--          System clock
--      R : std_logic_vector(7 downto 0)
--          Result computed from ALU, used in computing flag
--      Rd0 : std_logic
--          Bit 0 of operand A of ALU operation
--      Rd3 : std_logic
--          Bit 3 of operand A of ALU operation
--      Rd7 : std_logic
--          Bit 7 of operand A of ALU operation
--      Rr3 : std_logic
--          Bit 3 of operand B of ALU operation
--      Rr7 : std_logic
--          Bit 7 of operand B of ALU operation
--      Rdb : std_logic
--          Bit b of register being used to set T flag
--      BST : std_logic
--          Set to 1 when we are executing BST instruction
--      sel : std_logic_vector(2 downto 0)
--          Index to the array of flag computations used to propagate flag to
--          output
--      mask : std_logic_vector(7 downto 0)
--          Masks for flags that shouldn't be changing on this operation.  Locations
--          of affected flags should have zeroes.
--
-- Outputs:
--      TF : std_logic
--          T flag for BLD instruction
--      SREG : std_logic_vector(7 downto 0)
--          Status register
--
-- Revision History:
--      31 Jan 17   Tim Menninger   Created
--
-----------------------------------------------------------------------------------------

--
-- entity status
--
-- Defines the status register
--
entity Status is
    port (
        clk         : in  std_logic;                            -- system clock

        R           : in  std_logic_vector(7 downto 0);         -- result from ALU
        Rd0         : in  std_logic;                            -- bit 0 of operand A
        Rd3         : in  std_logic;                            -- bit 3 of operand A
        Rr3         : in  std_logic;                            -- bit 3 of operand B
        Rd7         : in  std_logic;                            -- bit 7 of operand A
        Rr7         : in  std_logic;                            -- bit 7 of operand B
        Rdb         : in  std_logic;                            -- Bit to set T to

        BST         : in  std_logic;                            -- 1 when BST instruction
        sel         : in  std_logic_vector(2 downto 0);         -- selects flag index
        mask        : in  std_logic_vector(7 downto 0);         -- masks unaffected flags
        clkIdx      : in  natural range 0 to 3;                 -- clocks since instrctn
        ENRes       : in  std_logic;                            -- set SREG to R

        TF          : out std_logic;                            -- always sent to regs
        SREG        : out std_logic_vector(7 downto 0)          -- status register
    );
end Status;

--
-- architecture update of status
--
-- Updates the status register when status input changes
--
architecture update of Status is

    -- Define constants to more readably access flags
    constant T : natural := 6;
    constant H : natural := 5;
    constant S : natural := 4;
    constant V : natural := 3;
    constant N : natural := 2;
    constant Z : natural := 1;
    constant C : natural := 0;

    -- Flags that we will compute on the run
    signal HF : std_logic_vector(7 downto 0);
    signal VF : std_logic_vector(7 downto 0);
    signal NF : std_logic;
    signal ZF : std_logic;
    signal CF : std_logic_vector(7 downto 0);

    -- Register the status flags
    signal status : std_logic_vector(7 downto 0);

begin

    -- Always output the status register
    SREG <= status;

    -- All flag vectors are in the order starting from 0 that they appear in the
    -- datasheet.  Note:
    --      Interrupt flag only changes if manually done
    --      T flag must be set manually
    --      Corrected sign flag always NF xor VF
    --

    TF <= status(T);

    -- Half-carry flag
    HF(0) <=
        '1' when
            (Rd3 = '1' and Rr3 = '1') or
            (Rr3 = '1' and R(3)  = '0') or
            (R(3) = '0' and Rd3 = '1')
        else '0';
    HF(1) <= '0';
    HF(2) <= '0';
    HF(3) <=
        '1' when
            (Rd3 = '0' and Rr3 = '1') or
            (Rr3 = '1' and R(3) = '1') or
            (R(3) = '1' and Rd3 = '0')
        else '0';
    HF(4) <= '0';
    HF(5) <= '0';
    HF(6) <= R(3) or Rd3;

    -- Overflow flag
    VF(0) <=
        '1' when
            (Rd7 = '1' and Rr7 = '1' and R(7) = '0') or
            (Rd7 = '0' and Rr7 = '0' and R(7) = '1')
        else '0';
    VF(1) <=
        '1' when
            (Rd0 = '1' and R(7) = '0') or
            (Rd0 = '0' and R(7) = '1')
        else '0';
    VF(2) <= '0';
    VF(3) <=
        '1' when
            (Rd7 = '1' and Rr7 = '0' and R(7) = '0') or
            (Rd7 = '0' and Rr7 = '1' and R(7) = '1')
        else '0';
    VF(4) <= '1' when R = "01111111" else '0';
    VF(5) <= '1' when R = "10000000" else '0';
    VF(6) <= '1' when R = "10000000" else '0';

    -- Sign flag
    NF <= R(7);

    -- Zero flag
    ZF <=
        '1' when
            (R = "00000000" and clkIdx = 0)
        else '1' when
            (R = "00000000" and clkIdx = 1 and status(Z) = '1')
        else '0';

    -- Carry flag
    CF(0) <=
        '1' when
            (Rd7 = '1' and Rr7 = '1') or
            (Rr7 = '1' and R(7) = '0') or
            (R(7) = '0' and Rd7 = '1')
        else '0';
    CF(1) <= Rd0;
    CF(2) <= '1';
    CF(3) <=
        '1' when
            (Rd7 = '0' and Rr7 = '1') or
            (Rr7 = '1' and R(7) = '1') or
            (R(7) = '1' and Rd7 = '0')
        else '0';
    CF(4) <= '0';
    CF(5) <= R(7);
    CF(6) <= '0' when R = "00000000" else '1';

    -------------------------------------------------------------------------------------
    -- Actually set flags
    --

    setFlags : process (clk) is
    begin

        if (rising_edge(clk)) then
            -- Set each flag to either its previous value or the new indexed value, based
            -- on the flag mask
            case mask(T) is
                when '0' => status(T) <= Rdb;
                when others => status(T) <= status(T);
            end case;

            case mask(H) is
                when '0' => status(H) <= HF(conv_integer(sel));
                when others => status(H) <= status(H);
            end case;

            case mask(S) is
                when '0' => status(S) <= (NF xor VF(conv_integer(sel)));
                when others => status(S) <= status(S);
            end case;

            case mask(V) is
                when '0' => status(V) <= VF(conv_integer(sel));
                when others => status(V) <= status(V);
            end case;

            case mask(N) is
                when '0' => status(N) <= NF;
                when others => status(N) <= status(N);
            end case;

            case mask(C) is
                when '0' => status(C) <= CF(conv_integer(sel));
                when others => status(C) <= status(C);
            end case;

            -- In some cases we set the status based on result
            if (ENRes = '0') then
                status <= (mask and status) or (not mask and R);
            end if;

            -- Extract one bit from result if on BST instruction
            if (BST = '1') then
                status(T) <= R(conv_integer(sel));
            end if;

        end if;

    end process setFlags;

end update;
