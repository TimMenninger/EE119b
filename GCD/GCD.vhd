-------------------------------------------------------------------------------------------
--
-- GCD.vhd
--
-- This implements Euclid's subtraction algorithm to compute the GCD of two inputs.
-- It performs GCD on two 16-bit inputs.  It waits in an idle state for the calculate
-- signal to go active.  It latches this signal and the next time can_read_vals goes
-- active, which indicates inputs are valid, it begins computation.  To compute GCD,
-- we use Euclid's subtraction algorithm.  When the computed result is valid, result_rdy
-- is asserted.
--
-- Revision History:
--     18 Jan 17 - Tim Menninger: Created
--
-------------------------------------------------------------------------------------------

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;

--
-- GCD entity
--
-- inputs:
--      sysclk (std_logic)                      - system clock
--      aIn (std_logic_vector(15 downto 0))     - first input
--      bIn (std_logic_vector(15 downto 0))     - second input
--      can_read_vals (std_logic)               - goes high when a and b ready
--      calculate (std_logic)                   - calculate switch
--
-- outputs:
--      result (std_logic_vector(15 downto 0))  - GCD of a and b
--      result_rdy (std_logic)                  - goes high when result ready
--
entity  GCD  is
    port (
        sysclk        : in  std_logic;

        aIn           : in  std_logic_vector(15 downto 0);
        bIn           : in  std_logic_vector(15 downto 0);
        can_read_vals : in  std_logic;
        calculate     : in  std_logic;

        result        : out std_logic_vector(15 downto 0);
        result_rdy    : out std_logic
    );
end GCD;

--
-- compute architecture for GCD
--
-- Comptues the GCD of the two inputs when they are valid and calculate was high
--
-- Types:
--      FSM - State machine for computing GCD
--
-- Signals:
--      state (FSM) - State in computation state machine
--      a (unsigned) - Latch for one input
--      b (unsigned) - Latch for other input
--      calc (std_logic) - Used to latch calculate button in case it is released
--          before can_read_vals goes active
--
architecture  compute  of  GCD  is

    --
    -- type FSM
    --
    -- State machine for detecting calculate buttonpush and then computing GCD of two
    -- inputs.
    --
    -- States:
    --      idle - Propagates the result and waits for calculate signal to begin
    --          calculating.  It keeps result_rdy high (active)
    --      computeLoop - Computes GCD of the two inputs.  It keeps result_rdy low
    --          and moves to idle when done computation (where result_rdy will be
    --          made active)
    --
    type FSM is (
        idle,
        computeLoop
    );

    -- Signals used to compute GCD
    signal state : FSM := idle;

    signal a     : unsigned (15 downto 0) := "0000000000000000";
    signal b     : unsigned (15 downto 0) := "0000000000000000";

    signal calc  : std_logic := '0';

begin
    --
    -- computeGCD process
    --
    -- Waits for the calculate buttonpush, then latches it.  After latcing,
    -- it waits for the input valid signal.  Finally, when the button was pushed
    -- and the inputs are valid, it computes the GCD of the two inputs and puts
    -- it in result.  When the result is valid, it asserts the result_rdy signal.
    --
    computeGCD: process (sysclk) is

    begin

        if (rising_edge(sysclk)) then
            -- Latch calculate button
			if (calculate = '0') then
                calc <= '1';
			end if;

            -- State definitions for the GCD fsm
            case state is
                ---------------------------------------------------------------------------
                -- idle state: waits for calculate signal
                ---------------------------------------------------------------------------
                when idle =>
                    -- The result is always ready in the idle state
                    result_rdy <= '1';

                    -- If calculate signal comes in and inputs valid, begin calculation
                    if (calc = '1' and can_read_vals = '1') then
                        -- Reset calc for next buttonpush
                        calc <= '0';

                        -- If either input is 0, return 0
                        if (aIn = "0000000000000000" or bIn = "0000000000000000") then
                            result <= "0000000000000000";
                        else
                            -- Latch inputs
                            a <= unsigned(aIn);
                            b <= unsigned(bIn);
                            state <= computeLoop;
						end if;
                    end if;
                ---------------------------------------------------------------------------
                -- computeLoop state: computes GCD
                ---------------------------------------------------------------------------
                when computeLoop =>
					-- The result is not ready
                    result_rdy <= '0';
                    
                    -- In the end, result will contain b.  Because result_rdy is 0, we can
                    -- destroy the old value it held
                    result <= std_logic_vector(b);

                    -- Euclid's algorithm states that we should subtract b from a when
                    -- a is greater than or equal to b.  Otherwise, we switch a and b.
                    if (a >= b) then
                        -- Subtract b from a.
                        a <= a - b;
                    else
                        -- Switch the contents of a and b
                        a <= b;
                        b <= a;

                        -- We iterate until b is 0.  Because b contains what a had, we
                        -- check a instead.
                        if (a = 0) then
                            state <= idle;
                        end if;
                    end if;
            end case;
        end if;
    end process computeGCD;
end compute;
