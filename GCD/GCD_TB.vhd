----------------------------------------------------------------------------
--
--  Test Bench for GCD
--
--  This is a test bench for the GCD entity.
--
--  Revision History:
--
----------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity gcd_tb is
end gcd_tb;

architecture TB_ARCHITECTURE of gcd_tb is
    -- GCD component
    component  GCD  is
        generic (
            dbncBits      : natural := 5
        );
        port (
            sysclk        : in  std_logic;
            aIn           : in  std_logic_vector(15 downto 0);
            bIn           : in  std_logic_vector(15 downto 0);
            can_read_vals : in  std_logic;
            calculate     : in  std_logic;
            result        : out std_logic_vector(15 downto 0);
            result_rdy    : out std_logic
        );
    end component;

    -- Stimulus signals - signals mapped to the input and inout ports of tested entity
    signal sysclk        : std_logic;
    signal aIn           : std_logic_vector(15 downto 0);
    signal bIn           : std_logic_vector(15 downto 0);
    signal can_read_vals : std_logic;
    signal calculate     : std_logic;

    -- Observed signals - signals mapped to the output ports of tested entity
    signal result        : std_logic_vector(15 downto 0);
    signal result_rdy    : std_logic;

    --Signal used to stop clock signal generators
    signal  END_SIM      : BOOLEAN := FALSE;

    -- test values
    constant  TestInputs : std_logic_vector(575 downto 0) :=
        --       a         &         b            -- expected output
        -- Test GCD with input(s) 0
        "0000000000000000" & "0000000000000000" & -- GCD(0, 0) should give 0
        "0000000000000000" & "1010101010101010" & -- GCD(0, x != 0) should give 0
        "1111111111111111" & "0000000000000000" & -- GCD(x != 0, 0) should give 0
        -- Test GCD with input 1
        "0000000000000001" & "1101100110010101" & -- GCD(1, x != 0) should give 1
        "0001101110011010" & "0000000000000001" & -- GCD(x != 0, 1) should give 1
        -- Test GCD with duplicate inputs
        "1111111111111111" & "1111111111111111" & -- GCD(x, x) should give x
        -- Test GCD when multiples of each other
        "0111111111111111" & "1111111111111110" & -- GCD(x, 2x) should give x
        "1111111111111110" & "0111111111111111" & -- GCD(2x, x) should give x
        -- Test GCD with two primes. Note: 65521 and 65519 are prime
        "1111111111110001" & "1111111111101111" & -- GCD(65521, 65519) should give 1
        "1111111111101111" & "1111111111110001" & -- GCD(65519, 65521) should give 1
        -- Test GCD with one prime and one composite
        "1111111111101111" & "1110000000000000" & -- GCD(65519, 57344) should give 1
        "1110000000000000" & "1111111111101111" & -- GCD(57344, 65519) should give 1
        -- Test GCD with two composites that are relatively prime
        "0000001001011100" & "0000000010000111" & -- GCD(604, 135) should give 1
        "0000000010000111" & "0000001001011100" & -- GCD(135, 604) should give 1
        -- Test GCD with values that are not relatively prime and not multiples
        "1110101001100000" & "1110101001011011" & -- GCD(60000, 59995) should give 5
        "1110101001011011" & "1110101001100000" & -- GCD(59995, 60000) should give 5
        "1110101001100000" & "1100001101010000" & -- GCD(60000, 50000) should give 10000
        "1100001101010000" & "1110101001100000";  -- GCD(50000, 60000) should give 10000

    constant  TestOutput : std_logic_vector(287 downto 0) :=
        --    GCD(a, b)      -- inputs
        "0000000000000000" & -- GCD(0, 0) = 0
        "0000000000000000" & -- GCD(0, x != 0) = 0
        "0000000000000000" & -- GCD(x != 0, 0) = 0
        "0000000000000001" & -- GCD(1, x != 0) = 1
        "0000000000000001" & -- GCD(x != 0, 1) = 1
        "1111111111111111" & -- GCD(x, x) = x
        "0111111111111111" & -- GCD(x, 2x) = x
        "0111111111111111" & -- GCD(2x, x) = x
        "0000000000000001" & -- GCD(65521, 65519) = 1
        "0000000000000001" & -- GCD(65519, 65521) = 1
        "0000000000000001" & -- GCD(65519, 57344) = 1
        "0000000000000001" & -- GCD(57344, 65519) = 1
        "0000000000000001" & -- GCD(604, 135) = 1
        "0000000000000001" & -- GCD(135, 604) = 1
        "0000000000000101" & -- GCD(60000, 59995) = 5
        "0000000000000101" & -- GCD(59995, 60000) = 5
        "0010011100010000" & -- GCD(60000, 50000) = 10000
        "0010011100010000";  -- GCD(50000, 60000) = 10000


begin

    -- Unit Under Test port map
    UUT : GCD
        port map (
            sysclk        => sysclk,
            aIn           => aIn,
            bIn           => bIn,
            can_read_vals => can_read_vals,
            calculate     => calculate,
            result        => result,
            result_rdy    => result_rdy
        );


    -- now generate the stimulus and test the design
    process

        -- some useful variables
        variable  i  :  integer;        -- general loop index

    begin  -- of stimulus process

        -- everything inactive to start, except inputs which force relatively
        -- bad time
        aIn <= "0000000000111111";
        bIn <= "0000000000000001";
        can_read_vals <= '0';
        calculate <= '1'; -- active low

        -- run for a few clocks
        wait for 100 ns;

        -- fsm should be idle, which has result_rdy
        assert (std_match(result_rdy, '1'))
            report  "not starting in idle state"
            severity  ERROR;

        -----------------------------------------------------------------------------------
        -- Test valid signal
        --

        -- "push" calculate button
        calculate <= '0';

        -- Wait a few clocks
        wait for 100 ns;

        -- Show that result_rdy is still active (should be until can_read_vals)
        assert (std_match(result_rdy, '1'))
            report  "result_rdy active before can_read_vals"
            severity  ERROR;

        -- "release" calculate button and show that it stays in idle
        calculate <= '1';
        wait for 800 ns;
        assert (std_match(result_rdy, '1'))
            report  "fsm did not stay in idle after calculate released"
            severity  ERROR;

        -- show that the fsm will now start because it latched calculate signal
        can_read_vals <= '1';
        wait for 100 ns;
        assert (std_match(result_rdy, '0'))
            report  "fsm did not latch calculate signal"
            severity  ERROR;

        -----------------------------------------------------------------------------------
        -- Test GCD
        --

        for i in 17 downto 0 loop
            -- Set initial values
            aIn <= TestInputs(2*i*16 + 31 downto 2*i*16 + 16);
            bIn <= TestInputs(2*i*16 + 15 downto 2*i*16 + 0);
            calculate <= '1'; -- active low
            can_read_vals <= '0';

            -- Wait a few clocks and then start
            wait for 100 ns;
            calculate <= '0';

            -- wait for debouncing before entering computation loop
            wait for 1000 ns;
            can_read_vals <= '1'; -- allows GCD to compute

            -- wait sufficiently long for GCD (65535 * 20 ns = 1.3e6)
            wait for 1400000 ns;

            -- check that output is correct
            assert (std_match(result, TestOutput(i*16 + 15 downto i*16)))
                report  "incorrect outputs to gcd"
                severity  ERROR;

        end loop;

        END_SIM <= TRUE;        -- end of stimulus events
        wait;                   -- wait for simulation to end

    end process; -- end of stimulus process


    CLOCK_CLK : process

    begin

        -- this process generates a 20 ns period, 50% duty cycle clock

        -- only generate clock if still simulating

        if END_SIM = FALSE then
            sysclk <= '0';
            wait for 10 ns;
        else
            wait;
        end if;

        if END_SIM = FALSE then
            sysclk <= '1';
            wait for 10 ns;
        else
            wait;
        end if;

    end process;


end TB_ARCHITECTURE;
