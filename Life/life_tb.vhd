-----------------------------------------------------------------------------------------
--
-- Conway's Game of Life Testbench
--
-- This tests the cell and systolic array that is used to perform Conway's
-- game of life simulation.  We test a 2x2 array with all of the possible
-- combinations that can't be achieved by rotating or flipping another.  We then test
-- a 4x4 array with it then completely dead, which should result in no change over
-- all clocks.  Then, a larger array is used.  A fully alive array is tested, a
-- "checkered" one, and a random one.  We don't test a 1x1 array because there is
-- no functionality for it.
--
-- Revision History:
--      09 Mar 17    Tim Menninger   Created
--
-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity LIFE_TB is
    generic (
        -- The size of our test array
        n       : natural := 10
    );
end LIFE_TB;

architecture TB_ARCHITECTURE of LIFE_TB is
    -- GCD component
    component  GameOfLife  is
        generic (
            n       : integer                   -- We will create n^2 mesh
        );
        port (
            Tick    : in  std_logic;            -- Cells update on tick rising edge
            Shift   : in  std_logic;            -- Shift data through rows on high
            DataIn  : in  std_logic_vector(0 to n-1); -- Data to shift in
            DataOut : out std_logic_vector(0 to n-1)  -- Data shifted out
        );
    end component;

    -- Contains the test array inputs and outputs
    file   cellArrays   : text;

    -- Signal used to clock next cell array values
    signal Tick         : std_logic := '0';

    -- Signal used to denote shifting
    signal Shift        : std_logic := '0';

    -- Input and output signals to shift adta in and out of the array
    signal DataIn       : std_logic_vector(0 to n-1) := (others => '0');
    signal DataOut2x2   : std_logic_vector(0 to 1)   := (others => '0');
    signal DataOut4x4   : std_logic_vector(0 to 3)   := (others => '0');
    signal DataOutnxn   : std_logic_vector(0 to n-1) := (others => '0');

begin

    -- 2x2 Unit Under Test
    UUT2x2 : GameOfLife
        generic map (
            n           => 2
        )
        port map (
            Tick        => Tick,
            Shift       => Shift,
            DataIn      => DataIn(0 to 1),
            DataOut     => DataOut2x2
        );

    -- 4x4 Unit Under Test
    UUT4x4 : GameOfLife
        generic map (
            n           => 4
        )
        port map (
            Tick        => Tick,
            Shift       => Shift,
            DataIn      => DataIn(0 to 3),
            DataOut     => DataOut4x4
        );

    -- nxn Unit Under Test
    UUTnxn : GameOfLife
        generic map (
            n           => n
        )
        port map (
            Tick        => Tick,
            Shift       => Shift,
            DataIn      => DataIn(0 to n-1),
            DataOut     => DataOutnxn
        );

    -- now generate the stimulus and test the design
    DoTest: process

        -- Iteration variables
        variable i              : integer;

        -- Array of values for testing
        type CellArray is array(0 to n-1) of std_logic_vector(0 to n-1);

        -- Variables for reading lines
        variable intLine        : line;
        variable arrLine        : line;
        variable row            : bit_vector(0 to n-1);
        variable num            : integer;
        variable numTicks       : integer;
        variable cells          : CellArray := (others => (others => '0'));
        variable expected       : std_logic_vector(0 to n-1) := (others => '0');

    begin  -- of stimulus process
        -- Open the testcase file
        file_open(cellArrays, "life_vectors.txt", read_mode);

        -- Wait a few clocks with everything idle
        Tick <= '0';
        Shift <= '0';
        wait for 200 ns;

        ---------------------------------------------------------------------------------
        --
        -- Test all combinations of 2x2 and Shift
        --
        ---------------------------------------------------------------------------------

        ------------------------------
        -- Testing shift
        ------------------------------
        -- We're shifting, load data so we have all 1's
        DataIn(0 to 1) <= "11";
        Shift <= '1';
        wait for 5 ns;
        -- Shift data in
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0'; -- Now everything is 1, next shift should shift out 1's
        wait for 50 ns;
        -- Do another shift.  We should now see data out be 1
        Tick <= '1';
        wait for 50 ns;
        assert (std_match(DataOut2x2, "11"))
            report  "Incorrect 2x2 output from shift when started all alive"
            severity  ERROR;
        -- Data out shouldn't change when Tick goes back low
        Tick <= '0';
        wait for 50 ns;
        assert (std_match(DataOut2x2, "11"))
            report  "Shift changed when Tick went inactive in 2x2 array"
            severity  ERROR;

        ------------------------------
        -- All alive
        ------------------------------
        Shift <= '0';
        wait for 50 ns;
        for i in 0 to 10 loop
            Tick <= '1';
            wait for 50 ns;
            Tick <= '0';
            wait for 50 ns;
        end loop;
        -- Should have all 1's still.  Meanwhile, shift in next test.
        DataIn(0 to 1) <= "00";
        Shift <= '1';
        wait for 50 ns;
        -- Rightmost column valid on shift, each tick will shift
        assert (std_match(DataOut2x2, "11"))
            report  "Incorrect 2x2 output col 0 from simulation when started all alive"
            severity  ERROR;
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        assert (std_match(DataOut2x2, "11"))
            report  "Incorrect 2x2 output col 1 from simulation when started all alive"
            severity  ERROR;
        -- Shift in the last column of zeroes
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;

        ------------------------------
        -- All dead
        ------------------------------
        Shift <= '0';
        -- Data already here, just simulate and test outputs
        wait for 50 ns;
        for i in 0 to 10 loop
            Tick <= '1';
            wait for 50 ns;
            Tick <= '0';
            wait for 50 ns;
        end loop;
        -- Should have all 1's still.  Meanwhile, shift in next test.
        DataIn(0 to 1) <= "10";
        Shift <= '1';
        wait for 50 ns;
        -- DataOut valid from start
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 0 from simulation when started all dead"
            severity  ERROR;
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 1 from simulation when started all dead"
            severity  ERROR;
        -- Shift in last column
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;

        ------------------------------
        -- One row alive
        ------------------------------
        Shift <= '0';
        -- Data already here, just simulate and test outputs
        wait for 50 ns;
        -- All cells dead after one clock
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        -- Should have all 1's still.  Meanwhile, shift in next test.
        DataIn(0 to 1) <= "10";
        Shift <= '1';
        wait for 50 ns;
        -- DataOut valid from start
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 0 from simulation when one row alive"
            severity  ERROR;
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        DataIn(0 to 1) <= "01";
        wait for 50 ns;
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 1 from simulation when one row alive"
            severity  ERROR;
        -- Shift in last column
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;

        ------------------------------
        -- Diagonal alive
        ------------------------------
        Shift <= '0';
        -- Data already here, just simulate and test outputs
        wait for 50 ns;
        -- All cells dead after one clock
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        -- Should have all 1's still.  Meanwhile, shift in next test.
        Shift <= '1';
        wait for 50 ns;
        -- DataOut valid from start
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 0 from simulation when diagonal alive"
            severity  ERROR;
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;
        assert (std_match(DataOut2x2, "00"))
            report  "Incorrect 2x2 output col 1 from simulation when diagonal alive"
            severity  ERROR;
        -- Shift in last column
        Tick <= '1';
        wait for 50 ns;
        Tick <= '0';
        wait for 50 ns;

        ---------------------------------------------------------------------------------
        --
        -- Use 4x4 array to show all dead leads to continuation of all dead
        --
        ---------------------------------------------------------------------------------
        Shift <= '1';
        DataIn(0 to 3) <= "0000";
        wait for 50 ns;
        -- All cells dead
        for i in 0 to 3 loop
            Tick <= '1';
            wait for 50 ns;
            Tick <= '0';
            wait for 50 ns;
        end loop;
        Shift <= '0';
        -- Run then show that all cells still dead
        for i in 0 to 10 loop
            Tick <= '1';
            wait for 50 ns;
            Tick <= '0';
            wait for 50 ns;
        end loop;
        -- Shift out and show all cells are still dead
        Shift <= '1';
        wait for 50 ns;
        assert (std_match(DataOut4x4, "0000"))
            report  "Incorrect 4x4 output col 0 from simulation when one row alive"
            severity  ERROR;
        for i in 1 to 3 loop
            Tick <= '1';
            wait for 50 ns;
            Tick <= '0';
            wait for 50 ns;
            assert (std_match(DataOut4x4, "0000"))
                report  "Incorrect 2x2 output col " & integer'image(i) &
                        " from simulation when one row alive"
                severity  ERROR;
        end loop;

        ---------------------------------------------------------------------------------
        --
        -- Use nxn array to show more cases
        --
        ---------------------------------------------------------------------------------
        -- These are arbitrary cases generated using the random package in Python

        -- First line is size of array
        readline(cellArrays, intLine);
        read(intLine, num);
        assert(num = n)
            report "Test cases size does not match test vector size"
            severity ERROR;

        while (not endfile(cellArrays) and num = n) loop
            -- See how many ticks we do
            readline(cellArrays, intLine);
            read(intLine, numTicks);
            -- Shift new values in, while checking to make sure values that come out
            -- are what they should be.
            for i in 0 to n-1 loop
                readline(cellArrays, arrLine);
                read(arrLine, row);
                for j in 0 to n-1 loop
                    cells(i) := to_stdlogicvector(row);
                end loop;
            end loop;

            -- Run for specified clocks
            Shift <= '0';
            wait for 50 ns;
            for i in 1 to numTicks loop
                Tick <= '1';
                wait for 50 ns;
                Tick <= '0';
                wait for 50 ns;
            end loop;

            Shift <= '1';
            wait for 45 ns;
            for i in 0 to n-1 loop
                -- Fill expected vector
                for j in 0 to n-1 loop
                    DataIn(j) <= cells(j)(n-1-i);
                end loop;
                wait for 5 ns;
                -- If there were 0 ticks, we don't care about the output anymore
                -- for this iteration because we are switching tests
                if (numTicks /= 0) then
                    assert(std_match(DataOutnxn, DataIn))
                        report "Test nxn array didn't give correct output at col " &
                                integer'image(i)
                        severity ERROR;
                end if;

                -- Shift vector in
                Tick <= '1';
                wait for 50 ns;
                Tick <= '0';
                wait for 45 ns;
            end loop;
        end loop;

        -- Buffer for end of wave viewer
        wait for 50 ns;
        wait;

    end process; -- end of stimulus process


end TB_ARCHITECTURE;
