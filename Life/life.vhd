-----------------------------------------------------------------------------------------
--
-- life.vhd
--
-- This is entity that connects the cells in the systolic array for life simulation.  It
-- does not support a 1x1 array because this case would require extra logic for a
-- scenario nobody cares about (all starting configurations end in the one cell dying
-- and remaining dead indefinitely).
--
-- Revision History:
--      07 Mar 17   Tim Menninger   Created
--
-----------------------------------------------------------------------------------------

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- Entity for game of life
entity GameOfLife is
    generic (
        n       : integer := 16             -- We will create n^2 mesh
    );
    port (
        Tick    : in  std_logic;            -- Cells update on tick rising edge
        Shift   : in  std_logic;            -- Shift data through rows on high
        DataIn  : in  std_logic_vector(0 to n-1); -- Data to shift in
        DataOut : out std_logic_vector(0 to n-1)  -- Data shifted out
    );
end entity;

-- Architecture to generate 2D mesh
architecture mesh of GameOfLife is

    -- We use the cell component for each value in the mesh
    component Cell is
        port (
            Tick        : in  std_logic;    -- Indicates time tick passed, active high
            Shift       : in  std_logic;    -- Shift data through rows (W to E) when high

            aliveW      : in  std_logic;    -- Cell on       west side alive when high
            aliveNW     : in  std_logic;    -- Cell on north west side alive when high
            aliveN      : in  std_logic;    -- Cell on north      side alive when high
            aliveNE     : in  std_logic;    -- Cell on north east side alive when high
            aliveE      : in  std_logic;    -- Cell on       east side alive when high
            aliveSE     : in  std_logic;    -- Cell on south east side alive when high
            aliveS      : in  std_logic;    -- Cell on south      side alive when high
            aliveSW     : in  std_logic;    -- Cell on south west side alive when high

            DataOut     : out std_logic     -- Shifted out, contains alive/not alive
        );
    end component;

    -- This type is every output for each cell.  We make a 2d array of these so we can
    -- neatly hook up cell outputs to inputs
    type CellRecord is
        record
            -- The state of this cell
            alive : std_logic;
        end record;

    -- Create the types for a 2D array of cells
    type CellRow   is array(0 to n-1) of CellRecord;
    type CellArray is array(0 to n-1) of CellRow;

    -- Create the array of cells
    signal Cells : CellArray := (others => (others => (others => '0')));

    -- This is how we know if we are shifting in from the left side, or not shifting and
    -- treating it as a ghost column of dead cells
    signal leftColIn : std_logic_vector(0 to n-1);

begin

    -- Connect the Cell processing elements.  We first do the boundary.  We are doing
    -- constant boundary, where everything on the edge thinks its neighbors outside
    -- of the array are dead.

    -- Top row
    TopRowCells: for j in 1 to n-2 generate
        Cell_0_j: Cell port map (
            Tick, Shift,

            Cells( 0 )(j-1).alive,  -- West cell
            '0',                    -- Northwest cell
            '0',                    -- North cell
            '0',                    -- Northeast cell
            Cells( 0 )(j+1).alive,  -- East cell
            Cells( 1 )(j+1).alive,  -- Southeast cell
            Cells( 1 )( j ).alive,  -- South cell
            Cells( 1 )(j-1).alive,  -- Southwest cell

            Cells(0)(j).alive
        );
    end generate;

    -- Bottom row
    BottomRowCells: for j in 1 to n-2 generate
        Cell_n1_j: Cell port map (
            Tick, Shift,

            Cells(n-1)(j-1).alive,  -- West cell
            Cells(n-2)(j-1).alive,  -- Northwest cell
            Cells(n-2)( j ).alive,  -- North cell
            Cells(n-2)(j+1).alive,  -- Northeast cell
            Cells(n-1)(j+1).alive,  -- East cell
            '0',                    -- Southeast cell
            '0',                    -- South cell
            '0',                    -- Southwest cell

            Cells(n-1)(j).alive
        );
    end generate;

    -- Left and right columns (skipping top and bottom to avoid redundancy)
    SideColumnsCells: for i in 1 to n-2 generate
        -- Left column
        leftColIn(i) <= Shift and DataIn(i); -- Either dead cell, or shifting in
        Cell_i_0: Cell port map (
            Tick, Shift,

            leftColIn(i),           -- West cell to accommodate shifting in
            '0',                    -- Northwest cell
            Cells(i-1)( 0 ).alive,  -- North cell
            Cells(i-1)( 1 ).alive,  -- Northeast cell
            Cells( i )( 1 ).alive,  -- East cell
            Cells(i+1)( 1 ).alive,  -- Southeast cell
            Cells(i+1)( 0 ).alive,  -- South cell
            '0',                    -- Southwest cell

            Cells(i)(0).alive
        );
        -- Right column
        Cell_i_n1: Cell port map (
            Tick, Shift,

            Cells( i )(n-2).alive,  -- West cell
            Cells(i-1)(n-2).alive,  -- Northwest cell
            Cells(i-1)(n-1).alive,  -- North cell
            '0',                    -- Northeast cell
            '0',                    -- East cell
            '0',                    -- Southeast cell
            Cells(i+1)(n-1).alive,  -- South cell
            Cells(i+1)(n-2).alive,  -- Southwest cell

            Cells(i)(n-1).alive
        );
        -- Data output comes from right column
        DataOut(i) <= Cells(i)(n-1).alive;
    end generate;

    -- Handle the corners separately
    -- Top Left Corner
    leftColIn(0) <= Shift and dataIn(0);
    Cell_0_0: Cell port map (
        Tick, Shift,

        leftColIn(0),           -- West cell
        '0',                    -- Northwest cell
        '0',                    -- North cell
        '0',                    -- Northeast cell
        Cells(0)(1).alive,      -- East cell
        Cells(1)(1).alive,      -- Southeast cell
        Cells(1)(0).alive,      -- South cell
        '0',                    -- Southwest cell

        Cells(0)(0).alive
    );
    -- Top Right Corner
    Cell_0_n1: Cell port map (
        Tick, Shift,

        Cells(0)(n-2).alive,    -- West cell
        '0',                    -- Northwest cell
        '0',                    -- North cell
        '0',                    -- Northeast cell
        '0',                    -- East cell
        '0',                    -- Southeast cell
        Cells(1)(n-1).alive,    -- South cell
        Cells(1)(n-2).alive,    -- Southwest cell

        Cells(0)(n-1).alive
    );
    -- Data output comes from right column
    DataOut(0) <= Cells(0)(n-1).alive;
    -- Bottom Right Corner
    Cell_n1_n1: Cell port map (
        Tick, Shift,

        Cells(n-1)(n-2).alive,  -- West cell
        Cells(n-2)(n-2).alive,  -- Northwest cell
        Cells(n-2)(n-1).alive,  -- North cell
        '0',                    -- Northeast cell
        '0',                    -- East cell
        '0',                    -- Southeast cell
        '0',                    -- South cell
        '0',                    -- Southwest cell

        Cells(n-1)(n-1).alive
    );
    -- Data output comes from right column
    DataOut(n-1) <= Cells(n-1)(n-1).alive;
    -- Bottom Left Corner
    leftColIn(n-1) <= Shift and dataIn(n-1);
    Cell_n1_0: Cell port map (
        Tick, Shift,

        leftColIn(n-1),         -- West cell
        '0',                    -- Northwest cell
        Cells(n-2)(0).alive,    -- North cell
        Cells(n-2)(1).alive,    -- Northeast cell
        Cells(n-1)(1).alive,    -- East cell
        '0',                    -- Southeast cell
        '0',                    -- South cell
        '0',                    -- Southwest cell

        Cells(n-1)(0).alive
    );


    -- Fill in the rest of the matrix
    MiddleRowsCells: for i in 1 to n-2 generate
        MiddleCells: for j in 1 to n-2 generate
            Cell_i_j: Cell port map (
                Tick, Shift,

                Cells( i )(j-1).alive,  -- West cell
                Cells(i-1)(j-1).alive,  -- Northwest cell
                Cells(i-1)( j ).alive,  -- North cell
                Cells(i-1)(j+1).alive,  -- Northeast cell
                Cells( i )(j+1).alive,  -- East cell
                Cells(i+1)(j+1).alive,  -- Southeast cell
                Cells(i+1)( j ).alive,  -- South cell
                Cells(i+1)(j-1).alive,  -- Southwest cell

                Cells( i )( j ).alive   -- Current cell
            );
        end generate;
    end generate;

end architecture;
