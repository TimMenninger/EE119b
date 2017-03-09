

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- Entity for game of life
entity GameOfLife is
    generic (
        n       : integer                   -- We will create n^2 mesh
    );

    port (
        Tick    : in  std_logic;            -- Cells update on tick rising edge
        Shift   : in  std_logic;            -- Shift data through rows on high
        DataIn  : in  std_logic_vector(n-1 downto 0); -- Data to shift in
        DataOut : out std_logic_vector(n-1 downto 0)  -- Data shifted out
    );
end entity;

-- Architecture to generate 2D mesh
architecture mesh of GameOfLife is

    -- We use the cell component for each value in the mesh
    component Cell8 is
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

    -- This type is everything contained in one cell
    type Cell is
        record
            -- All of these contain whether adjacent cell is dead or alive
            W     : std_logic;
            NW    : std_logic;
            N     : std_logic;
            NE    : std_logic;
            E     : std_logic;
            SE    : std_logic;
            S     : std_logic;
            SW    : std_logic;
            -- The state of this cell
            alive : std_logic;
        end record;

    -- Create the types for a 2D array of cells
    type CellRow   is array(0 to n-1) of Cell;
    type CellArray is array(0 to n-1) of CellRow;

    -- Create the array of cells
    signal Cells : CellArray := (others => (others => (others => '0')));

begin

    -- Connect the Cell processing elements.  We first do the boundary.  We are doing
    -- constant boundary, where everything on the edge thinks its neighbors outside
    -- of the array are dead.

    -- Top row
    TopRowCells: for j in 0 to n-1 generate
        Cell_0_j: Cell8 port map (
            Tick, Shift,
            '0', '0', '0', '0', '0', '0', '0', '0',
            Cells(0)(j).alive
        );
    end generate;

    -- Bottom row
    BottomRowCells: for j in 0 to n-1 generate
        Cell_n1_j: Cell8 port map (
            Tick, Shift,
            '0', '0', '0', '0', '0', '0', '0', '0',
            Cells(n-1)(j).alive
        );
    end generate;

    -- Left and right columns (skipping top and bottom to avoid redundancy)
    SideColumnsCells: for i in 1 to n-2 generate
        -- Left column
        Cell_i_0: Cell8 port map (
            Tick, Shift,
            '0', '0', '0', '0', '0', '0', '0', '0',
            Cells(0)(i).alive
        );
        -- Right column
        Cell_i_n1: Cell8 port map (
            Tick, Shift,
            '0', '0', '0', '0', '0', '0', '0', '0',
            Cells(n-1)(i).alive
        );
    end generate;

    -- Fill in the rest of the matrix
    MiddleRowsCells: for i in 1 to n-2 generate
        MiddleCells: for j in 1 to n-2 generate
            Cell_i_j: Cell8 port map (
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
