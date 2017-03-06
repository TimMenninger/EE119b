

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- The entity declaration of one cell/processing unit in the game of life
entity Cell6 is
    port (
        Tick        : in  std_logic;    -- Indicates time tick passed, active high
        Shift       : in  std_logic;    -- Shift data through rows (W to E) when high

        aliveW      : in  std_logic;    -- Cell on west side alive when high
        aliveE      : in  std_logic;    -- Cell on east side alive when high
        aliveN      : in  std_logic_vector(1 downto 0); -- Number of cells above alive
        aliveS      : in  std_logic_vector(1 downto 0); -- Number of cells below alive

        neighbors   : out std_logic_vector(1 downto 0); -- 0-2 alive of W, E, itself

        TickOut     : out std_logic;    -- Passes along tick signal horizontally
        ShiftOut    : out std_logic;    -- Passes along shift signal horizontally
        DataOut     : out std_logic     -- Shifted out, contains alive/not alive
    );
end entity;

-- The architecture, which updates liveliness on each tick rising edge
architecture update of Cell6 is

    signal alive          : std_logic := '0'; -- State of cell
    signal surroundings   : std_logic_vector(5 downto 0); -- Surrounding cell lives

begin

    -- This process propagates the state of the cell onto the output.  If shift is
    -- active, then we just shift what was on the left to the right cell.  We also
    -- Tell cell above and below how many of (west cell, itself, east cell) are
    -- alive.
    process (Tick) is
    begin
        if (rising_edge(Tick)) then
            -- Data output to E and W cells
            DataOut <= (aliveW and Shift) or (alive and not Shift);

            -- Data output to N and S cells
            neighbors(1) <= (aliveW and alive)  or
                            (aliveW and aliveE) or
                            (alive  and aliveE);
            neighbors(0) <= (    aliveW and     alive and not aliveE) or
                            (    aliveW and not alive and     aliveE) or
                            (not aliveW and     alive and     aliveE);
        end if;
    end process;

    -- Fill surroundings variable with the signals from surrounding cells so we can
    -- do a mux on it to determine liveliness
    surroundings <= aliveW & aliveE & aliveN & aliveS;

    -- Do 6:1 mux from surroundings to alive output
    with surroundings select alive <=
        -- Any combination where there are 4+ 1's will cause the cell to die.  Here,
        -- we enumerate all such cases.  Note that the low four bits are two sets
        -- of two-bit numbers, so "000010" means two, not 1, and "001100" means
        -- three
        '0' when "1---11",   -- SW, S, SE all alive
        '0' when "-1--11",
        '0' when "--1-11",
        '0' when "---111",

        '0' when "--11-1",   -- NW, N, NE all alive
        '0' when "--111-",
        '0' when "-111--",
        '0' when "1-11--",

        '0' when "11--1-",   -- Two of SW, S, SE alive
        '0' when "-1-11-",
        '0' when "1--11-",

        '0' when "1-1--1",   -- Two of NW, N, NE alive
        '0' when "-11--1",
        '0' when "111---",

        '0' when "--1-1-",   -- Two above and two below alive

        -- Any combination where 7+ are dead, the cell dies.  We again enumerate
        -- all possible scenarios.
        '0' when "-00000",
        '0' when "0-0000",
        '0' when "000-00",
        '0' when "00000-",

        -- All other scenarios, the cell lives
        '1' when others;

end architecture;
