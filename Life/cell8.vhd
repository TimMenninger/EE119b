

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- The entity declaration of one cell/processing unit in the game of life
entity Cell8 is
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
end entity;

-- The architecture, which updates liveliness on each tick rising edge
architecture update of Cell8 is

    signal alive          : std_logic := '0'; -- State of cell
    signal surroundings   : std_logic_vector(7 downto 0); -- Surrounding cell lives

begin

    -- This process propagates the state of the cell onto the output.  If shift is
    -- active, then we just shift what was on the left to the right cell.
    process (Tick) is
        variable surroundings : std_logic_vector(8 downto 0); -- Surrounding cells
    begin
        -- Only update on rising edge of clock
        if (rising_edge(Tick)) then
            DataOut <= (aliveW and Shift) or (alive and not Shift);
        end if;
    end process;

    -- Fill surroundings variable with the signals from surrounding cells so we can
    -- do a mux on it to determine liveliness
    surroundings <= aliveW & aliveNW & aliveN & aliveNE &
                    aliveE & aliveSE & aliveS & aliveSW;

    -- Do 8:1 mux from surroundings to alive output.  We count the number of 1's
    -- in this case, where each bit is the state of an adjacent cell.
    with surroundings select alive <=
        -- Any combination where there are 4+ 1's will cause the cell to die.  Here,
        -- we enumerate all such cases.  There are 8 choose 4 = 70.
        '0' when "111----1",
        '0' when "11-1---1",
        '0' when "1-11---1",
        '0' when "-111---1",
        '0' when "111---1-",
        '0' when "11-1--1-",
        '0' when "1-11--1-",
        '0' when "-111--1-",
        '0' when "11----11",
        '0' when "1-1---11",
        '0' when "1--1--11",
        '0' when "-11---11",
        '0' when "-1-1--11",
        '0' when "--11--11",
        '0' when "111--1--",
        '0' when "11-1-1--",
        '0' when "1-11-1--",
        '0' when "-111-1--",
        '0' when "11---1-1",
        '0' when "1-1--1-1",
        '0' when "1--1-1-1",
        '0' when "-11--1-1",
        '0' when "-1-1-1-1",
        '0' when "--11-1-1",
        '0' when "11---11-",
        '0' when "1-1--11-",
        '0' when "1--1-11-",
        '0' when "-11--11-",
        '0' when "-1-1-11-",
        '0' when "--11-11-",
        '0' when "1----111",
        '0' when "-1---111",
        '0' when "--1--111",
        '0' when "---1-111",
        '0' when "111-1---",
        '0' when "11-11---",
        '0' when "1-111---",
        '0' when "-1111---",
        '0' when "11--1--1",
        '0' when "1-1-1--1",
        '0' when "1--11--1",
        '0' when "-11-1--1",
        '0' when "-1-11--1",
        '0' when "--111--1",
        '0' when "11--1-1-",
        '0' when "1-1-1-1-",
        '0' when "1--11-1-",
        '0' when "-11-1-1-",
        '0' when "-1-11-1-",
        '0' when "--111-1-",
        '0' when "1---1-11",
        '0' when "-1--1-11",
        '0' when "--1-1-11",
        '0' when "---11-11",
        '0' when "11--11--",
        '0' when "1-1-11--",
        '0' when "1--111--",
        '0' when "-11-11--",
        '0' when "-1-111--",
        '0' when "--1111--",
        '0' when "1---11-1",
        '0' when "-1--11-1",
        '0' when "--1-11-1",
        '0' when "---111-1",
        '0' when "1---111-",
        '0' when "-1--111-",
        '0' when "--1-111-",
        '0' when "---1111-",
        '0' when "----1111",
        '0' when "1111----",

        -- Any case with <2 live neighbors also dies.  For this, we enumerate every
        -- possible orientation with 7 dead neighbors.  There are 8 choose 7 = 8
        '0' when "0000000-",
        '0' when "000000-0",
        '0' when "00000-00",
        '0' when "0000-000",
        '0' when "000-0000",
        '0' when "00-00000",
        '0' when "0-000000",
        '0' when "-0000000",

        -- All other scenarios, the cell lives
        '1' when others;

end architecture;
