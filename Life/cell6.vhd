

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
        DataOut     : out std_logic     -- Shifted out, contains alive/not alive
    );
end entity;

-- The architecture, which updates liveliness on each tick rising edge
architecture update of Cell6 is

    signal alive          : std_logic := '0'; -- State of cell

begin

    -- This process propagates the state of the cell onto the output.  If shift is
    -- active, then we just shift what was on the left to the right cell.  We also
    -- Tell cell above and below how many of (west cell, itself, east cell) are
    -- alive.
    process (Tick) is
        variable surroundings : std_logic_vector(6 downto 0); -- Surrounding cells
    begin
        if (rising_edge(Tick)) then
            -- Data output to N and S cells
            neighbors(1) <= (aliveW and alive)  or
                            (aliveW and aliveE) or
                            (alive  and aliveE);
            neighbors(0) <= (    aliveW and     alive and not aliveE) or
                            (    aliveW and not alive and     aliveE) or
                            (not aliveW and     alive and     aliveE);

            -- Fill surroundings variable with the signals from surrounding cells so we can
            -- do a mux on it to determine liveliness
            surroundings := Shift & aliveW & aliveE & aliveN & aliveS;

            -- Any combination where there are 4+ 1's will cause the cell to die.  Here,
            -- we enumerate all such cases.  Note that the low four bits are two sets
            -- of two-bit numbers, so "000010" means two, not 1, and "001100" means
            -- three
            case surroundings is
                -- When shifting, use value from west side
                when "1------" =>
                    DataOut <= aliveW;

                -- SW, S, SE all alive
                when "01---11" =>
                    DataOut <= '0';
                when "0-1--11" =>
                    DataOut <= '0';
                when "0--1-11" =>
                    DataOut <= '0';
                when "0---111" =>
                    DataOut <= '0';

                -- NW, N, NE all alive
                when "0--11-1" =>
                    DataOut <= '0';
                when "0--111-" =>
                    DataOut <= '0';
                when "0-111--" =>
                    DataOut <= '0';
                when "01-11--" =>
                    DataOut <= '0';

                -- Two of SW, S, SE alive
                when "011--1-" =>
                    DataOut <= '0';
                when "0-1-11-" =>
                    DataOut <= '0';
                when "01--11-" =>
                    DataOut <= '0';

                -- Two of NW, N, NE alive
                when "01-1--1" =>
                    DataOut <= '0';
                when "0-11--1" =>
                    DataOut <= '0';
                when "0111---" =>
                    DataOut <= '0';

                -- Two above and two below alive
                when "0--1-1-" =>
                    DataOut <= '0';

                -- Any combination where 7+ are dead, the cell dies.  We again enumerate
                -- all possible scenarios.
                when "0-00000" =>
                    DataOut <= '0';
                when "00-0000" =>
                    DataOut <= '0';
                when "0000-00" =>
                    DataOut <= '0';
                when "000000-" =>
                    DataOut <= '0';

                -- All other scenarios, the cell lives
                when others =>
                    DataOut <= '1';
            end case;
        end if;
    end process;

end architecture;
