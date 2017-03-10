

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- The entity declaration of one cell/processing unit in the game of life
entity Cell is
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
architecture update of Cell is

    signal alive            : std_logic := '0'; -- state of the cell
    signal nextAlive        : std_logic := '0'; -- next state of cell
    signal surroundings     : std_logic_vector(7 downto 0); -- Surrounding cell lives

begin
    -- Always put alive signal to DataOut
    DataOut <= alive;

    -- This process propagates the state of the cell onto the output.  If shift is
    -- active, then we just shift what was on the left to the right cell.
    process (Tick) is
    begin
        -- Only update on rising edge of clock
        if (rising_edge(Tick)) then
            alive <= (nextAlive and not Shift) or (aliveW and Shift);
        end if;
    end process;

    -- Fill surroundings variable with the signals from surrounding cells so we can
    -- do a mux on it to determine liveliness
    surroundings <= aliveW & aliveNW & aliveN & aliveNE &
                    aliveE & aliveSE & aliveS & aliveSW;

    -- Do 8:1 mux from surroundings to alive output.  We count the number of 1's
    -- in this case, where each bit is the state of an adjacent cell.
    with surroundings select nextAlive <=
        -- Any combination where there are 4+ 1's will cause the cell to die.  Here,
        -- we enumerate all such cases.
        '0' when "11100001",
        '0' when "11100011",
        '0' when "11100101",
        '0' when "11100111",
        '0' when "11101001",
        '0' when "11101011",
        '0' when "11101101",
        '0' when "11101111",
        '0' when "11110001",
        '0' when "11110011",
        '0' when "11110101",
        '0' when "11110111",
        '0' when "11111001",
        '0' when "11111011",
        '0' when "11111101",
        '0' when "11111111",
        '0' when "11010001",
        '0' when "11010011",
        '0' when "11010101",
        '0' when "11010111",
        '0' when "11011001",
        '0' when "11011011",
        '0' when "11011101",
        '0' when "11011111",
        '0' when "10110001",
        '0' when "10110011",
        '0' when "10110101",
        '0' when "10110111",
        '0' when "10111001",
        '0' when "10111011",
        '0' when "10111101",
        '0' when "10111111",
        '0' when "01110001",
        '0' when "01110011",
        '0' when "01110101",
        '0' when "01110111",
        '0' when "01111001",
        '0' when "01111011",
        '0' when "01111101",
        '0' when "01111111",
        '0' when "11100010",
        '0' when "11100110",
        '0' when "11101010",
        '0' when "11101110",
        '0' when "11110010",
        '0' when "11110110",
        '0' when "11111010",
        '0' when "11111110",
        '0' when "11010010",
        '0' when "11010110",
        '0' when "11011010",
        '0' when "11011110",
        '0' when "10110010",
        '0' when "10110110",
        '0' when "10111010",
        '0' when "10111110",
        '0' when "01110010",
        '0' when "01110110",
        '0' when "01111010",
        '0' when "01111110",
        '0' when "11000011",
        '0' when "11000111",
        '0' when "11001011",
        '0' when "11001111",
        '0' when "10100011",
        '0' when "10100111",
        '0' when "10101011",
        '0' when "10101111",
        '0' when "10010011",
        '0' when "10010111",
        '0' when "10011011",
        '0' when "10011111",
        '0' when "01100011",
        '0' when "01100111",
        '0' when "01101011",
        '0' when "01101111",
        '0' when "01010011",
        '0' when "01010111",
        '0' when "01011011",
        '0' when "01011111",
        '0' when "00110011",
        '0' when "00110111",
        '0' when "00111011",
        '0' when "00111111",
        '0' when "11100100",
        '0' when "11101100",
        '0' when "11110100",
        '0' when "11111100",
        '0' when "11010100",
        '0' when "11011100",
        '0' when "10110100",
        '0' when "10111100",
        '0' when "01110100",
        '0' when "01111100",
        '0' when "11000101",
        '0' when "11001101",
        '0' when "10100101",
        '0' when "10101101",
        '0' when "10010101",
        '0' when "10011101",
        '0' when "01100101",
        '0' when "01101101",
        '0' when "01010101",
        '0' when "01011101",
        '0' when "00110101",
        '0' when "00111101",
        '0' when "11000110",
        '0' when "11001110",
        '0' when "10100110",
        '0' when "10101110",
        '0' when "10010110",
        '0' when "10011110",
        '0' when "01100110",
        '0' when "01101110",
        '0' when "01010110",
        '0' when "01011110",
        '0' when "00110110",
        '0' when "00111110",
        '0' when "10000111",
        '0' when "10001111",
        '0' when "01000111",
        '0' when "01001111",
        '0' when "00100111",
        '0' when "00101111",
        '0' when "00010111",
        '0' when "00011111",
        '0' when "11101000",
        '0' when "11111000",
        '0' when "11011000",
        '0' when "10111000",
        '0' when "01111000",
        '0' when "11001001",
        '0' when "10101001",
        '0' when "10011001",
        '0' when "01101001",
        '0' when "01011001",
        '0' when "00111001",
        '0' when "11001010",
        '0' when "10101010",
        '0' when "10011010",
        '0' when "01101010",
        '0' when "01011010",
        '0' when "00111010",
        '0' when "10001011",
        '0' when "01001011",
        '0' when "00101011",
        '0' when "00011011",
        '0' when "11001100",
        '0' when "10101100",
        '0' when "10011100",
        '0' when "01101100",
        '0' when "01011100",
        '0' when "00111100",
        '0' when "10001101",
        '0' when "01001101",
        '0' when "00101101",
        '0' when "00011101",
        '0' when "10001110",
        '0' when "01001110",
        '0' when "00101110",
        '0' when "00011110",
        '0' when "00001111",
        '0' when "11110000",

        -- Any case with <2 live neighbors also dies.  For this, we enumerate every
        -- possible orientation with 7 dead neighbors.  There are 8 choose 7 = 8
        '0' when "00000000",
        '0' when "00000001",
        '0' when "00000010",
        '0' when "00000100",
        '0' when "00001000",
        '0' when "00010000",
        '0' when "00100000",
        '0' when "01000000",
        '0' when "10000000",

        -- Any case with exactly 2 live neighbors either stays dead or stays alive
        alive when "00000011",
        alive when "00000101",
        alive when "00001001",
        alive when "00010001",
        alive when "00100001",
        alive when "01000001",
        alive when "10000001",
        alive when "00000110",
        alive when "00001010",
        alive when "00010010",
        alive when "00100010",
        alive when "01000010",
        alive when "10000010",
        alive when "00001100",
        alive when "00010100",
        alive when "00100100",
        alive when "01000100",
        alive when "10000100",
        alive when "00011000",
        alive when "00101000",
        alive when "01001000",
        alive when "10001000",
        alive when "00110000",
        alive when "01010000",
        alive when "10010000",
        alive when "01100000",
        alive when "10100000",
        alive when "11000000",

        -- All other scenarios, the cell lives
        '1' when others;

end architecture;
