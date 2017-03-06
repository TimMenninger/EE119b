

-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;

-- Entity for game of life
entity GameOfLife is
    generic (
        n   : integer   -- We will create n^2 mesh
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

            TickOut     : out std_logic;    -- Passes along tick signal horizontally
            ShiftOut    : out std_logic;    -- Passes along shift signal horizontally
            DataOut     : out std_logic     -- Shifted out, contains alive/not alive
        );
    end component;

begin
end architecture;
