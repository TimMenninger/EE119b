-----------------------------------------------------------------------------
--                                system.vhd                               --
--                              GCD Calculator                             --
-----------------------------------------------------------------------------
--
--  This is the main system entity for the GCD calculator.  It connects
--  together each of the individual blocks that make up the calculator.  The
--  entities used are:
--     counter - a divider to generate the 1 KHz scan and multiplex clock
--     muxer   - the display multiplexer
--     keypad  - the keypad scanner and debouncer
--
--  To implement the GCD calculator, the GCD entity must be hooked into this
--  entity.  It will need to use signals from the muxer as well as the system
--  clock.  The calculate input is used to tell when the user has requested a
--  GCD to be calculated (active low).  The GCD block may only use the a and b
--  operand values from the muxer block when the can_read_vals signal is
--  active.  Further the muxer block will only load the result from the GCD
--  block when the result_rdy and can_read_vals signals are both active.
--
--  Revision HIstory
--     29 May 99     Brian Frazier       Created file
--     30 May 99     Glen George         Modified contents to work with  the
--                                          Altera tools
--      2 Jun 99     Brian Frazier       Updated signals to included calculate
--                                          button.
--      7 Jun 99     Brian Frazier       Added comments and restructured code
--     13 Jan 08     Glen George         Changed from bit to std_logic and
--                                          restructured code
--     15 Jan 08     Glen George         Added decimal point segment and
--                                          simple example connections for the
--                                          result



-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;



--  system
--
--  inputs:
--      sysclk (std_logic)                      -  system clock
--      row (std_logic_vector(3 downto 0))      -  the row inputs from the
--                                                 keypad
--      operand (std_logic)                     -  operand select switch
--      calculate (std_logic)                   -  calculate switch
--
--  outputs:
--      col (std_logic_vector(3 downto 0))      -  column of the keypad to
--                                                 scan (read)
--      digit (std_logic_vector(11 downto 0))   -  decoded digit to currently
--                                                 mux (1 hot shift register)
--      segmenta (std_logic)                    -  segment a of the display
--      segmentb (std_logic)                    -  segment b of the display
--      segmentc (std_logic)                    -  segment c of the display
--      segmentd (std_logic)                    -  segment d of the display
--      segmente (std_logic)                    -  segment e of the display
--      segmentf (std_logic)                    -  segment f of the display
--      segmentg (std_logic)                    -  segment g of the display
--      segmentdp (std_logic)                   -  decimal point of display

entity  system  is
    port (

        sysclk    : in  std_logic;

        row       : in  std_logic_vector(3 downto 0);

        operand   : in  std_logic;
        calculate : in  std_logic;

        col       : buffer  std_logic_vector(3 downto 0);

        digit     : buffer  std_logic_vector(11 downto 0);

        segmenta  : out  std_logic;
        segmentb  : out  std_logic;
        segmentc  : out  std_logic;
        segmentd  : out  std_logic;
        segmente  : out  std_logic;
        segmentf  : out  std_logic;
        segmentg  : out  std_logic;
        segmentdp : out  std_logic
    );
end system;


--
--  structural architecture for implementation
--

architecture  structural  of  system  is

    -- components
    -- Keypad component
    component  keypad  is
        port (
            sys_clk  : in  std_logic;
            scan_clk : in  std_logic;
            row      : in std_logic_vector(3 downto 0);
            gotkey   : in std_logic;
            col      : buffer std_logic_vector(3 downto 0);
            keyavail : out std_logic;
            keyvalue : out std_logic_vector(3 downto 0)
        );
    end component;
    -- Muxer component
    component  muxer  is
        port (
            sys_clk       : in  std_logic;
            value         : in  std_logic_vector(3 downto 0);
            key           : in  std_logic;
            operand       : in  std_logic;
            result        : in  std_logic_vector(15 downto 0);
            result_rdy    : in  std_logic;
            mux_clk       : in  std_logic;
            a             : buffer  std_logic_vector(15 downto 0);
            b             : buffer  std_logic_vector(15 downto 0);
            can_read_vals : out  std_logic;
            used_key      : out  std_logic;
            digit         : buffer  std_logic_vector(11 downto 0);
            segmenta      : out  std_logic;
            segmentb      : out  std_logic;
            segmentc      : out  std_logic;
            segmentd      : out  std_logic;
            segmente      : out  std_logic;
            segmentf      : out  std_logic;
            segmentg      : out  std_logic
        );
    end component;
    -- counter component
    component  counter  is
        port (
            sys_clk  : in   std_logic;
            mux_clk  : out  std_logic
        );
    end component;
    -- GCD component
    component  GCD  is
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

    -- interconnects

    -- key available from keypad
    signal key      : std_logic;
    -- acknowledgement of key available from muxer
    signal key_ack  : std_logic;
    -- value read from keypad
    signal keyvalue : std_logic_vector(3 downto 0);

    -- signal from multiplexer signalling that "a" and "b" can be read.
    signal can_read_vals : std_logic;

    -- ties values a and b and the result between components
    signal a : std_logic_vector(15 downto 0);
    signal b : std_logic_vector(15 downto 0);
    signal r : std_logic_vector(15 downto 0);

    -- result ready signal from GCD component
    signal result_rdy : std_logic;

    -- approximately 1 KHz multiplex clock signal
    signal mux_clk : std_logic;

begin

    -- don't ever output decimal points (remember segments are active low)
    segmentdp <= '1';

    -- instantiate and connect the blocks (entities)
    Cntr:  counter port map(sysclk, mux_clk);
    Kypd:  keypad  port map(sysclk, mux_clk, row, key_ack, col, key,
                                   keyvalue);
    Muxr:  muxer   port map(sysclk, keyvalue, key, operand,
                                   r, result_rdy, mux_clk,
                                   a, b, can_read_vals, key_ack, digit,
                                   segmenta, segmentb, segmentc, segmentd,
                                   segmente, segmentf, segmentg);
    Gcdr:  GCD     port map(sysclk, a, b, can_read_vals, calculate,
                                   r, result_rdy);

end structural;
