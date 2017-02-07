-----------------------------------------------------------------------------
--                                muxer.vhd                                --
--                       12-digit Display Multiplexer                      --
-----------------------------------------------------------------------------
-- 
--  This implements the 12 digit display multiplexer for the GCD calculator.
--  The digits to be displayed are held in three (3) 16-bit values (a, b, and
--  r).  To minimize logic only one nibble is ever displayed and the values
--  are shifted into that nibble as the digits are output.
-- 
--  The values in a and b are loaded from keypresses from the key debouncer.
--  The operand switch determines which value the input key is destined for.
--  The result value (r) is loaded from the input result bus whenever the
--  result_rdy signal is active.  The values in a and b are also output so
--  that calculations may be done with them.  Note that due to the shifting
--  the values of a and b are only valid when can_read_vals is active and
--  the new result (r) value will only be loaded at this time as well.
-- 
--  The multiplexer uses a multiplex clock (mux_clk) which should be active
--  for one system clock (sys_clk) appproximately every millisecond.  Each
--  time the multiplex clock goes active a new digit will be output.
-- 
--  Revision History
--     29 May 99     Brian Frazier       Created file
--     30 May 99     Glen George         Modified contents to work with the
--                                          Altera tools (added "wait until"
--                                          statements)
--      2 Jun 99     Brian Frazier       Updated digit decoding and switch
--                                          polarity
--      7 Jun 99     Brian Frazier       Added comments and restructured code
--     15 Jan 08     Glen George         Changed from bit to std_logic and
--                                          restructured code



-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;



--  muxer
--
--  inputs:
--      sys_clk (std_logic)                     -  system clock
--      value (std_logic_vector(3 downto 0))    -  current key value
--      key (std_logic)                         -  a key is available (value
--                                                 bus is valid)
--      operand (std_logic)                     -  current setting of operand
--                                                 switch (used to determine
--                                                 which operand to put the
--                                                 new key into)
--      result (std_logic_vector(15 downto 0))  -  result to be displayed
--      result_rdy (std_logic)                  -  a result is available
--                                                 (result bus is valid)
--      mux_clk (std_logic)                     -  multiplex clock
--
--  outputs:
--      a (std_logic_vector(15 downto 0))       -  first operand to work with
--      b (std_logic_vector(15 downto 0))       -  second operand to work with
--      can_read_vals (std_logic)               -  operand values (a and b)
--                                                 are valid
--      used_key (std_logic)                    -  handshake for keypad logic,
--                                                 indicates have used the key
--                                                 value
--      digit (std_logic_vector(11 downto 0))   -  decoded digit to currently
--                                                 mux (1 hot shift register)
--      segmenta (std_logic)                    -  segment a of the display
--      segmentb (std_logic)                    -  segment b of the display
--      segmentc (std_logic)                    -  segment c of the display
--      segmentd (std_logic)                    -  segment d of the display
--      segmente (std_logic)                    -  segment e of the display
--      segmentf (std_logic)                    -  segment f of the display
--      segmentg (std_logic)                    -  segment g of the display

entity  muxer  is
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
end muxer;


--
--  architecture for implementation
--

architecture  archMultiplexer  of  muxer  is

    -- constants indicating which operand is being input
    constant  OP_A : std_logic := '0';       -- first operand
    constant  OP_B : std_logic := '1';       -- second operand

    -- the result stored internally
    signal  r     : std_logic_vector(15 downto 0);

    -- vectors holding the new values for the a/b/r shift register
    signal  new_a : std_logic_vector(15 downto 0);
    signal  new_b : std_logic_vector(15 downto 0);
    signal  new_r : std_logic_vector(15 downto 0);

begin

    -- can read the values of a and b when they are in the "correct" position
    --   in the shift register made up of a, b, and r - this is when we are on
    --   the first digit of multiplexing
    can_read_vals <= (digit(0) and not mux_clk);


    -- concurrent logic used for shifting a, b, and result
    --   shift order is r <- b <- a <- r, by nibbles
    -- shifting occurs when mux_clk is active
    -- shift the new key value into low nibble of a or b if on first mux digit
    --   (everything is in the "right" place), this also causes the other
    --   nibbles in a or b to shift an extra nibble
    -- shift incoming result into r if it is ready and on first mux digit

    -- a: if new digit for a     a(7..0) | value | r(15..12)
    --    otherwise              a(11..0) | r(15..12)
    new_a <=        a(7 downto 0) & value(3 downto 0) & r(15 downto 12)
                          when  ((mux_clk = '1') and (digit(0) = '1') and
                                 (key = '1') and (operand = OP_A))
              else  a(11 downto 0) & r(15 downto 12)
                          when  ((mux_clk = '1') and
                                 ((digit(0) = '0') or (key = '0') or
                                  (operand = OP_B)))
              else  a;

    -- b: if new digit for b     b(7..0) | value | a(15..12)
    --    if new digit for a     b(11..0) | a(11..8)
    --    otherwise              b(11..0) | a(15..12)
    new_b <=        b(7 downto 0) & value(3 downto 0) & a(15 downto 12)
                          when  ((mux_clk = '1') and (digit(0) = '1') and
                                 (key = '1') and (operand = OP_B))
              else  b(11 downto 0) & a(11 downto 8)
                          when  ((mux_clk = '1') and (digit(0) = '1') and
                                 (key = '1') and (operand = OP_A))
              else  b(11 downto 0) & a(15 downto 12)
                          when  ((mux_clk = '1') and
                                 ((digit(0) = '0') or (key = '0')))
              else  b;

    -- r: if have a result       result
    --    if new digit for b     r(11..0) | b(11..8)
    --    otherwise              r(11..0) | b(15..12)
    new_r <=        result
                          when  ((mux_clk = '0') and (digit(0) = '1') and
                                 (result_rdy = '1'))
              else  r(11 downto 0) & b(11 downto 8)
                          when  ((mux_clk = '1') and (digit(0) = '1') and
                                 (key = '1') and (operand = OP_B))
              else  r(11 downto 0) & b(15 downto 12)
                          when  ((mux_clk = '1') and
                                 ((digit(0) = '0') or (key = '0') or
                                  (operand = OP_A)))
              else  r;


    -- we have used the key any time we're on digit 0 and have a multiplex
    --   clock and there was a key (that means the key has been shifted in)
    used_key <= (digit(0) and mux_clk and key);


    -- segment decoding logic
    -- the low nibble of a is always output (the appropriate digit is shifted
    --   into that position)
    -- for each segment just determine which digit values turn it off (driver
    --   on the board actually inverts the signal)
    segmenta <=       '1' when ((a(3 downto 0) = "0001") or
                                (a(3 downto 0) = "0100") or
                                (a(3 downto 0) = "1011") or
                                (a(3 downto 0) = "1101"))
                else  '0';
    segmentb <=       '1' when ((a(3 downto 0) = "0101") or
                                (a(3 downto 0) = "0110") or
                                (a(3 downto 0) = "1011") or
                                (a(3 downto 0) = "1100") or
                                (a(3 downto 0) = "1110") or
                                (a(3 downto 0) = "1111"))
                else  '0';
    segmentc <=       '1' when ((a(3 downto 0) = "0010") or
                                (a(3 downto 0) = "1100") or
                                (a(3 downto 0) = "1110") or
                                (a(3 downto 0) = "1111"))
                else  '0';
    segmentd <=       '1' when ((a(3 downto 0) = "0001") or
                                (a(3 downto 0) = "0100") or
                                (a(3 downto 0) = "0111") or
                                (a(3 downto 0) = "1001") or
                                (a(3 downto 0) = "1010") or
                                (a(3 downto 0) = "1111"))
                else  '0';
    segmente <=       '1' when ((a(3 downto 0) = "0001") or
                                (a(3 downto 0) = "0011") or
                                (a(3 downto 0) = "0100") or
                                (a(3 downto 0) = "0101") or
                                (a(3 downto 0) = "0111") or
                                (a(3 downto 0) = "1001"))
                else  '0';
    segmentf <=       '1' when ((a(3 downto 0) = "0001") or
                                (a(3 downto 0) = "0010") or
                                (a(3 downto 0) = "0011") or
                                (a(3 downto 0) = "0111") or
                                (a(3 downto 0) = "1101"))
                else  '0';
    segmentg <=       '1' when ((a(3 downto 0) = "0000") or
                                (a(3 downto 0) = "0001") or
                                (a(3 downto 0) = "0111") or
                                (a(3 downto 0) = "1100"))
                else  '0';


    -- process to handle simple DFF's for storing values
    process(sys_clk)
    begin

        -- clock on the rising edge
        if  ((sys_clk = '1') and sys_clk'event)  then

            -- handle the shift register
            a <= new_a;
            b <= new_b;
            r <= new_r;

            -- digit multiplexing
            -- one-hot shift register that self clears
            --   only shifts when mux_clk is active
            if  (mux_clk = '1')  then
                digit(10 downto 0) <=  digit(11 downto 1);
                if  (digit(11 downto 1) = "00000000000")  then
                    digit(11) <= '1';
                else
                    digit(11) <= '0';
                end if;
            else
                digit(11 downto 0) <=  digit(11 downto 0);
            end if;
        end if;
    end process;
    
end archMultiplexer;
