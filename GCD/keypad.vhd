-----------------------------------------------------------------------------
--                              keypad.vhd                                 --
--                           Keypad Debouncing                             --
-----------------------------------------------------------------------------
--
--  This implements a 4x4 keypad scanner and debouncer for the GCD calculator.
--  The debouncing is done with a simple counter/state machine.  The circuit
--  expects an input signal (scan_clk) which is active for one system clock
--  (sys_clk) approximately every millisecond.  This is used to time the
--  switch scanning and debouncing.  The circuit outputs a key available
--  signal along with the 4-bit keycode of the debounced key.  This signal
--  will be active until it is acknowledged and the debounced key will be
--  remembered until then as well - scanning stops until it is acknowledged
--  that the debounced key has been read.
--
--  Autorepeat is not supported.
--
--  Revision History:
--     29 May 99     Brian Frazier       Created file
--     30 May 99     Glen George         Modified contents to work with the
--                                          Altera tools
--      2 Jun 99     Brian Frazier       Updated signals and key decoding
--                                          logic
--      7 Jun 99     Brian Frazier       Added comments and restructured code
--     13 Jan 08     Glen George         Changed from bit to std_logic and
--                                          restructured code
--     15 Jan 08     Glen George         Cleaned up logic a bit for column
--                                          scanning



-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;



--  keypad
--
--  inputs:
--      sys_clk (std_logic)                     -  system clock
--      scan_clk (std_logic)                    -  scanning and debouncing
--                                                 clock (about 1 KHz)
--      row (std_logic_vector(3 downto 0))      -  the row inputs from the
--                                                 keypad
--      gotkey (std_logic)                      -  indicates the debounced key
--                                                 has been read
--
--  outputs:
--      col (std_logic_vector(3 downto 0))      -  column of the keypad to
--                                                 scan (read)
--      keyavail (std_logic)                    -  key is available to be read
--      keyvalue (std_logic_vector(3 downto 0)) -  key value that has been
--                                                 debounced

entity  keypad  is
    port (

        sys_clk  : in  std_logic;
        scan_clk : in  std_logic;

        row      : in std_logic_vector(3 downto 0);

        gotkey   : in std_logic;

        col      : buffer std_logic_vector(3 downto 0);

        keyavail : out std_logic;
        keyvalue : out std_logic_vector(3 downto 0)
    );
end keypad;


--
--  architecture for implementation
--

architecture  archKeypad  of  keypad  is

    -- state defintions/declarations
    subtype  state  is  std_logic_vector(4 downto 0);-- state type

    constant  SCANNING   : state := "00000";        -- scanning the keypad
    constant  START_DEB  : state := "00001";        -- start debouncing a key
    constant  DEBOUNCED  : state := "10000";        -- have a debounced key
    constant  WAIT_REL   : state := "10001";        -- waiting for key release

    signal    cur_state  : state;                   -- current state
    signal    next_state : state;                   -- next state

    -- last value read for the row (used for debouncing)
    signal  last_row : std_logic_vector(3 downto 0);
    -- value to store in last_row
    signal  store_row : std_logic_vector(3 downto 0);

    -- next column to scan
    signal  next_col : std_logic_vector(3 downto 0);

begin

    -- encode the value of the key being scanned into a 4-bit value
    keyvalue(3) <= not col(3) or not col(2);
    keyvalue(2) <= not col(3) or not col(1);
    keyvalue(1) <= not last_row(3) or not last_row(2);
    keyvalue(0) <= not last_row(3) or not last_row(1);

    -- a key is available when in the debounced state
    keyavail <=       '1'  when (cur_state = DEBOUNCED)
                else  '0';


    -- state transitions for the scanning and debouncing state machine
    transition:  process (cur_state, row, last_row, scan_clk, gotkey)
    begin

        -- state transitions
        case  cur_state  is         -- do the state transitions

            when  SCANNING =>       -- scanning the keypad
                if  ((row = "1110") or (row = "1101") or (row = "1011") or (row = "0111"))  then
                    -- have a valid key press
                    -- start debouncing
                    next_state <= START_DEB;
                else
                    -- just keep scanning
                    next_state <= SCANNING;
                end if;

            when  DEBOUNCED =>      -- keypad input is debounced

                if  (gotkey = '0')  then
                    -- no acknowledgement, consider key still debounced
                    next_state <= DEBOUNCED;
                else
                    -- key has been acknowledged, is it still down
                    if  (row = last_row)  then
                        -- key is still down, wait for it to be released
                        next_state <= WAIT_REL;
                    else
                        -- key no longer down, start scanning again
                        next_state <= SCANNING;
                    end if;
                end if;

            when  WAIT_REL =>       -- waiting for key release

                if  (row = last_row)  then
                    -- key is still down, still wait for release
                    next_state <= WAIT_REL;
                else
                    -- key is up, can start scanning again
                    next_state <= SCANNING;
                end if;

            when  others =>          -- must be debouncing the key

                if  (row = last_row)  then
                    -- key is still down, keep debouncing (count scan clocks)
                    if  (scan_clk = '1')  then
                        next_state <= cur_state + 1;
                    else
                        next_state <= cur_state;
                    end if;
                else
                    -- key is not down anymore, go back to scanning
                    next_state <= SCANNING;
                end if;

        end case;
    end process transition;


    -- state outputs for the scanning and debouncing state machine
    outputs:  process (cur_state, row, last_row, col, scan_clk)
    begin

        -- state outputs
        case  cur_state  is         -- do the state outputs

            when  SCANNING =>       -- scanning the keypad

                -- always store the current row value in the scanning state
                store_row <= row;

                -- check if no key is pressed in this column and time to scan
                --   to the next column
                if  ((row = "1111") and (scan_clk = '1'))  then
                    -- nothing pressed in this column and time to move to next

                    -- scan the next column (active low)
                    --   this is self clearing in that if it ends up in an
                    --   illegal state it, it will correct it
                    if  (col = "1011")  then
                        next_col <= "0111";
                    elsif  (col = "1101")  then
                        next_col <= "1011";
                    elsif  (col = "1110")  then
                        next_col <= "1101";
                    else
                        next_col <= "1110";
                    end if;

                else
                    -- a key is pressed or it's not time to scan a new column
                    --   stay on this column
                    next_col <= col;
                end if;

            when  others =>          -- any other state

                -- any other state, keep the column and last row value as is
                next_col <= col;
                store_row <= last_row;

        end case;
    end process outputs;


    -- process to handle simple DFF's for storing values
    process(sys_clk)
    begin

        -- clock on the rising edge
        if  ((sys_clk = '1') and sys_clk'event)  then

            -- update the scanning column
            col <= next_col;

            -- store the new row information
            last_row <= store_row;

            -- store the new state
            cur_state <= next_state;
        end if;
    end process;

end archKeypad;
