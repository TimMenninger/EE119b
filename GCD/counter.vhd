------------------------------------------------------------------------------
--                               counter.vhd                                --
--                      15-bit LFSR counter component                       --
------------------------------------------------------------------------------
--
--  This implements a 15-bit LFSR counter to divide down the system clock for
--  the GCD calculator to a multiplex rate clock (around 1 KHz assuming a 32
--  MHz system clock).  A single pulse one system clock wide is generated
--  (mux_clk) approximately every millisecond (every 32767 system clocks).
--
--  Revision History:
--     29 May 99     Brian Frazier       Created file
--      7 Jun 99     Brian Frazier       Added comments and restructured code
--     13 Jan 08     Glen George         Changed from bit to std_logic and
--                                          restructured code



-- bring in the necessary packages
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.std_logic_unsigned.all;



--  counter
--
--  inputs:
--      sys_clk (std_logic)  -  system clock (should be 32 MHz for 1 KHz
--                                 output multiplex clock)
--  outputs:
--      mux_clk (std_logic)  -  multiplex clock which is one (1) sys_clk wide
--                                 and occurs every 32767 sys_clk's.

entity  counter  is
    port (
        sys_clk  : in   std_logic;
        mux_clk  : out  std_logic
    );
end counter;


--
--  architecture for implementation
--

architecture  archCounter  of  counter  is

    -- bits of the counter
    signal  count :  std_logic_vector(14 downto 0);

begin

    -- simple counter and every is synchronous so do it in one process
    process(sys_clk)
    begin

        -- only change on the rising edge of the system clock
        if  ((sys_clk = '1') and sys_clk'event)  then

            -- generate the multiplex clock for exactly one count value
            if  (count = "100000000000000")  then
                mux_clk <= '1';
            else
                mux_clk <= '0';
            end if;


            -- implement the LFSR

            -- most bits just shift
            count(14 downto 1) <= count(13 downto 0);

            -- low bit has feedback term and handles the illegal LFSR state
            if  (count = "000000000000000")  then
                -- in illegal state - shift in a 1
                count(0) <= '1';
            else
                -- normal state, do the feedback
                count(0) <= count(14) xor count(13);
            end if;
        end if;
    end process;

end archCounter;
