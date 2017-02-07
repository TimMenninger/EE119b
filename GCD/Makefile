#
# Makefile to test GCD using GHDL on Mac.
#

TB_EXE    	= gcd_tb

test:
	ghdl -a --ieee=synopsys -fexplicit *.vhd
	ghdl -e --ieee=synopsys -fexplicit $(TB_EXE)
	ghdl -r $(TB_EXE)
	make tidy

tidy:
	rm -f *.o *.ucf *.cf $(TOP_VHDL) .DS_Store

clean:
	make tidy
	rm -f $(TB_EXE)
