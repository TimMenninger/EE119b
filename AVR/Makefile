#
# Makefile to test GCD using GHDL on Mac.
#

TESTFILE = test_system

test:
	ghdl -a --ieee=synopsys -fexplicit *.vhd
	ghdl -e --ieee=synopsys -fexplicit test_system
	ghdl -r test_system

clean:
	rm -f *.o *.cf *.ucf *.lst test_system
