#
# Makefile to test GCD using GHDL on Mac.
#

TESTFILE = test_system

alutest:
	ghdl -a --ieee=synopsys -fexplicit $(shell find . ! -name "test_regs.*" -name "*.vhd")
	ghdl -e --ieee=synopsys -fexplicit alu_testbench
	ghdl -r alu_testbench

regtest:
	ghdl -a --ieee=synopsys -fexplicit $(shell find . ! -name "test_alu.*" -name "*.vhd")
	ghdl -e --ieee=synopsys -fexplicit regs_testbench
	ghdl -r regs_testbench

clean:
	rm -rf *.o *.cf *.ucf *.lst alu_testbench regs_testbench testcases/__pycache__
