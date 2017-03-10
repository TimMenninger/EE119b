#
# Makefile to test GCD using GHDL on Mac.
#

TB_EXE    	= life_tb

analyze:
	ghdl -a --ieee=synopsys -fexplicit *.vhd

test:
	python generate_test_vectors.py life_vectors.txt 10
	ghdl -a --ieee=synopsys -fexplicit *.vhd
	ghdl -e --ieee=synopsys -fexplicit $(TB_EXE)
	ghdl -r $(TB_EXE) --vcd=$(TB_EXE).vcd
	python ../fix_vcd.py $(TB_EXE).vcd
	make tidy

tidy:
	rm -f *.o *.ucf *.cf *.pyc $(TOP_VHDL) .DS_Store

clean:
	make tidy
	rm -f *.vcd $(TB_EXE)
