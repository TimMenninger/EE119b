#
# Makefile to test GCD using GHDL on Mac.
#

ALU_TB_FILE = alu_testbench
REG_TB_FILE = reg_testbench

alutest:
	python testcases/generate_ALU_vectors.py testcases/ALU_instructions.txt testcases/ALU_vectors.txt
	ghdl -a --ieee=synopsys -fexplicit $(shell find . ! -name "test_regs.*" -name "*.vhd")
	ghdl -e --ieee=synopsys -fexplicit $(ALU_TB_FILE)
	ghdl -r $(ALU_TB_FILE) --vcd=$(ALU_TB_FILE).vcd

regtest:
	python testcases/generate_REG_vectors.py testcases/REG_instructions.txt testcases/REG_vectors.txt
	ghdl -a --ieee=synopsys -fexplicit $(shell find . ! -name "test_alu.*" -name "*.vhd")
	ghdl -e --ieee=synopsys -fexplicit $(REG_TB_FILE)
	ghdl -r $(REG_TB_FILE) --vcd=$(REG_TB_FILE).vcd

clean:
	rm -rf *.o *.cf *.ucf *.lst *.vcd $(ALU_TB_FILE) $(REG_TB_FILE) testcases/__pycache__
