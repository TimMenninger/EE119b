#
# Makefile to test GCD using GHDL on Mac.
#

ALU_TB_FILE = alu_testbench
REG_TB_FILE = reg_testbench
MEM_TB_FILE = mem_testbench

memtest:
	python testcases/generate_MEM_vectors.py testcases/MEM_instructions.txt testcases/MEM_vectors.txt
	make analyze
	ghdl -e --ieee=synopsys -fexplicit $(MEM_TB_FILE)
	make tidy
	ghdl -r $(MEM_TB_FILE) --vcd=$(MEM_TB_FILE).vcd
	python ../fix_vcd.py $(MEM_TB_FILE).vcd

alutest:
	python testcases/generate_ALU_vectors.py testcases/ALU_instructions.txt testcases/ALU_vectors.txt
	make analyze
	ghdl -e --ieee=synopsys -fexplicit $(ALU_TB_FILE)
	make tidy
	ghdl -r $(ALU_TB_FILE) --vcd=$(ALU_TB_FILE).vcd
	python ../fix_vcd.py $(ALU_TB_FILE).vcd

regtest:
	python testcases/generate_REG_vectors.py testcases/REG_instructions.txt testcases/REG_vectors.txt
	make analyze
	ghdl -e --ieee=synopsys -fexplicit $(REG_TB_FILE)
	make tidy
	ghdl -r $(REG_TB_FILE) --vcd=$(REG_TB_FILE).vcd
	python ../fix_vcd.py $(REG_TB_FILE).vcd

analyze:
	ghdl -a --ieee=synopsys -fexplicit --work=common common.vhd
	ghdl -a --ieee=synopsys -fexplicit --work=opcodes opcodes.vhd
	ghdl -a --ieee=synopsys -fexplicit $(shell find . ! -name "opcodes.vhd" ! -name "common.vhd" -name "*.vhd")

tidy:
	rm -rf *.o *.cf *.ucf *.lst .DS_Store testcases/__pycache__

clean:
	rm -rf *.o *.cf *.ucf *.lst *.vcd $(MEM_TB_FILE) $(ALU_TB_FILE) $(REG_TB_FILE) testcases/__pycache__
