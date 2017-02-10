import sys
import random
from generate_values import *

dontCareAddr = [ "LDI", "MOV" ]
dontCareData = [ "POP", "LDX", "LDXI", "LDXD", "LDYI", "LDYD", "LDDY",
                 "LDZI", "LDZD", "LDDZ", "LDI", "LDS", "MOV" ]
preDecInst = [ "LDXD", "LDYD", "LDZD", "STXD", "STYD", "STZD" ]
postIncInst = [ "LDXI", "LDYI", "LDZI", "STXI", "STYI", "STZI" ]

def generate(fIn, fOut):
    fIn.readline() # First line is a comment
    line = fIn.readline()
    skip = False
    status = ["-"] * 8

    # Dictionary of known values in memory
    memory = {}
    keys = []

    # Invalid register values to avoid assuming previous values
    registers = [ -1 ] * 32

    # Stack pointer starts as all 1's
    SP = binary_str_to_int("1111111111111111")
    stack = []

    # Start with random values in X, Y and Z for data accessing
    for i in range(26, 32):
        registers[i] = random.randint(0, 255)

    # Go to end of file or till END (for shortening without deleting)
    while (line != "" and "END" not in line):
        if ("SKIP" in line):
            skip = not skip

        if (line == "\n" or skip):
            line = fIn.readline()
            continue

        vals = line.split(" ")

        # Get arguments
        inA = int(vals[1])
        inB = int(vals[2])

        # Assume not reading or writing
        expRd = "1"
        expWr = "1"

        # Generate opcode
        opcode = compute_instruction(vals[0], inA, inB)

        # Number of clocks
        numClks = int_to_binary(instructions[vals[0]][1] - 1, 2)
        numClks = [ str(i) for i in numClks ]
        numClks = "".join(numClks)

        # If X Y or Z is in the instruction, that is our address
        expAddr = 0
        useReg = False

        # increment or decrement appropriately
        preDec = 0
        if (vals[0] in preDecInst):
            preDec = -1
        postInc = 0
        if (vals[0] in postIncInst):
            postInc = 1

        if ("X" in vals[0]):
            registers[26] += preDec
            expAddr = registers[27] * 256 + registers[26]
            registers[26] += postInc
            if (registers[26] == 0):
                registers[27] += postInc
            useReg = True
        if ("Y" in vals[0]):
            registers[28] += preDec
            expAddr = registers[29] * 256 + registers[28]
            registers[28] += postInc
            if (registers[28] == 0):
                registers[29] += postInc
            useReg = True
        if ("Z" in vals[0]):
            registers[30] += preDec
            expAddr = registers[31] * 256 + registers[30]
            registers[30] += postInc
            if (registers[30] == 0):
                registers[31] += postInc
            useReg = True

        # Figure out the memory input.  This is progDB and only used in LDS and STS
        mem = "0000000000000000"
        if (vals[0] in [ "LDS", "STS" ]):
            expAddr = random.randint(0, 2**16-1)

        # The expected address is the address sent to memory
        if (vals[0] in [ "STDY", "STDZ", "LDDY", "LDDZ" ]):
            expAddr += inB

        # Data is in regs[A] for load instructions
        expData = 0
        if (vals[0][:2] == "ST"):
            expData = registers[inA]
            if (vals[0][2] == "S"):
                # Store into memory, save upper bits for stack
                expAddr = random.randint(0, 2**15)
            memory[expAddr] = registers[inA]
            keys.append(expAddr)
            random.shuffle(keys) # so it's not a queue
            expWr = "0"
        elif (vals[0] == "PUSH"):
            expData = registers[inA]
            stack.append(registers[inA])
            expWr = "0"
            expAddr = SP
            SP -= 1
        elif (vals[0][:2] == "LD"):
            expRd = "0"
            if (useReg):
                registers[inA] = memory[expAddr]
            elif (vals[0] == "LDI"):
                registers[inA] = inB
                expRd = "1"
            else:
                registers[inA] = memory[keys[0]]
                keys = keys[1:]
            expData = registers[inA]
        elif (vals[0] == "POP"):
            registers[inA] = stack[-1]
            stack = stack[:-1]
            expData = registers[inA]
            expRd = "0"
            SP += 1
            expAddr = SP
        elif (vals[0] == "MOV"):
            registers[inA] = registers[inB]
            expData = registers[inA]

        # Convert expected address to a string
        expAddr = int_to_binary(expAddr, 16)
        expAddr = [ str(i) for i in expAddr ]
        expAddr = "".join(expAddr)

        # Also expData
        expData = int_to_binary(expData, 8)
        expData = [ str(i) for i in expData ]
        dataIn = "".join(expData)
        if (vals[0] in dontCareData):
            expData = [ "-" ] * 8
        expData = "".join(expData)

        # Always let mem be expAddr because if we use mem at all, they are the same
        mem = expAddr
        if (vals[0] in dontCareAddr):
            expAddr = "".join(["-"]*16)

        # Create test vector
        vector = [ opcode, mem, dataIn, expAddr, expData, expRd, expWr, numClks ]
        fOut.write(",".join(vector) + "\n")
        line = fIn.readline()

if __name__ == "__main__":
    fIn = open(sys.argv[1], "r")
    fOut = open(sys.argv[2], "w")

    header = "instruction      memory address   data in  expected address exp data R W clks\n"
    fOut.write(header)
    generate(fIn, fOut)

    fIn.close()
    fOut.close()
