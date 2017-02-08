import random
import sys
from generate_values import *

# Instructions where we don't write result back to register
dontWriteRegs = [ "BCLR", "BSET", "BST", "CP", "CPC", "CPI", "MUL" ]

# Two-clock instructions
twoClocks = [ "ADIW", "SBIW", "MUL" ]

def generate(fIn, fOut):
    fIn.readline() # First line is a comment
    line = fIn.readline()
    skip = False
    determined = False # false until we know contents of all regs
    status = ["-"] * 8

    # Random register values to avoid assuming revious values
    registers = [ random.randint(0, 255) for i in range(32) ]

    # Go to end of file or till END (for shortening without deleting)
    while (line != "" and "END" not in line):
        if ("SKIP" in line):
            skip = not skip

        if (line == "\n" or skip):
            line = fIn.readline()
            continue

        vals = line.split(" ")

        if (not determined and vals[0] == "DETERMINED"):
            determined = True
            line = fIn.readline()
            continue

        inA = int(vals[1])
        regAIdx = inA
        inB = int(vals[2])
        opcode = compute_instruction(vals[0], inA, inB)

        # Check if expA and expB are constants or register indices
        instruction = instructions[vals[0]][0]
        if ('r' in instruction):
            expB = registers[inB]
        else:
            expB = inB
        if ('d' in instruction):
            expA = registers[inA]
        else:
            expA = inA

        # If it's a word instruction, reflect that in operand
        if (vals[0] in [ "ADIW", "SBIW" ]):
            expA += 256 * registers[inA+1]

        nextRegIn, status = compute_result(vals[0], expA, expB, list(status))

        # Undo word correction
        if (vals[0] in [ "ADIW", "SBIW" ]):
            expA -= 256 * registers[inA+1]

        # Repeated code but who cares, not the intent rn
        if ("d" in instruction and determined):
            expA = int_to_binary(expA, 8)
            expA = [ str(i) for i in expA ]
        else:
            expA = ["-"] * 8
        if ("r" in instruction and determined):
            expB = int_to_binary(expB, 8)
            expB = [ str(i) for i in expB ]
        else:
            expB = ["-"] * 8

        expA = "".join(expA)
        expB = "".join(expB)

        vector = [ opcode, ",", nextRegIn, ",", expA, ",", expB, "\n" ]
        # Need two lines if two clock instruction
        if (vals[0] in twoClocks):
            if (vals[0] == "MUL"):
                registers[0] = binary_str_to_int(nextRegIn[8:])
                vector[2] = int_to_binary(registers[0], 8)
            else:
                registers[regAIdx] = binary_str_to_int(nextRegIn[8:])
                vector[2] = int_to_binary(registers[regAIdx], 8)
            vector[2] = [ str(i) for i in vector[2] ]
            vector[2] = "".join(vector[2])
            fOut.write("".join(vector))

            # Second clock inputs
            if (vals[0] == "MUL"):
                vector[4] = "--------"
                registers[1] = binary_str_to_int(nextRegIn[:8])
                vector[2] = int_to_binary(registers[1], 8)
            else:
                vector[4] = int_to_binary(registers[regAIdx+1], 8)
                vector[4] = [ str(i) for i in vector[4] ]
                vector[4] = "".join(vector[4])
                registers[regAIdx+1] = binary_str_to_int(nextRegIn[:8])
                vector[2] = int_to_binary(registers[regAIdx+1], 8)
            vector[2] = [ str(i) for i in vector[2] ]
            vector[2] = "".join(vector[2])
        elif (vals[0] not in dontWriteRegs):
            registers[regAIdx] = binary_str_to_int(nextRegIn)

        fOut.write("".join(vector))
        line = fIn.readline()

if __name__ == "__main__":
    fIn = open(sys.argv[1], "r")
    fOut = open(sys.argv[2], "w")

    header = "instruction      nextReg  expA     expB\n"
    fOut.write(header)
    generate(fIn, fOut)

    fIn.close()
    fOut.close()
