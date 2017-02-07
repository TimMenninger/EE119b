import random
import sys
from generate_values import *

# Instructions where we don't write result back to register
dontWriteRegs = [ "BCLR", "BSET", "BST", "CP", "CPC", "CPI", "MUL" ]

def generate(fIn, fOut):
    fIn.readline() # First line is a comment
    line = fIn.readline()
    status = ["-", "-", "-", "-", "-", "-", "-", "-"]
    # Random register values to avoid assuming revious values
    registers = [ random.randint(0, 255) for i in range(32) ]

    while (line != ""):
        if (line == "\n"):
            line = fIn.readline()
            continue

        vals = line.split(" ")

        opA = int(vals[1])
        opB = int(vals[2])
        regAIdx = opA
        opcode = compute_instruction(vals[0], opA, opB)

        # Check if opA and opB are constants or register indices
        instruction = instructions[vals[0]][0]
        if ('r' in instruction):
            opB = registers[opB]
        if ('d' in instruction):
            opA = registers[opA]

        result, status = compute_result(vals[0], opA, opB, list(status))

        opA = int_to_binary(opA, 8)
        opA = [ str(i) for i in opA ]
        opA = "".join(opA)
        opB = int_to_binary(opB, 8)
        opB = [ str(i) for i in opB ]
        opB = "".join(opB)

        vector = [ opcode, ",", opA, ",", opB, ",", result, ",", status, "\n" ]
        if (vals[0] == "ADIW" or vals[0] == "SBIW" or vals[0] == "MUL"):
            if (vals[0] == "ADIW" or vals[0] == "SBIW"):
                registers[regAIdx+1] = binary_str_to_int(result[:8])
                registers[regAIdx] = binary_str_to_int(result[8:])
                vector[4] = "00000000"
            temp = result[:8]
            vector[6] = result[8:]
            fOut.write("".join(vector))
            vector[6] = temp
        elif (vals[0] not in dontWriteRegs):
            registers[regAIdx] = binary_str_to_int(result)
        fOut.write("".join(vector))

        line = fIn.readline()

if __name__ == "__main__":
    fIn = open(sys.argv[1], "r")
    fOut = open(sys.argv[2], "w")

    header = "instruction      opA      opB      result   status\n"
    fOut.write(header)
    generate(fIn, fOut)

    fIn.close()
    fOut.close()
