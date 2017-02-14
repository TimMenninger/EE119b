import sys
from generate_values import *

# Instructions where we don't write result back to register.  The exceptions
# are BLD SWAP and MOV, where registers are written to explicitly
dontWriteRegs = [ "BCLR", "BSET", "BST", "CP", "CPC", "CPI", "MUL", "BLD", "SWAP", "MOV" ]

# Two-clock instructions
twoClocks = [ "ADIW", "SBIW", "MUL" ]

# Instructions with no result
noResult = [ "BCLR", "BSET", "BLD", "BST", "CP", "CPC", "CPI", "SWAP", "MOV" ]

def generate(fIn, fOut):
    fIn.readline() # First line is a comment
    line = fIn.readline()
    status = ["-", "-", "-", "-", "-", "-", "-", "-"]
    skip = False
    # Random register values to avoid assuming revious values
    registers = [ -1 ] * 32

    # Go to end of file or till END
    while (line != "" and "END" not in line):
        if ("SKIP" in line):
            skip = not skip

        if (line == "\n" or skip):
            line = fIn.readline()
            continue

        vals = line.split(" ")

        inA = int(vals[1])
        regAIdx = inA
        inB = int(vals[2])
        opcode = compute_instruction(vals[0], inA, inB)

        # Check if opA and opB are constants or register indices
        instruction = instructions[vals[0]][0]
        if ('r' in instruction):
            opB = registers[inB]
        else:
            opB = inB
        if ('d' in instruction):
            opA = registers[inA]
        else:
            opA = inA

        # MOV is special case
        if (vals[0] == "MOV"):
            registers[inA] = opB

        # If it's a word instruction, reflect that in operand
        if (vals[0] in [ "ADIW", "SBIW" ]):
            opA += 256 * registers[inA+1]

        result, status = compute_result(vals[0], opA, opB, list(status))

        # Undo word correction
        if (vals[0] in [ "ADIW", "SBIW" ]):
            opA -= 256 * registers[inA+1]

        # Don't care if there is no result
        if (vals[0] in noResult):
            if (vals[0] in [ "BLD", "SWAP" ]):
                # Use the result to update register
                registers[inA] = binary_str_to_int(result)
            result = "-" * 8

        opA = int_to_binary(opA, 8)
        opA = [ str(i) for i in opA ]
        opA = "".join(opA)
        opB = int_to_binary(opB, 8)
        opB = [ str(i) for i in opB ]
        opB = "".join(opB)

        vector = [ opcode, ",", opA, ",", opB, ",", result, ",", status, "\n" ]
        if (vals[0] == "ADIW" or vals[0] == "SBIW" or vals[0] == "MUL"):
            if (vals[0] == "MUL"):
                registers[0] = binary_str_to_int(result[8:])
                vector[6] = int_to_binary(registers[0], 8)
            else:
                registers[regAIdx] = binary_str_to_int(result[8:])
                vector[6] = int_to_binary(registers[regAIdx], 8)
            vector[6] = [ str(i) for i in vector[6] ]
            vector[6] = "".join(vector[6])
            vector[8] = "-" * 8
            fOut.write("".join(vector))

            # Next clock
            if (vals[0] == "MUL"):
                registers[1] = binary_str_to_int(result[:8])
                vector[6] = int_to_binary(registers[1], 8)
            else:
                vector[2] = int_to_binary(registers[regAIdx+1], 8)
                vector[2] = [ str(i) for i in vector[2] ]
                vector[2] = "".join(vector[2])
                registers[regAIdx+1] = binary_str_to_int(result[:8])
                vector[6] = int_to_binary(registers[regAIdx+1], 8)
                vector[4] = "00000000"
            vector[6] = [ str(i) for i in vector[6] ]
            vector[6] = "".join(vector[6])
            vector[8] = status
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
