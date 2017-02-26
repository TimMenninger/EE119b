import copy
import sys
from generate_values import *

dontCare = ["--", "--------", "----------------"]

hex = { "0" :  0, "1" :  1, "2" :  2, "3" :  3, "4" :  4,
        "5" :  5, "6" :  6, "7" :  7, "8" :  8, "9" :  9,
        "A" : 10, "B" : 11, "C" : 12, "D" : 13, "E" : 14,
        "F" : 15, "a" : 10, "b" : 11, "c" : 12, "d" : 13,
        "e" : 14, "f" : 15 }

flags = { "Z" : Z, "I" : I, "C" : C, "N" : N, "H" : H, "S" : S, "V" : V, "T" : T }

# Hard coding when to and not to branch and skip
branches = [1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1]
skips = [0, 2, 1, 0, 2, 1, 0, 2, 1]

def parse_instruction(line):
    vals = line.split()
    if (len(vals) == 0):
        return False, vals, ""

    orig = vals[0]

    # Load instructions
    if (vals[0] == "LDD"):
        vals[0] += vals[2]
        vals[2] = vals[4]
    elif (vals[0] == "STD"):
        vals[0] += vals[1]
        vals[1] = vals[4]
        vals[2] = vals[3]
    elif (vals[0] == "LD"):
        # Add the register
        if ("X" in vals[2]):
            vals[0] += "X"
        elif ("Y" in vals[2]):
            vals[0] += "Y"
        else:
            vals[0] += "Z"
        # Add increment/decrement
        if ("-" in vals[2]):
            vals[0] += "D"
        elif ("+" in vals[2]):
            vals[0] += "I"
        # We no longer need second argument
        vals[2] = "0"
    elif (vals[0] == "ST"):
        # Add the register
        if ("X" in vals[1]):
            vals[0] += "X"
        elif ("Y" in vals[1]):
            vals[0] += "Y"
        else:
            vals[0] += "Z"
        # Add increment/decrement
        if ("-" in vals[1]):
            vals[0] += "D"
        elif ("+" in vals[1]):
            vals[0] += "I"
        # We no longer need second argument
        vals[1] = vals[2]
        vals[2] = "0"

    # clear and set flag instructions
    if (vals[0][:2] in [ "CL", "SE" ]):
        # Set the first argument before changing the instruction name
        if (vals[0][2] == "I"):
            vals[1] = I
        elif (vals[0][2] == "T"):
            vals[1] = T
        elif (vals[0][2] == "H"):
            vals[1] = H
        elif (vals[0][2] == "S"):
            vals[1] = S
        elif (vals[0][2] == "V"):
            vals[1] = V
        elif (vals[0][2] == "N"):
            vals[1] = N
        elif (vals[0][2] == "Z"):
            vals[1] = Z
        elif (vals[0][2] == "C"):
            vals[1] = C
        vals[1] = str(7 - vals[1])

        # Now we can set the instruction name
        if (vals[0][:2] == "CL"):
            vals[0] = "BCLR"
        else:
            vals[0] = "BSET"
        vals[2] = "0"

    # Do the branching instructions
    if (vals[0] == "BREQ"): # Z flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-Z)
    elif (vals[0] == "BRNE"): # Z flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-Z)
    elif (vals[0] == "BRCS" or vals[0] == "BRLO"): # C flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-C)
    elif (vals[0] == "BRCC" or vals[0] == "BRSH"): # C flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-C)
    elif (vals[0] == "BRMI"): # N flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-N)
    elif (vals[0] == "BRPL"): # N flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-N)
    elif (vals[0] == "BRGE"): # S flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-S)
    elif (vals[0] == "BRLT"): # S flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-S)
    elif (vals[0] == "BRHS"): # H flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-H)
    elif (vals[0] == "BRHC"): # H flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-H)
    elif (vals[0] == "BRTS"): # T flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-T)
    elif (vals[0] == "BRTC"): # T flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-T)
    elif (vals[0] == "BRVS"): # V flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-V)
    elif (vals[0] == "BRVC"): # V flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-V)
    elif (vals[0] == "BRIE"): # I flag 1
        vals[0] = "BRBS"
        vals[2] = vals[1]
        vals[1] = str(7-I)
    elif (vals[0] == "BRID"): # I flag 0
        vals[0] = "BRBC"
        vals[2] = vals[1]
        vals[1] = str(7-I)

    if (vals[0] in instructions):
        return True, vals, orig

    return False, vals, orig


def parse_arg(arg, labels):
    # If there's a comma, remove it
    arg = arg.split(",")[0]

    # If this is a register, remove the R
    if (arg[0] == "R"):
        arg = int(arg[1:])
    # If this is an immediate, turn it into an int
    elif (arg[0] == "$"):
        argHex = arg[1:]
        arg = 0
        for i in range(len(argHex)-1, -1, -1):
            arg += 16**(len(argHex) - i - 1) * hex[argHex[i]]
    elif ("LOW" in arg):
        arg = arg.split("(")[-1].split(")")[0]
        addr = int_to_binary(labels[arg], 16)
        addr = [ str(i) for i in addr ]
        arg = binary_str_to_int(addr[8:])
    elif ("HIGH" in arg):
        arg = arg.split("(")[-1].split(")")[0]
        addr = int_to_binary(labels[arg], 16)
        addr = [ str(i) for i in addr ]
        arg = binary_str_to_int(addr[:8])
    else:
        try:
            arg = int(arg)
        except:
            pass

    return arg

def parse_comment(comment):
    comment = comment.split(";")
    if (len(comment) > 1):
        comment = comment[1]
    if (len(comment) == 0 or (comment[0] != "R" and comment[0] != "W")):
        return False, "-1", "--------", "----------------"

    # Figure out if it is a read or write
    RW = "01"
    if (comment[0] == "W"):
        RW = "10"

    # Get the value we expect
    value = int_to_binary(16 * hex[comment[1]] + hex[comment[2]], 8)
    value = [ str(i) for i in value ]

    # Get the address we are accessing
    addr = int_to_binary(16*16*16*hex[comment[3]] + 16*16*hex[comment[4]] + \
            16*hex[comment[5]] + hex[comment[6]], 16)
    addr = [ str(i) for i in addr ]

    # Return the parsed values
    return True, RW, "".join(value), "".join(addr)

def generate(fIn, fOut, fVecs, template):
    #####################################################################################
    # FIRST PASS TO GET NUMBER OF INSTRUCTIONS
    #####################################################################################

    # Count number of instructions
    numInstructions = 0

    # Label addresses
    labels = {}

    # The instructions
    asm_lines = []

    # Parse for key addresses
    asm_line = fIn.readline()
    while (asm_line != ""):
        valid, vals, origInst = parse_instruction(asm_line)
        if (len(vals) == 0):
            asm_line = fIn.readline()
            continue

        # Add the number of instruction words
        if (vals[0] in instructions):
            asm_lines.append(asm_line)
            numInstructions += 1
            if (instructions[vals[0]][2] == 2):
                asm_lines.append("")
                numInstructions += 1

        # Record key addresses by labels
        if (len(vals) > 0 and vals[0][-1] == ":"):
            labels[vals[0][:-1]] = numInstructions

        asm_line = fIn.readline()

    #####################################################################################
    # COPY FIRST PART OF CODE
    #####################################################################################

    # Copy the template file into the output vhd file until the start token
    ROM = ["        X\"DEAD\",\n"] * numInstructions
    template_line = template.readline()
    while ("ROM_CODE_START" not in template_line):
        fOut.write(template_line)
        template_line = template.readline()

    # First line declares the constant
    fOut.write("    type ROMtype is array(0 to %d) of address_t;\n" %(numInstructions))
    fOut.write("    signal ROMbits : ROMtype :=  (\n")

    #####################################################################################
    # TEST CODE START
    #####################################################################################

    # Reset the file pointer for copying
    fIn.seek(0)

    # Keep status to know about conditional jumping and all
    status = "--------"
    registers = [0] * 32

    IP = 0

    # Get each instruction opcode
    for i, elem in enumerate(asm_lines):
        if (elem == ""):
            continue
        valid, vals, origInst = parse_instruction(elem)

        # Make sure we don't index out of range
        if (len(vals) < 2 or vals[1][0] == ";"):
            vals.insert(1, "0")
        if (len(vals) < 3 or vals[2][0] == ";"):
            vals.insert(2, "0")

        # Get the arguments
        vals[1] = parse_arg(vals[1], labels)
        vals[2] = parse_arg(vals[2], labels)
        if (type(vals[1]) is str):
            vals[1] = labels[vals[1]]
        if (type(vals[2]) is str):
            vals[2] = labels[vals[2]]

        asm_lines[i] = [ copy.deepcopy(vals), asm_lines[i] ]

        # For RJMP and RCALL, we use the label to get an offset and embed teh offset
        # in the opcode
        if (vals[0] == "RJMP" or vals[0] == "RCALL"):
            vals[1] = vals[1] - i - 1

        # For branches, we also need offset
        if (vals[0][:2] == "BR"):
            vals[2] = vals[2] - i - 1

        # Store the opcode
        ROM[i] = "        \"" + compute_instruction(vals[0], vals[1], vals[2]) + "\",\n"

        # If it is two-word, get the second word, too
        if (vals[0] in [ "JMP", "CALL", "LDS", "STS" ]):
            if (vals[0] == "LDS"):
                ROM[i+1] = int_to_binary(vals[2], 16)
            else:
                ROM[i+1] = int_to_binary(vals[1], 16)
            ROM[i+1] = [ str(j) for j in ROM[i+1] ]
            ROM[i+1] = "        \"" + "".join(ROM[i+1]) + "\",\n"

    # Put the opcodes in the VHD file
    for elem in ROM:
        fOut.write(elem)
    fOut.write("        X\"DEAD\" -- End of code in ROM\n")

    #####################################################################################
    # FINISH COPYING TEMPLATE
    #####################################################################################

    # Close the signal declaration
    fOut.write("    );\n")

    # Finish copying template
    template_line = template.readline() # Skip token line
    while (template_line != ""):
        fOut.write(template_line)
        template_line = template.readline()

    #####################################################################################
    # CREATE VECTORS
    #####################################################################################
    callStack = []

    IP = 0
    numWrites = 0
    done = False
    while (not done):
        numWrites += 1
        vals = asm_lines[IP][0]
        instruction = vals[0]

        # This is how many clocks we skip.  Usually zero unless skip instruction
        skip = 0

        # Figure out the data and address, if applicable
        valid, RW, DataDB, DataAB = parse_comment(asm_lines[IP][1])

        # Program address for the instruction
        ProgAB = int_to_binary(IP, 16)
        ProgAB = [ str(i) for i in ProgAB ]
        ProgAB = "".join(ProgAB)

        # Update the instruction pointer
        if (instruction == "CALL"):
            callStack.append(IP + 2)
            IP = vals[1]
        elif (instruction == "RCALL"):
            callStack.append(IP + 1)
            IP = vals[1]
        elif (instruction == "ICALL"):
            callStack.append(IP + 1)
            IP = labels["Subr1"]
        elif (instruction == "JMP"):
            IP = vals[1]
        elif (instruction == "RJMP"):
            IP = vals[1]
        elif (instruction == "IJMP"):
            IP = labels["TestIJump"]
        elif (instruction == "RET"):
            IP = callStack.pop()
        elif (instruction == "RETI"):
            IP = callStack.pop()
            status = "1" + status[1:]
        elif (instruction[:2] == "BR" and branches[-1] == 1): # I flag 0
            IP = vals[2]
            skip = 1
            branches.pop()
        elif (instruction[:2] == "BR"):
            IP += instructions[instruction][2]
            branches.pop()
        elif (instruction in ["CPSE", "SBRS", "SBRC"]):
            skip = skips[-1]
            IP += skips.pop() + 1
        else:
            IP += instructions[instruction][2]

        # Construct the vectors
        # instruction, RW, data, addr, progaddr
        vector = [ instruction + (" " * (5 - len(instruction))) ] + [ " " ] * 4
        for i in range(instructions[instruction][1]):
            # Only guaranteed to have program address correct on first clock while
            # we latch the instruction
            if (i == 0):
                vector[4] = ProgAB
            else:
                vector[4] = "----------------"

            # Don't care about anything else until last clock
            if (i == instructions[instruction][1] - 1):
                vector[1] = RW
                vector[2] = DataDB
                vector[3] = DataAB
            else:
                vector[1] = "-1"
                vector[2] = "--------"
                vector[3] = "----------------"

            # Special case for RW when push/pop twice
            if (i == 0 and vals[0] in [ "RCALL", "ICALL" ]):
                vector[1] = "10"
            elif (i == 1 and vals[0] in [ "RCALL", "ICALL", "CALL" ]):
                vector[1] = "10"
            elif (i == 1 and vals[0] in [ "RET", "RETI" ]):
                vector[1] = "01"
            elif (i == 2 and vals[0] in [ "RET", "RETI" ]):
                vector[1] = "01"
            elif (i == 2 and vals[0] == "CALL"):
                vector[1] = "10"
            elif (vals[0] in [ "RET", "RETI", "CALL", "RCALL", "ICALL" ]):
                vector[1] = "11"
            fVecs.write(",".join(vector) + "\n")

        # If we are skipping anything, throw in dummy vectors ensuring we aren't
        # writing to memory
        for i in range(skip):
            vector = ["XXXXX", "-1", "--------", "----------------", "----------------"]
            fVecs.write(",".join(vector) + "\n")

        if (IP == 1 and numWrites > 2):
            done = True

    return

###################

useRd = [ "ADC", "ADD", "ADIW", "AND", "ANDI", "ASR", "BLD", "BST", "COM", "CP",
          "CPC", "CPI", "DEC", "EOR", "INC", "LSR", "MUL", "NEG", "OR", "ORI",
          "ROR", "SBC", "SBCI", "SBIW", "SUB", "SUBI", "SWAP" ]
useRr = [ "ADC", "ADD", "AND", "CP", "CPC", "EOR", "MUL", "OR", "SBC", "SUB" ]
writeRegs = [ "ADC", "ADD", "ADIW", "AND", "ANDI", "ASR", "BLD", "COM", "DEC",
              "EOR", "INC", "LSR", "MUL", "NEG", "OR", "ORI", "ROR", "SBC", "SBCI",
              "SBIW", "SUB", "SUBI", "SWAP" ]
noResult = [ "BCLR", "BSET", "BLD", "BST", "CP", "CPC", "CPI", "SWAP", "MOV" ]

# Compute the new status
def compute_status(instruction, opA, opB, status, registers):
    inA = int(opA)
    opA = inA
    inB = int(opB)
    opB = inB
    if (instruction in useRd):
        opA = registers[inA]
    if (instruction in useRr):
        opB = registers[inB]

    status = list(status)

    # MOV is special case
    if (instruction == "MOV"):
        registers[inA] = opB

    # If it's a word instruction, reflect that in operand
    if (instruction in [ "ADIW", "SBIW" ]):
        opA += 256 * registers[inA+1]

    result, status = compute_result(instruction, opA, opB, status)

    # Undo word correction
    if (instruction in [ "ADIW", "SBIW" ]):
        opA -= 256 * registers[inA+1]

    # Don't care if there is no result
    if (instruction in noResult):
        if (instruction in [ "BLD", "SWAP" ]):
            # Use the result to update register
            registers[inA] = binary_str_to_int(result)

    # Two-word instructions
    if (instruction == "ADIW" or instruction == "SBIW" or instruction == "MUL"):
        if (instruction == "MUL"):
            registers[0] = binary_str_to_int(result[8:])
        else:
            registers[inA] = binary_str_to_int(result[8:])

        # Next clock
        if (instruction == "MUL"):
            registers[1] = binary_str_to_int(result[:8])
        else:
            registers[inA+1] = binary_str_to_int(result[:8])
    elif (instruction in writeRegs):
        registers[inA] = binary_str_to_int(result)

    return status, registers, 0

if __name__ == '__main__':
    fIn = open(sys.argv[1], "r")
    fOut = open(sys.argv[2], "w")
    fVecs = open(sys.argv[3], "w")
    template = open(sys.argv[4], "r")

    generate(fIn, fOut, fVecs, template)

    fIn.close()
    fOut.close()
    fVecs.close()
    template.close()
