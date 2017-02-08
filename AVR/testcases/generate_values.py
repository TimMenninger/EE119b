import sys

T = 1
H = 2
S = 3
V = 4
N = 5
Z = 6
C = 7

instructions = {
    "ADC"   :  ("000111rdddddrrrr", 1), # Rd = Rd + Rr + C
    "ADD"   :  ("000011rdddddrrrr", 1), # Rd = Rd + Rr
    "ADIW"  :  ("10010110KKddKKKK", 2), # Rd+1|Rd = Rd+1|Rd + K
    "AND"   :  ("001000rdddddrrrr", 1), # Rd = Rd AND Rr
    "ANDI"  :  ("0111KKKKddddKKKK", 1), # Rd = Rd AND K
    "ASR"   :  ("1001010ddddd0101", 1), # Rd arithmetic shift right
    "BCLR"  :  ("100101001sss1000", 1), # SREG(s) = 0
    "BLD"   :  ("1111100ddddd0bbb", 1), # Rd(b) = T
    "BSET"  :  ("100101000sss1000", 1), # SREG(s) = 1
    "BST"   :  ("1111101ddddd0bbb", 1), # T = Rd(b)
    "COM"   :  ("1001010ddddd0000", 1), # Rd = NOT Rd
    "CP"    :  ("000101rdddddrrrr", 1), # Rd - Rr
    "CPC"   :  ("000001rdddddrrrr", 1), # Rd - Rr - C
    "CPI"   :  ("0011KKKKddddKKKK", 1), # Rd - K
    "DEC"   :  ("1001010ddddd1010", 1), # Rd = Rd - 1
    "EOR"   :  ("001001rdddddrrrr", 1), # Rd = Rd XOR Rr
    "INC"   :  ("1001010ddddd0011", 1), # Rd = Rd + 1
    "LSR"   :  ("1001010ddddd0110", 1), # Rd = logical shift right
    "MUL"   :  ("100111rdddddrrrr", 2), # R1|R0 = Rd * Rr
    "NEG"   :  ("1001010ddddd0001", 1), # Rd = -1 * Rd
    "OR"    :  ("001010rdddddrrrr", 1), # Rd = Rd OR Rr
    "ORI"   :  ("0110KKKKddddKKKK", 1), # Rd = Rd OR K
    "ROR"   :  ("1001010ddddd0111", 1), # Rd = rotate right
    "SBC"   :  ("000010rdddddrrrr", 1), # Rd = Rd - Rr - C
    "SBCI"  :  ("0100KKKKddddKKKK", 1), # Rd = Rd - K - C
    "SBIW"  :  ("10010111KKddKKKK", 2), # Rd+1|Rd = Rd+1|Rd - K
    "SUB"   :  ("000110rdddddrrrr", 1), # Rd = Rd - Rr
    "SUBI"  :  ("0101KKKKddddKKKK", 1), # Rd = Rd - K
    "SWAP"  :  ("1001010ddddd0010", 1)  # Swap nibbles of Rd
}

# Binary string to integer
def binary_str_to_int(n):
    out = 0
    for i in range(len(n)-1, -1, -1):
        if (n[i] == "1"):
            out += 2**(len(n)-i-1)
    return out

# Integer to binary list of 1's and 0's
def int_to_binary(n, bits):
    out = [0] * bits
    if (n == None):
        return out

    power = bits
    while (n > 2**bits - 1):
        n -= 2**power
        power += 1

    while (n < -1 * 2**(bits-1)):
        n += 2**bits

    if (n < 0):
        out[0] = 1
        for i in range(bits-2, -1, -1):
            if (-1 * 2**(bits-1) + 2**i <= n):
                out[bits-i-1] = 1
                n -= 2**i

    else:
        for i in range(bits-1, -1, -1):
            if (2**i <= n):
                out[bits-i-1] = 1
                n -= 2**i

    return out

# Compute the flags
def compute_result(instruction, opA, opB, status):
    # Compute flags and result for each individual instruction

    flagMask = [1,1,1,1,1,1,1,1]
    TF = 0
    idx = 0

    if (instruction == "ADC"):
        res = int_to_binary(opA + opB + int(status[C]), 8)
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "ADD"):
        res = int_to_binary(opA + opB, 8)
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "ADIW"):
        res = int_to_binary(opA + opB, 16)
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "AND"):
        res = int_to_binary(opA & opB, 8)
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "ANDI"):
        res = int_to_binary(opA & opB, 8)
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "ASR"):
        res = int_to_binary(opA, 8)
        res[1:] = res[:7]
        res[0] = res[1]
        idx = 1
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "BCLR"):
        res = int_to_binary(0, 8)
        status[7 - opA] = 0
    if (instruction == "BLD"):
        res = int_to_binary(opA, 8)
        res[7 - opB] = int(status[T])
    if (instruction == "BSET"):
        res = int_to_binary(0, 8)
        status[7 - opA] = 1
    if (instruction == "BST"):
        res = int_to_binary(0, 8)
        TF = int_to_binary(opA, 8)[7 - opB]
        flagMask = [1,0,1,1,1,1,1,1];
    if (instruction == "COM"):
        res = int_to_binary(opA, 8)
        idx = 2
        res = [ (i+1) % 2 for i in res ]
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "CP"):
        res = int_to_binary(opA - opB, 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "CPC"):
        res = int_to_binary(opA - opB - int(status[C]), 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "CPI"):
        res = int_to_binary(opA - opB, 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "DEC"):
        idx = 5
        res = int_to_binary(opA - 1, 8)
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "EOR"):
        res = int_to_binary(opA ^ opB, 8)
        idx = 2
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "INC"):
        res = int_to_binary(opA + 1, 8)
        idx = 5
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "LSR"):
        res = int_to_binary(opA, 8)
        res[1:] = res[:7]
        res[0] = 0
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "MUL"):
        idx = 5
        res = int_to_binary(opA * opB, 16)
        flagMask = [1,1,1,1,1,1,0,0];
    if (instruction == "NEG"):
        res = int_to_binary(-1 * opA, 8)
        idx = 6
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "OR"):
        res = int_to_binary(opA | opB, 8)
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "ORI"):
        res = int_to_binary(opA | opB, 8)
        flagMask = [1,1,1,0,0,0,0,1];
    if (instruction == "ROR"):
        res = int_to_binary(opA, 8)
        temp = int(status[C])
        status[C] = str(res[7])
        res[1:] = res[:7]
        res[0] = temp
        idx = 1
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "SBC"):
        res = int_to_binary(opA - opB - int(status[C]), 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "SBCI"):
        res = int_to_binary(opA - opB - int(status[C]), 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "SBIW"):
        res = int_to_binary(opA - opB, 16)
        idx = 3
        flagMask = [1,1,1,0,0,0,0,0];
    if (instruction == "SUB"):
        res = int_to_binary(opA - opB, 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "SUBI"):
        res = int_to_binary(opA - opB, 8)
        idx = 3
        flagMask = [1,1,0,0,0,0,0,0];
    if (instruction == "SWAP"):
        res = int_to_binary(opA, 8)
        temp = res[:4]
        res[:4] = res[4:]
        res[4:] = temp

    HF = [0] * 7
    VF = [0] * 7
    CF = [0] * 7

    R = res
    Rd0 = int_to_binary(opA, 16)[15]
    Rd3 = int_to_binary(opA, 16)[12]
    Rr3 = int_to_binary(opB, 8)[4]
    Rd7 = int_to_binary(opA, 16)[8]
    if (instruction in [ "ADIW", "SBIW" ]):
        Rd7 = int_to_binary(opA, 16)[0]
    Rr7 = int_to_binary(opB, 8)[0]

    MSB = 0

    # Half-carry flag
    HF[0] = (Rd3 == 1 and Rr3 == 1) or \
            (Rr3 == 1 and R[4]  == 0) or \
            (R[4] == 0 and Rd3 == 1)
    HF[1] = 0
    HF[2] = 0
    HF[3] = (Rd3 == 0 and Rr3 == 1) or \
            (Rr3 == 1 and R[4] == 1) or \
            (R[4] == 1 and Rd3 == 0)
    HF[4] = 0
    HF[5] = 0
    HF[6] = R[4] or Rd3

    # Overflow flag
    VF[0] = (Rd7 == 1 and Rr7 == 1 and R[MSB] == 0) or \
            (Rd7 == 0 and Rr7 == 0 and R[MSB] == 1)
    VF[1] = (Rd0 == 1 and R[MSB] == 0) or \
            (Rd0 == 0 and R[MSB] == 1)
    VF[2] = 0
    VF[3] = (Rd7 == 1 and Rr7 == 0 and R[MSB] == 0) or \
            (Rd7 == 0 and Rr7 == 1 and R[MSB] == 1)
    VF[4] = R == [0, 1, 1, 1, 1, 1, 1, 1]
    VF[5] = R == [1, 0, 0, 0, 0, 0, 0, 0]
    VF[6] = R == [1, 0, 0, 0, 0, 0, 0, 0]

    # Sign flag
    NF = R[MSB]

    # Zero flag
    ZF = (R == [ 0 ] * len(R))

    # Carry flag
    CF[0] = (Rd7 == 1 and Rr7 == 1) or \
            (Rr7 == 1 and R[MSB] == 0) or \
            (R[MSB] == 0 and Rd7 == 1)
    CF[1] = Rd0
    CF[2] = 1
    CF[3] = (Rd7 == 0 and Rr7 == 1) or \
            (Rr7 == 1 and R[MSB] == 1) or \
            (R[MSB] == 1 and Rd7 == 0)
    CF[4] = 0
    CF[5] = R[MSB]
    CF[6] = R != [0, 0, 0, 0, 0, 0, 0, 0]

    newFlags = [0, TF, int(HF[idx]), int(NF ^ VF[idx]), \
        int(VF[idx]), int(NF), int(ZF), int(CF[idx])]

    for i in range(8):
        if (flagMask[i] == 0):
            status[i] = newFlags[i]

    res = [ str(i) for i in res ]
    status = [ str(i) for i in status ]

    return ''.join(res), ''.join(status)

# Generate opcode
def compute_instruction(instruction, opA, opB):
    # Start with skeleton
    opcode = list(instructions[instruction][0])

    # Get operand A if it exists
    opAList = []
    if (opA != None):
        opAList = int_to_binary(opA, 8)
    idxA = len(opAList) - 1

    # Get operand B if it exists
    opBList = []
    if (opB != None):
        opBList = int_to_binary(opB, 8)
    idxB = len(opBList) - 1

    # Fill bits in skeleton
    opAChars = ['d', 's']
    opBChars = ['r', 'b', 'K']
    for i in range(len(opcode)-1, -1, -1):
        if (opcode[i] in opAChars):
            opcode[i] = str(opAList[idxA])
            idxA -= 1
        if (opcode[i] in opBChars):
            opcode[i] = str(opBList[idxB])
            idxB -= 1

    return ''.join(opcode)
