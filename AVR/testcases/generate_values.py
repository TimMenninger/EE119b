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
    "SWAP"  :  ("1001010ddddd0010", 1), # Swap nibbles of Rd
    "LDX"   :  ("1001000ddddd1100", 2), # LD Rd, X
    "LDXI"  :  ("1001000ddddd1101", 2), # LD Rd, X+
    "LDXD"  :  ("1001000ddddd1110", 2), # LD Rd, -X
    "LDYI"  :  ("1001000ddddd1001", 2), # LD Rd, Y+
    "LDYD"  :  ("1001000ddddd1010", 2), # LD Rd, -Y
    "LDZI"  :  ("1001000ddddd0001", 2), # LD Rd, Z+
    "LDZD"  :  ("1001000ddddd0010", 2), # LD Rd, -Z
    "LDDY"  :  ("10q0qq0ddddd1qqq", 2), # LDD Rd, Y + q
    "LDDZ"  :  ("10q0qq0ddddd0qqq", 2), # LDD Rd, Z + q
    "LDI"   :  ("1110kkkkddddkkkk", 1), # LDI Rd, k
    "LDS"   :  ("1001000ddddd0000", 3), # LDS Rd, m
    "MOV"   :  ("001011rdddddrrrr", 1), # MOV Rd, Rr
    "STX"   :  ("1001001rrrrr1100", 2), # ST X, Rr
    "STXI"  :  ("1001001rrrrr1101", 2), # ST X+, Rr
    "STXD"  :  ("1001001rrrrr1110", 2), # ST -X, Rr
    "STYI"  :  ("1001001rrrrr1001", 2), # ST Y+, Rr
    "STYD"  :  ("1001001rrrrr1010", 2), # ST -Y, Rr
    "STZI"  :  ("1001001rrrrr0001", 2), # ST Z+, Rr
    "STZD"  :  ("1001001rrrrr0010", 2), # ST -Z, Rr
    "STDY"  :  ("10q0qq1rrrrr1qqq", 2), # STD Y + q, Rr
    "STDZ"  :  ("10q0qq1rrrrr0qqq", 2), # STD Z + q, Rr
    "STS"   :  ("1001001rrrrr0000", 3), # STS m, Rr
    "POP"   :  ("1001000ddddd1111", 2), # POP Rd
    "PUSH"  :  ("1001001rrrrr1111", 2)  # PUSH Rd
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

    # For computing flags
    Rd0 = bool(int_to_binary(opA, 16)[15])
    Rd3 = bool(int_to_binary(opA, 16)[12])
    Rr3 = bool(int_to_binary(opB, 8)[4])
    Rd7 = bool(int_to_binary(opA, 16)[8])
    if (instruction in [ "ADIW", "SBIW" ]):
        Rd7 = bool(int_to_binary(opA, 16)[0])
    Rr7 = bool(int_to_binary(opB, 8)[0])

    # Perform operation
    if (instruction == "ADC"):
        R = int_to_binary(opA + opB + int(status[C]), 8)
        status[H] = (Rd3 and Rr3) or (Rr3 and not R[4]) or (not R[4] and Rd3)
        status[V] = (Rd7 and Rr7 and not R[0]) or (not Rd7 and not Rr7 and R[0])
        status[N] = R[0]
        status[S] = (status[V] and not status[N]) or (not status[V] and status[N])
        status[Z] = (R == [0] * 8)
        status[C] = (Rd7 and Rr7) or (Rr7 and not R[0]) or (not R[0] and Rd7)
    if (instruction == "ADD"):
        R = int_to_binary(opA + opB, 8)
        status[H] = (Rd3 and Rr3) or (Rr3 and not R[4]) or (not R[4] and Rd3)
        status[V] = (Rd7 and Rr7 and not R[0]) or (not Rd7 and not Rr7 and R[0])
        status[N] = R[0]
        status[S] = (status[V] and not status[N]) or (not status[V] and status[N])
        status[Z] = (R == [0] * 8)
        status[C] = (Rd7 and Rr7) or (Rr7 and not R[0]) or (not R[0] and Rd7)
    if (instruction == "ADIW"):
        R = int_to_binary(opA + opB, 16)
        status[V] = not Rd7 and R[0]
        status[N] = R[0]
        status[S] = (status[V] and not status[N]) or (not status[V] and status[N])
        status[Z] = (R == [0] * 16)
        status[C] = not R[0] and Rd7
    if (instruction == "AND"):
        R = int_to_binary(opA & opB, 8)
        status[V] = False
        status[N] = R[0]
        status[S] = (status[V] and not status[N]) or (not status[V] and status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "ANDI"):
        R = int_to_binary(opA & opB, 8)
        status[V] = False
        status[N] = R[0]
        status[S] = (status[V] and not status[N]) or (not status[V] and status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "ASR"):
        R = int_to_binary(opA, 8)
        R[1:] = R[:7]
        R[0] = R[1]
        status[N] = R[0]
        status[Z] = (R == [0] * 8)
        status[C] = Rd0
        status[V] = (not status[N] and status[C]) or (status[N] and not status[C])
        status[S] = (not status[N] and status[V]) or (status[N] and not status[V])
    if (instruction == "BCLR"):
        R = int_to_binary(0, 8)
        status[7 - opA] = 0
    if (instruction == "BLD"):
        R = int_to_binary(opA, 8)
        R[7 - opB] = int(status[T])
    if (instruction == "BSET"):
        R = int_to_binary(0, 8)
        status[7 - opA] = 1
    if (instruction == "BST"):
        R = int_to_binary(0, 8)
        status[T] = int_to_binary(opA, 8)[7 - opB]
    if (instruction == "COM"):
        R = int_to_binary(opA, 8)
        idx = 2
        R = [ (i+1) % 2 for i in R ]
        status[V] = False
        status[N] = R[0]
        status[Z] = (R == [0] * 8)
        status[C] = True
        status[S] = (not status[N] and status[V]) or (status[N] and not status[V])
    if (instruction == "CP"):
        R = int_to_binary(opA - opB, 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[N] and status[V]) or (status[N] and not status[V])
        status[Z] = (R == [0] * 8)
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "CPC"):
        R = int_to_binary(opA - opB - int(status[C]), 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[N] and status[V]) or (status[N] and not status[V])
        status[Z] = (R == [0] * 8) and status[Z]
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "CPI"):
        R = int_to_binary(opA - opB, 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[N] and status[V]) or (status[N] and not status[V])
        status[Z] = (R == [0] * 8)
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "DEC"):
        R = int_to_binary(opA - 1, 8)
        status[V] = (R == [0, 1, 1, 1, 1, 1, 1, 1])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "EOR"):
        R = int_to_binary(opA ^ opB, 8)
        status[V] = False
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "INC"):
        R = int_to_binary(opA + 1, 8)
        status[V] = (R == [1, 0, 0, 0, 0, 0, 0, 0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "LSR"):
        R = int_to_binary(opA, 8)
        R[1:] = R[:7]
        R[0] = 0
        status[N] = 0
        status[Z] = (R == [0] * 8)
        status[C] = Rd0
        status[V] = (not status[C] and status[N]) or (status[C] and not status[N])
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
    if (instruction == "MUL"):
        R = int_to_binary(opA * opB, 16)
        status[C] = R[0]
        status[Z] = (R == [0] * 16)
    if (instruction == "NEG"):
        R = int_to_binary(-1 * opA, 8)
        status[H] = R[4] or Rd3
        status[V] = (R == [1, 0, 0, 0, 0, 0, 0, 0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
        status[C] = (R != [0] * 8)
    if (instruction == "OR"):
        R = int_to_binary(opA | opB, 8)
        status[V] = 0
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "ORI"):
        R = int_to_binary(opA | opB, 8)
        status[V] = 0
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
    if (instruction == "ROR"):
        R = int_to_binary(opA, 8)
        temp = int(status[C])
        status[C] = str(R[0])
        R[1:] = R[:7]
        R[0] = temp
        status[N] = R[0]
        status[Z] = (R == [0] * 8)
        status[C] = Rd0
        status[V] = (not status[C] and status[N]) or (status[C] and not status[N])
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
    if (instruction == "SBC"):
        R = int_to_binary(opA - opB - int(status[C]), 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8) and status[Z]
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "SBCI"):
        R = int_to_binary(opA - opB - int(status[C]), 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8) and status[Z]
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "SBIW"):
        R = int_to_binary(opA - opB, 16)
        status[V] = Rd7 and not R[0]
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 16)
        status[C] = R[0] and not Rd7
    if (instruction == "SUB"):
        R = int_to_binary(opA - opB, 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "SUBI"):
        R = int_to_binary(opA - opB, 8)
        status[H] = (not Rd3 and Rr3) or (Rr3 and R[4]) or (R[4] and not Rd3)
        status[V] = (Rd7 and not Rr7 and not R[0]) or (not Rd7 and Rr7 and R[0])
        status[N] = R[0]
        status[S] = (not status[V] and status[N]) or (status[V] and not status[N])
        status[Z] = (R == [0] * 8)
        status[C] = (not Rd7 and Rr7) or (Rr7 and R[0]) or (R[0] and not Rd7)
    if (instruction == "SWAP"):
        R = int_to_binary(opA, 8)
        temp = R[:4]
        R[:4] = R[4:]
        R[4:] = temp
    if (instruction == "MOV"):
        R = int_to_binary(opB, 8)
    if (instruction == "LDI"):
        R = int_to_binary(opB, 8)

    R = [ str(int(i)) for i in R ]
    for i, elem in enumerate(status):
        try:
            status[i] = str(int(elem))
        except:
            status[i] = str(elem)

    return ''.join(R), ''.join(status)

# Generate opcode
def compute_instruction(instruction, opA, opB):
    # Start with skeleton
    opcode = list(instructions[instruction][0])

    # If the instruction has a source register and no destination, then we should
    # have opB contain the input that is in opA
    if ('r' in opcode and ('d' not in opcode and 'q' not in opcode)):
        opB = opA

    # Get operand A if it exists
    opAList = []
    if (opA != None):
        opAList = int_to_binary(opA, 8)
    idxA = len(opAList) - 1
    if (instruction in [ "ADIW", "SBIW" ]):
        idxA -= 1

    # Get operand B if it exists
    opBList = []
    if (opB != None):
        opBList = int_to_binary(opB, 8)
    idxB = len(opBList) - 1

    # Fill bits in skeleton
    opAChars = ['d', 's']
    opBChars = ['b', 'K', 'k', 'q']
    if ('q' in opcode):
        opAChars.append('r')
    else:
        opBChars.append('r')

    for i in range(len(opcode)-1, -1, -1):
        if (opcode[i] in opAChars):
            opcode[i] = str(opAList[idxA])
            idxA -= 1
        if (opcode[i] in opBChars):
            opcode[i] = str(opBList[idxB])
            idxB -= 1

    return ''.join(opcode)
