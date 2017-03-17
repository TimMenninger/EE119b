#
# This tests all O(2^n) configurations of nodes stuck at 0 or 1, or not stuck, for
# n = 24.
#

S3 = None
S2 = None
S1 = None
S0 = None
P3 = 0
P2 = 0
P1 = 0
P0 = 0
N01 = None
N02 = None
N03 = None
N04 = None
N05 = None
N06 = None
N07 = None
N08 = None
N09 = None
N10 = None
N11 = None
N12 = None
N13 = None
N14 = None
C0 = None
C1 = None

def int_to_binary24(v):
    out = [0] * 24
    count = 0
    for i in range(23, -1, -1):
        if (2**i <= v):
            v -= 2**i
            out[i] = 1
            count += 1
    return out, count

def generate_stuck(lst):
    out = [None] * 24
    count = 0
    for i in range(len(lst)):
        if (lst[i] == 1):
            out[i] = 0
            count += 1
    return out

def enumerate_stuck(lst, count):
    out = []
    for i in range(2**count):
        i_bin, c = int_to_binary24(i)
        idx = 0
        out.append([])
        for j in range(len(lst)):
            out[i].append(None)
            if (lst[j] != None):
                out[i][j] = i_bin[idx]
                idx += 1
    return out

def AND(*args):
    # AND arbitrary number of values
    out = 1
    for arg in args:
        out = int(out and arg)
    return out

def XOR(*args):
    # XOR arbitrary number of values
    out = 0
    for arg in args:
        out += arg
    return (out % 2)

def OR(*args):
    # OR arbitrary number of values
    out = 0
    for arg in args:
        out = int(out or arg)
    return out

def NOT(arg):
    # NOT value
    return (arg + 1) % 2

def reset(stuck):
    global S3, S2, S1, S0, P3, P2, P1, P0, N01, N02, N03, N04, N05, N06, N07, N08
    global N09, N10, N11, N12, N13, N14, C0, C1

    S3  = stuck[0]
    S2  = stuck[1]
    S1  = stuck[2]
    S0  = stuck[3]
    if (stuck[4] == None):
        P3 = 0
    else:
        P3  = stuck[4]
    if (stuck[5] == None):
        P2 = 0
    else:
        P2  = stuck[5]
    if (stuck[6] == None):
        P1 = 0
    else:
        P1  = stuck[6]
    if (stuck[7] == None):
        P0 = 0
    else:
        P0  = stuck[7]
    N01 = stuck[8]
    N02 = stuck[9]
    N03 = stuck[10]
    N04 = stuck[11]
    N05 = stuck[12]
    N06 = stuck[13]
    N07 = stuck[14]
    N08 = stuck[15]
    N09 = stuck[16]
    N10 = stuck[17]
    N11 = stuck[18]
    N12 = stuck[19]
    N13 = stuck[20]
    N14 = stuck[21]
    C0  = stuck[22]
    C1  = stuck[23]

def printNodes():
    print(S3, S2, S1, S0, P3, P2, P1, P0, N01, N02, N03, N04, N05, N06, N07,\
          N08, N09, N10, N11, N12, N13, N14, C0, C1)

def update(Die22, Die21, Die20, Die12, Die11, Die10, StorePoint, stuck):
    # StorePoint 1 when rising edge
    global S3, S2, S1, S0, P3, P2, P1, P0, N01, N02, N03, N04, N05, N06, N07, N08
    global N09, N10, N11, N12, N13, N14, C0, C1

    # Update
    C0  = AND(Die10, Die20)
    if (stuck[22] != None):
        C0 = stuck[22]
    N01 = XOR(Die11, Die21)
    if (stuck[8] != None):
        N01 = stuck[8]
    N02 = NOT(AND(Die11, Die21))
    if (stuck[9] != None):
        N02 = stuck[9]
    N03 = NOT(AND(C0, N01))
    if (stuck[10] != None):
        N03 = stuck[10]

    C1  = NOT(AND(N02, N03))
    if (stuck[23] != None):
        C1 = stuck[23]
    N04 = XOR(Die12, Die22)
    if (stuck[11] != None):
        N04 = stuck[11]
    N05 = NOT(AND(Die12, Die22))
    if (stuck[12] != None):
        N05 = stuck[12]
    N06 = NOT(AND(C1, N04))
    if (stuck[13] != None):
        N06 = stuck[13]

    S0  = XOR(Die10, Die20)
    if (stuck[3] != None):
        S0 = stuck[3]
    S1  = XOR(C0, N01)
    if (stuck[2] != None):
        S1 = stuck[2]
    S2  = XOR(C1, N04)
    if (stuck[1] != None):
        S2 = stuck[1]
    S3  = NOT(AND(N05, N06))
    if (stuck[0] != None):
        S3 = stuck[0]

    if (StorePoint == 1):
        P0 = S0
        if (stuck[7] != None):
            P0 = stuck[7]
        P1 = S1
        if (stuck[6] != None):
            P1 = stuck[6]
        P2 = S2
        if (stuck[5] != None):
            P2 = stuck[5]
        P3 = S3
        if (stuck[4] != None):
            P3 = stuck[4]

    N07 = XOR(P0, S0)
    if (stuck[14] != None):
        N07 = stuck[14]
    N08 = XOR(P1, S1)
    if (stuck[15] != None):
        N08 = stuck[15]
    N09 = XOR(P2, S2)
    if (stuck[16] != None):
        N09 = stuck[16]
    N10 = XOR(P3, S3)
    if (stuck[17] != None):
        N10 = stuck[17]

    N11 = NOT(S1)
    if (stuck[18] != None):
        N11 = stuck[18]
    N12 = NOT(S2)
    if (stuck[19] != None):
        N12 = stuck[19]
    N13 = NOT(AND(S1, S3))
    if (stuck[20] != None):
        N13 = stuck[20]
    N14 = OR(S1, S2, S3)
    if (stuck[21] != None):
        N14 = stuck[21]

    DiceEqPoint = NOT(OR(N07, N08, N09, N10))
    DiceEq7     = AND(N11, S0, S2)
    DiceEq11    = AND(N12, S0, S3)
    DiceEq2312  = NOT(AND(N13, N14))

    return DiceEqPoint, DiceEq7, DiceEq11, DiceEq2312

def run():
    needCoverage = []
    count = 0
    for i in range(2**24):
        bits, count = int_to_binary24(i)
        stuckBits = generate_stuck(bits)
        stuckLists = enumerate_stuck(stuckBits, count)
        for stuck in stuckLists:
            reset(stuck)
            error  = NOT(update(0,0,0,0,0,0,0, stuck) == (1, 0, 0, 1))
            error += NOT(update(0,0,0,0,0,0,1, stuck) == (1, 0, 0, 1))
            error += NOT(update(0,1,0,0,0,0,0, stuck) == (0, 0, 0, 0))
            error += NOT(update(0,0,0,1,0,0,0, stuck) == (0, 0, 0, 0))
            error += NOT(update(1,1,0,1,1,1,0, stuck) == (0, 1, 0, 0))
            error += NOT(update(0,1,1,1,1,1,0, stuck) == (0, 0, 1, 0))
            error += NOT(update(1,1,1,1,1,0,0, stuck) == (0, 1, 0, 0))
            error += NOT(update(0,0,1,1,1,1,0, stuck) == (0, 0, 0, 0))
            error += NOT(update(0,1,1,0,0,1,0, stuck) == (0, 0, 0, 0))
            error += NOT(update(1,1,1,1,1,1,0, stuck) == (0, 0, 0, 1))
            error += NOT(update(1,1,1,1,1,1,1, stuck) == (1, 0, 0, 1))
            error += NOT(update(0,0,0,0,0,1,0, stuck) == (0, 0, 0, 1))
            error += NOT(update(0,0,0,0,0,1,1, stuck) == (1, 0, 0, 1))
            error += NOT(update(0,0,0,0,0,0,0, stuck) == (0, 0, 0, 1))
            error += NOT(update(0,0,0,0,0,0,1, stuck) == (1, 0, 0, 1))

            if (error == 0):
                print(stuck)
                needCoverage.append(stuck)
                count += 1
        if (i % 10000 == 0):
            print(i)
    print(needCoverage)
    print(count)

if __name__ == "__main__":
    run()
