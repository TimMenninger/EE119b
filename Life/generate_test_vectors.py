import random, sys

# We use two cell arrays because we need to update every cell based on the old
# values of the cells.  We keep a variable that tells us which to use.
cellArrayIdx = 0

# The two cell arrays, initialized to everything dead.  To avoid working with
# boundary conditions, we add an extra column and row on the left, right, top
# and bottom, all cells of which are always dead.
cellArrays = []

# Initializes an empty array
def initializeArray(size):
    global n, cellArrays, cellArrayIdx
    n = size
    cellArrayIdx = 0

    for i in range(2):
        cellArrays.append([])
        for row in range(n+2):
            cellArrays[i].append([])
            for col in range(n+2):
                cellArrays[i][row].append(0)

# Counts how many cells around this one are alive.  This ignores boundary
# conditions and assumes the row/col are not 0 or n+1
def surroundingAlive(row, col):
    """
    Returns the number of cells around the argued coordinate that are alive.
    This assumes argued indices that are not on the edge of the array.
    """
    return sum(cellArrays[cellArrayIdx][row-1][col-1:col+2]) + \
           sum(cellArrays[cellArrayIdx][row][col-1:col+2]) +   \
           sum(cellArrays[cellArrayIdx][row+1][col-1:col+2]) - \
           cellArrays[cellArrayIdx][row][col]

# Update the cell array and array index
def update(steps):
    """
    Updates cell array based on rules of life.  The cellArrayIdx at the time
    of function call is the current, and this fills the next and updates
    the index accordingly.
    """
    global cellArrayIdx, cellArrays

    for i in range(steps):
        newArrayIdx = (cellArrayIdx + 1) % 2

        # Set cell if exactly 2 or 3 surrounding cells are alive
        for row in range(1, n+1):
            for col in range(1, n+1):
                # Assume dying, change to alive if necessary
                cellArrays[newArrayIdx][row][col] = 0
                if (surroundingAlive(row, col) == 3):
                    cellArrays[newArrayIdx][row][col] = 1
                elif (surroundingAlive(row, col) == 2):
                    cellArrays[newArrayIdx][row][col] = cellArrays[cellArrayIdx][row][col]

        # Switch indices
        cellArrayIdx = newArrayIdx

    return

# This tests an array with a checkerboard pattern.  Each update will be in
# some way symmetric
def createTestAlternate():
    """
    Creates checkerboard test.
    """
    global cellArrays

    # Fill array
    for row in range(1, n+1):
        for col in range(1, n+1):
            cellArrays[cellArrayIdx][row][col] = 0
            if (row%2 == col%2):
                cellArrays[cellArrayIdx][row][col] = 1

    # Return without updating cell array index because we want the first run
    # to use what we just filled
    return

# This test creates a random array and puts it in.  It will generate a
# different test on every run.
def createTestRandom():
    """
    Creates a random cell array.
    """
    global cellArrays

    # Fill array
    for row in range(1, n+1):
        for col in range(1, n+1):
            cellArrays[cellArrayIdx][row][col] = random.randint(0, 1)

    # Return without updating cell array index because we want the first run
    # to use what we just filled
    return

# This creates an array with everything on.
def createTestAllAlive():
    """
    Fill the array with all live cells.
    """
    global cellArrays

    # Fill array
    for row in range(1, n+1):
        for col in range(1, n+1):
            cellArrays[cellArrayIdx][row][col] = 1

    # Return without updating cell array index because we want the first run
    # to use what we just filled
    return

# This prints the array
def printArray(f):
    for row in range(1, n+1):
        out = ""
        for col in range(1, n+1):
            out += str(cellArrays[cellArrayIdx][row][col])
        f.write(out + "\n")

# Writes the test cases
def runTest(f):
    # Allocates space for values
    initializeArray(n)

    f.write(str(n) + "\n")

    # Turn all cells on
    createTestAllAlive()
    f.write("0\n")
    printArray(f)
    update(1)
    f.write("1\n")
    printArray(f)
    update(3)
    f.write("3\n")
    printArray(f)

    # Tell VHDL file that we are starting new test
    f.write("0\n")

    # Does an alternating test, which will have a nice, symmetrical result.
    createTestAlternate()
    # Initial cell values
    printArray(f)

    # Update once, record that only one step should be taken, and print
    # expected cell values
    update(1)
    f.write("1\n")
    printArray(f)
    # Update a few times
    for i in range(5):
        update(1)
        f.write("1\n")
        printArray(f)

    # Write a 0, telling VHD test bench to reset array
    f.write("0\n")

    # Use random configuration and test some iterations.  Each new iteration
    # will essentially be also random.  If this works, the previous one does,
    # and the ones hard coded in the VHDL file, we assume the cell array is
    # functional
    createTestRandom()
    printArray(f)
    for i in range(20):
        update(1)
        f.write("1\n")
        printArray(f)

    return

if __name__ == "__main__":
    assert(len(sys.argv) == 3)
    n = int(sys.argv[2])
    f = open(sys.argv[1], "w")
    runTest(f)
    f.close()
