import sys
import os

f = sys.argv[1]

try:
    f = open(sys.argv[1], 'r')
    f2 = open('temp', 'w')
    for line in f.readlines():
        f2.write(line)
    f2.close()
    f.close()

    f = open('temp', 'r')
    f2 = open(sys.argv[1], 'w')
    for line in f.readlines():
        if (line[:15] == "$scope module ("):
            lineList = list(line)
            lineList.remove("(")
            lineList.remove(")")
            line = "".join(lineList)
        f2.write(line)
    f2.close()
    f.close()

    os.remove('temp')
except:
    pass
