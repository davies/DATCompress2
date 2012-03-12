
import sys, os
import struct

for path in sys.argv[1:]:
    lines = open(path).readlines()
    last = lines[-1].split('\t')
    X,Y = last[:2]
    X,Y = int(X)+1, int(Y)+1
    L = len(last)-2
    print path, X,Y,L
    if os.path.exists(path[:-3] + 'dat'): continue
    f = open(path[:-3] + 'dat', 'w')
    f.write(struct.pack("HHH", X,Y,L))
    for line in lines:
        f.write(struct.pack("H"*L, *[int(x) for x in line.split('\t')[2:]]))
    if (3 + X*Y*L) % 2 != 0:
        f.write(struct.pack("H", 0))
    f.close()

