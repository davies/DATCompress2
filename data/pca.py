import sys
import struct
import pca_module
import numpy

def pca(f):
    d = f.read()
    f.close()
    X,Y,L = struct.unpack("HHH", d[:6])
    X = 3
    data = []
    avg = [0] * L
    M = 20
    for x in range(X):
        for y in range(Y):
            dd = struct.unpack("H"*L, d[(x*Y+y)*L*2+6:(x*Y+y+1)*L*2+6])
            for i in xrange(L):
                avg[i] += dd[i]
            data.append(dd)
    for i in xrange(L):  
        avg[i] /= X*Y
    for i in xrange(X*Y):
        av = [(data[i][j]-avg[j]+M/2)/M for j in xrange(L)]
        pred = [av[j]-av[j-1] for j in xrange(1,L)]
        data[i] = pred
#        print pred
    T, P, e = pca_module.PCA_svd(numpy.array(data))
    t = T.tolist()
    print len(t), len(t[0]), t[0] 
    p = P.tolist()
    print len(p), len(p[0]), p[0]
 
pca(open(sys.argv[1]))
