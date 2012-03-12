import os, sys
import struct
import huffman

def smooth(x):
    y = list(x)
    for i in range(1, len(x)-1):
        y[i] = (x[i-1] + x[i+1] + x[i]) / 3
    return y



def stat(path):
    f = open(path)
    X,Y,L = struct.unpack("H"*3, f.read(6))
    X = 100
#    Y = 10
    print path, X, Y, L
    diff = 0
    bad = 0

    data = []
    avg = [0] * L
    for x in range(X):
        for y in range(Y):
            d = struct.unpack("H"*L, f.read(L*2))
#            d = [n-d[0] for n in d]
#            m = max(d)
#            d = [n*512/(m or 1) for n in d]
            data.append(d)
            if d[0] > 200:
                for i in range(L):
                    avg[i] += d[i]

    for i in range(L):
        avg[i] /= len(data)
    print avg
    
    comp = []
    header = []
    diff = 0
    bad = 0
    sss = set()
    N = 20
#    st = 
    for row in data:
        d = [row[i]-avg[i] for i in range(L)]
        dd = [(i+N/2)/N for i in d]
        pred = [dd[i]-dd[i-1] for i in range(1, L)]
        if max(pred) > 250 or min(pred) < -250:
            print pred
#        for i in range(L-2):
#            while pred[i] < -1 :#or (i and pred[i-1]+pred[i] <= -2):
#                pred[i] += 1
#                pred[i+1] -= 1
#            while pred[i] > 2 :#or (i and pred[i-1]+pred[i] >= 4):
#                pred[i] -= 1
#                pred[i+1] += 1
#        if pred[-1] < -1:
#            pred[-1] = -1
#        if pred[-1] > 2:
#            pred[-1] = 2
        
        for i in range(1, L):
            dd[i] = dd[i-1] + pred[i-1]
        df = sum((a-b*N)**2 for a,b in zip(d, dd)) / L
        diff += df
        
#        for i in range(1, L-1):
#            pred[i] -= pred[i-1]/2
#        pred = [pred[0]] + [pred[i]-pred[i-1] for i in range(1, L-1)]
#        print df, dd, pred
        M = 1
        def toint(l):
            s = 0
            for n in l:
                s <<= 2
                s += n + 1
            return s
        pred = [tuple(pred[i*M:i*M+M]) for i in range(len(pred)/M) ]
        #if pred not in sss:
        comp.extend(pred)
        header.append(dd[0])
 
    print diff / (X * Y -bad), len(sss) * 100.0 / X / Y
    
#    enc = huffman.Encoder()
#    enc.long_str = header
#    print float(enc.code_length) / len(data)
    #enc = huffman.Encoder()
    #enc.long_str = comp
    #print float(enc.code_length) / len(data) / (L-1)
     
for p in sys.argv[1:]:
    stat(p)    
