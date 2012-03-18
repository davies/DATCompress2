import os, sys
import struct
import huffman

def smoothy(pred):        
    for i in range(len(pred)-1):
        if pred[i]+pred[i+1] == 0 and pred[i]*pred[i+1] == -1:
            pred[i] = 0
            pred[i+1] = 0

def frequancy(comp):
    est = {} 
    for e in comp:
        est[e] = est.get(e, 0) + 1
    su = sum(est.itervalues())
    for e in sorted(est):
        print e, est[e] * 100.0 / su

def compress1(data):
    out = []
    size = 0
    encoding = {-1:'100', 0: '0', 1:'110'}
    for d, in data:
        if d in encoding:
            size += len(encoding[d])
#            out.append(encoding[d])
        else:
#            out.append('111')
#            out.append(d > 0 and '1' or '0')
#            out.append('0' * (abs(d)-2))
#            out.append('1')
            size += 2 + abs(d) - 2 + 2
    return size, ''.join(out)

def compress2(data):
    size = 0
    out = []
    encoding = {
        (-1,0): '100',
#        (-1,1): '1010',
        (0,-1): '101',
        (0,0):  '0',
        (0,1):  '110',
#        (1,-1): '1101',
        (1,0):  '1110',
    }
    for d in data:
        if d in encoding:
            size += len(encoding[d])
#            out.append(encoding[d])
        else:
            d1, d2 = d
            size += 4 + abs(d1) + 2 + abs(d2) + 2 
    return size, ''.join(out)

def two_byte(d1, d2):
    encoding = {
        (-1,0): '101',
#        (-1,1): '1010',
        (0,-1): '100',
        (0,0):  '0',
        (0,1):  '110',
#        (1,-1): '1101',
        (1,0):  '1110',
    }
    if (d1,d2) in encoding:
        return len(encoding[(d1,d2)])
    return 4 + abs(d1) + 2 + abs(d2) + 2

def compress4(data):
    size = 0
    out = []
    encoding = {
        (0,0,0,0): '0',
        (-1,0,0,0): '1000',
        (0,-1,0,0): '1001',
        (0,0,-1,0): '1010',
        (0,0,0,-1): '1011',
        (1,0,0,0):  '11000',
        (0,1,0,0):  '11001',
        (0,0,1,0):  '11010',
        (0,0,0,1):  '11011',
    }
    for d in data:
        if d in encoding:
            size += len(encoding[d])
        else:
            size += 3
            size += two_byte(*d[:2]) + two_byte(*d[2:])
    return size, ""

def compress6(data):
    size = 0
    out = []
    encoding = {
        (0,0,0,0,0,0): '0',
        (-1,0,0,0,0,0): '10000',
        (0,-1,0,0,0,0): '10001',
        (0,0,-1,0,0,0): '10010',
        (0,0,0,-1,0,0): '101000',
        (0,0,0,0,-1,0): '101001',
        (0,0,0,0,0,-1): '101010',
        (1,0,0,0,0,0):  '111011',
        (0,1,0,0,0,0):  '111100',
        (0,0,1,0,0,0):  '111101',
        (0,0,0,1,0,0):  '111110',
        (0,0,0,0,1,0):  '111111',
        (0,0,0,0,0,1):  '10011',
    }
    for d in data:
        if d in encoding:
            size += len(encoding[d])
        else:
            if len(d) == 6:
                size += 2
                size += two_byte(*d[:2]) + two_byte(*d[2:4]) + two_byte(*d[4:])
            else:
                size += two_byte(*d[:2]) + two_byte(*d[2:4])

    return size, ""

def get_avg(path):
    f = open(path)
    X,Y,L = struct.unpack("H"*3, f.read(6))
    for k in range(1):
        avg = [0] * L
        size = 0
        for x in range(30):
            for y in range(Y):
                d = struct.unpack("H"*L, f.read(L*2))
    #            d = [n-d[0] for n in d]
    #            m = max(d)
    #            d = [n*512/(m or 1) for n in d]
                if d[0] > 200:
                    for i in range(L):
                        avg[i] += d[i]
                    size += 1
        for i in range(L):
            avg[i] = (avg[i]) / size
#        print >>sys.stderr, avg
    return avg

def stat(path):
    avg = get_avg(path)
    
    N = 17
    M = 12
    f = open(path)
    X,Y,L = struct.unpack("H"*3, f.read(6))
    print path, X, Y, L
    X = 30
#    Y = 10
    diff = 0
    size = 0
    all = []
    davg = [0] * L
    dst = {}
    hst = {}

    huff = huffman.MyHuffman()
    for xx in xrange(X*Y):
        row = struct.unpack("H"*L, f.read(L*2))
        d = [row[i]-avg[i] for i in range(L)]
        dd = [(i+N/2)/N for i in d]
        pred = [dd[i]-dd[i-1] for i in range(1, L)]
        
        hst[(dd[0]+16)/32] = hst.get((dd[0]+16)/32, 0) + 1
        
        # TODO: need check before
        smoothy(pred)
        
#        for i in range(L-1):
#            davg[i] += pred[i]

        for i in range(1, L):
            dd[i] = dd[i-1] + pred[i-1]
        df = sum((a-b*N)**2 for a,b in zip(d, dd)) / L
        diff += df
        
#        for i in range(1, L-1):
#            pred[i] -= pred[i-1]/2
#        pred = [pred[0]] + [pred[i]-pred[i-1] for i in range(1, L-1)]
#        print df, dd, pred
        comp = [tuple(pred[i*M:i*M+M]) for i in range(len(pred)/M) ]
        for i in range(len(comp)):
            dst[comp[i]] = dst.get(comp[i], 0) + 1

        if M == 2:
            size += compress2(comp)[0]
        elif M == 1:
            size += compress1(comp)[0]
        elif M == 4:
            size += compress4(comp)[0]
        elif M==6:
            size += compress6(comp)[0]
#        huff.update(comp)
        all.extend(comp)
#    davg = [(i+X*Y/2)/X/Y for i in davg]
#    print davg

#    for k in sorted(hst):
#        print k, hst[k] * 100.0 / X / Y

#    huff.build()
#    huff.guess()
    size2 = huff.calc(all)
    print 'new ', 16 / ((float(size2) / X / Y + 11) / L)

    other = 100.0
    for k in sorted(dst):
        r = dst[k] * 100.0 / X / Y / (L-1)*M
        if r > 1:
            other -= r
            print k, r
    print 'other', other
    print diff / (X * Y), 16 / ((float(size) / X / Y + 11) / L)
    
#    frequancy(comp)

#    enc = huffman.Encoder()
#    enc.long_str = header
#    print float(enc.code_length) / len(data)
    #enc = huffman.Encoder()
    #enc.long_str = all
    #print 16 / ((float(enc.code_length) / X /Y + 11) / L)
     
for p in sys.argv[1:]:
    stat(p)    
