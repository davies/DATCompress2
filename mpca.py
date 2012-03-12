
from pca import *
import struct
import numpy

path = 'data/c1b255d9ce8312a2c5e613f57972f19b.dat'
d = open(path).read()
X,Y,L = struct.unpack("H"*3, d[:6])
N = X*Y
data = [None] * N
K = L
for i in range(N):
    num = struct.unpack("H"*L, d[i*L*2+6:(i+1)*L*2+6])
#    num = [n-num[0] for n in num] 
#    if max(num) < 100: 
#        print num
#        continue
    data[i] = num
    
A = numpy.array(data)
print A[0]
c = Center(A)
print c.mean
np.set_printoptions(1, threshold=100, suppress=True )

p = PCA(A, 0.99)
p.npc = 5
print 'npc:', p.npc

std = c.std[0]
diff = 0.0
valid = 0
x_st = []
#data = data[1000:]
for d in data:
    if not d: continue
    ox = numpy.array([float(i) for i in d])
    x = (ox - c.mean) / std
    x1 = p.vars_pc(x)
#    x1 = numpy.array([int(k*10)*0.1 for k in x1])
#    x1[0] = int(x1[0] / 10) * 10
#    x1[1] = int(x1[1] / 0.01) * 0.01
#    x1[4] = int(x1[4] / 1) * 1
    x_st.append(x1)
    xx = p.pc_vars(x1)
    #* std + c.mean
    v = (x - xx) * std
#    print v
    dd = sum([i*i for i in v])
#    if dd > 36 * L * 2:
#        print dd, v
#        continue

#    print v
    #dd = numpy.linalg.norm(v) * L
#    print 'diff', dd / len(d)
    diff += dd
    valid += 1

for i in range(p.npc):
    xx = [x_st[j][i] for j in range(len(x_st))]
    print i, max(xx), min(xx)

print diff / valid / len(data[0]), valid * 1.0 / len(data)
