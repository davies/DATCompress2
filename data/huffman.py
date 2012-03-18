import os
import marshal
import cPickle
import array

class HuffmanNode(object):
    recurPrint = False
    def __init__(self, ch=None, fq=None, lnode=None, rnode=None, parent=None):
        self.L = lnode
        self.R = rnode
        self.p = parent
        self.c = ch
        self.fq = fq
        
    def __repr__(self):
        if HuffmanNode.recurPrint:
            lnode = self.L if self.L else '#'  
            rnode = self.R if self.R else '#'        
            return ''.join( ('(%s:%d)'%(self.c, self.fq), str(lnode), str(rnode) ) )
        else:
            return '(%s:%d)'%(self.c, self.fq)
    
    def __cmp__(self, other):
        if not isinstance(other, HuffmanNode):
            return super(HuffmanNode, self).__cmp__(other)
        return cmp(self.fq, other.fq)

def _pop_first_two_nodes(nodes):
    if len(nodes)>1:
        first=nodes.pop(0)
        second=nodes.pop(0)
        return first, second
    else:
        #print "[popFirstTwoNodes] nodes's length <= 1"
        return nodes[0], None
        
def _build_tree(nodes):    
    nodes.sort()
    while(True):
        first, second = _pop_first_two_nodes(nodes)
        if not second:
            return first
        parent = HuffmanNode(lnode=first, rnode=second, fq=first.fq+second.fq)
        first.p = parent
        second.p = parent
        nodes.insert(0, parent)
        nodes.sort()

def _gen_huffman_code(node, dict_codes, buffer_stack=[]):
    if not node.L and not node.R:
        dict_codes[node.c] = ''.join(buffer_stack)
        return
    buffer_stack.append('0')
    _gen_huffman_code(node.L, dict_codes, buffer_stack)
    buffer_stack.pop()
    
    buffer_stack.append('1')
    _gen_huffman_code(node.R, dict_codes, buffer_stack)
    buffer_stack.pop()

class MyHuffman:
    def __init__(self):
        self.freq = {1:{}, 2:{}, 4:{}, 6:{}, 8:{}, 12:{}}
        self.encoding = {}

    def _update(self, seq, whole=True):
        n = len(seq)
        if whole and sum(1 for i in seq if -1<=i<=1) == len(seq) or n==1 and -10<= seq[0] <=10:
            k = tuple(seq)
            self.freq[len(seq)][k] = self.freq[len(seq)].get(k, 0) + 1
            return
        self.freq[len(seq)]['other'] = self.freq[n].get('other', 0) + 1
        if n > 1:
            if n % 3 ==0:
                self._update(seq[:n/3])
                self._update(seq[n/3:-n/3])
                self._update(seq[-n/3:])
            elif n % 2==0:
                self._update(seq[:n/2])
                self._update(seq[n/2:])

    def update(self, seqs):
        for s in seqs:
            self._update(s)

    def build(self):
        for k in reversed(sorted(self.freq)):
            freq = self.freq[k]
            if not freq: continue
            #move
            if k > 1:
                n = sum(freq.itervalues())
                limit = n / (1<<16)
                for l in freq.keys():
                    if freq[l] < limit:
                        self._update(l)
                        del freq[l]

            root = _build_tree(
                [HuffmanNode(ch=ch, fq=int(fq)) for ch, fq in self.freq[k].iteritems()]
                )
            codemap = {}
            _gen_huffman_code(root, codemap)
            self.encoding[k] = codemap
            for k in sorted(codemap):
                print k, codemap[k]
            print

    def guess(self):
        for s, enc in sorted(self.encoding.iteritems()):
            n = sum(self.freq[s].itervalues())
            l = 0.0
            for k,c in self.freq[s].iteritems():
                l += c*1.0 / n * len(enc[k])
            print s, l, len(enc.get('other','')), (self.freq[s].get('other',0))*1.0/n

    def _calc(self, c):
        n = len(c)
        if c in self.encoding[n]:
            return len(self.encoding[n][c])
        s = len(self.encoding[n]['other'])
        if n > 1:
            if n % 3 ==0:
                s += self._calc(c[:n/3]) + self._calc(c[n/3:-n/3]) + self._calc(c[-n/3:])
            else: 
                s += self._calc(c[:n/2]) + self._calc(c[n/2:])
        else:
            s += 5
        return s

    def calc(self, ll):
        self.update(ll)
        self.build()
        self.guess()
        return sum(self._calc(i) for i in ll)
            

def _cal_freq(long_str):
    from collections import defaultdict
    d = defaultdict(int)
    for c in long_str:
        d[c] += 1
    ks = d.keys()
#    print min(ks), max(ks)
#    for n in range(-64, 80):
#        d[n] = d.get(n, 0)
    return d

MAX_BITS = 8

class Encoder(object):
    def __init__(self, filename_or_long_str=None):
        if filename_or_long_str:
            if os.path.exists(filename_or_long_str):
                self.encode(filename_or_long_str)
            else:
                print '[Encoder] take \'%s\' as a string to be encoded.'\
                      % filename_or_long_str
                self.long_str = filename_or_long_str

    def __get_long_str(self):
        return self._long_str
    def __set_long_str(self, s):
        self._long_str = s
        if s:
            self.root = self._get_tree_root()
            self.code_map = self._get_code_map()
            for k, v in sorted(self.code_map.items()):
                if len(v) <= 7:
                    print k,v
            self.array_codes, self.code_length = self._encode()
    long_str = property(__get_long_str, __set_long_str)
    
    def _get_tree_root(self):
        d = _cal_freq(self.long_str)
        return _build_tree(
            [HuffmanNode(ch=ch, fq=int(fq)) for ch, fq in d.iteritems()]
            )

    def _get_code_map(self):
        a_dict={}
        _gen_huffman_code(self.root, a_dict)
        return a_dict
        
    def _encode(self):
        array_codes = array.array('B')
        code_length = 0
        buff, length = 0, 0
        for ch in self.long_str:
            code = self.code_map[ch]        
            for bit in list(code):
                if bit=='1':
                    buff = (buff << 1) | 0x01
                else: # bit == '0'
                    buff = (buff << 1)
                length += 1
                if length == MAX_BITS:
                    array_codes.extend([buff])
                    buff, length = 0, 0

            code_length += len(code)
            
        if length != 0:
            array_codes.extend([buff << (MAX_BITS-length)])
            
        return array_codes, code_length

    def encode(self, filename):
        fp = open(filename, 'rb')
        self.long_str = fp.read()
        fp.close()

    def write(self, filename):
        if self._long_str:
            fcompressed = open(filename, 'wb')
            marshal.dump(
                (cPickle.dumps(self.root), self.code_length, self.array_codes),
                fcompressed)
            fcompressed.close()
        else:
            print "You haven't set 'long_str' attribute."

class Decoder(object):
    def __init__(self, filename_or_raw_str=None):
        if filename_or_raw_str:
            if os.path.exists(filename_or_raw_str):
                filename = filename_or_raw_str
                self.read(filename)            
            else:
                print '[Decoder] take \'%s\' as raw string' % filename_or_raw_str
                raw_string = filename_or_raw_str
                unpickled_root, length, array_codes = marshal.loads(raw_string)
                self.root = cPickle.loads(unpickled_root)
                self.code_length = length        
                self.array_codes = array.array('B', array_codes)

    def _decode(self):
        string_buf = []
        total_length = 0    
        node = self.root
        for code in self.array_codes:
            buf_length = 0
            while (buf_length < MAX_BITS and total_length != self.code_length):
                buf_length += 1
                total_length += 1            
                if code >> (MAX_BITS - buf_length) & 1:
                    node = node.R
                    if node.c:
                        string_buf.append(node.c)
                        node = self.root
                else:
                    node = node.L
                    if node.c:
                        string_buf.append(node.c)
                        node = self.root

        return ''.join(string_buf)        

    def read(self, filename):
        fp = open(filename, 'rb')
        unpickled_root, length, array_codes = marshal.load(fp)        
        self.root = cPickle.loads(unpickled_root)
        self.code_length = length        
        self.array_codes = array.array('B', array_codes)
        fp.close()

    def decode_as(self, filename):
        decoded = self._decode()
        fout = open(filename, 'wb')
        fout.write(decoded)
        fout.close()

if __name__=='__main__':
    original_file = 'filename.txt'
    compressed_file = 'compressed.scw'
    decompressed_file = 'filename2.txt'

    # first way to use Encoder/Decoder
    enc = Encoder(original_file)    
    enc.write(compressed_file)
    dec = Decoder(compressed_file)
    dec.decode_as(decompressed_file)

    # second way
    #enc = Encoder()
    #enc.encode(original_file)
    #enc.write(compressed_file)
    #dec = Decoder()
    #dec.read(compressed_file)
    #dec.decode_as(decompressed_file)
