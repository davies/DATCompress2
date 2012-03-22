//#define KEEP_HALF

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <queue>
#include <assert.h>

//#define assert(x) 

using namespace std;

typedef long long ll;
typedef vector<int> VI;
typedef vector< VI > VVI;
typedef vector<string> VS;
typedef istringstream ISS;
typedef ostringstream OSS;

class myvector : public std::vector<int>
{
public:
  myvector(int n)
  {
    this->_M_impl._M_start = new int[n];
    this->_M_impl._M_finish = this->_M_impl._M_start + n;
      this->_M_impl._M_end_of_storage = this->_M_impl._M_start + n;
  }
  void myresize(int n)
  {
    this->_M_impl._M_finish = this->_M_impl._M_start + n;
  }
};


#define FOR(i,a,b) for(int i=(a);i<int(b);++i)
#define REP(i,n) FOR(i,0,n)
#define SZ(v) ((int)(v).size())
#define DV(v) REP(_i,SZ(v)) cerr << v[_i] << " "; cerr << endl
#define PII pair<int, int>
#define MP(x,y) make_pair<int,int>(x,y)

#ifdef USE_EXPECT
#define likely(x)    __builtin_expect((x), 1)
#define unlikely(x) __builtin_expect((x), 0)
#else
#define likely(x)    (x)
#define unlikely(x) (x)
#endif

struct Bits {
    int data;
    int bits;
    short* p;
    Bits(short *_p) {
        data = 0;
        bits = 0;
        p = _p;
    }
    void write(int d, int b) {
//        cout << "bits write " << b << " " << d << endl;
        assert(b > 0 && b <= 16);
        assert((d & (~((1<<b)-1))) == 0);
        d &= (1<<b) - 1;
        data <<= b;
        data |= d;
        bits += b;
        if (bits > 16) {
            *p++ = (data >> (bits-16)) & 0xffff;
//            cout << "flush " << p[-1] << endl;
            bits -= 16;
        }
    }
    void flush() {
        if (bits > 0) {
            data <<= 16-bits;
            *p ++ = data & 0xffff;
//            cout << "flush " << p[-1] << endl;
            bits = 0;
        }
    }

    int peek(int b) {
        if (bits < 16) {
            bits += 16;
            data <<= 16;
            data |= (*p++) & 0xffff;
//            cout << "load " << p[-1] << endl;
        }
        int r = (data >> (bits-b)) & ((1<<b)-1);
  //      cout << "peek " << b << " " << r << endl;
        return r;
    }
    void shift(int b) {
//        cout << "shift " << b << endl;
        bits -= b;
    }
    int read_bit() {
        int r = peek(1);
        shift(1);
        return r;
    }
    int read(int n) {
        int r = peek(n);
        shift(n);
//        cout << "read " << n << " " << r << endl;
        return r;
    }
    void rollback(void) {
        if (bits >= 16) {
            p --;
            data >>= 16;
            bits -= 16;
        }
    }
};
    
inline int bitCount(int x) {
    int b=0;
    while (x > 0) {
        b++;
        x >>= 1;
    }
    return b;
}

inline int encodeSize(int x) {
    if (x < 8) return x;
    int bc = bitCount(x);
    return -8 + (bc <<2) + ((x >> (bc-3)) & 3);
}

inline int decodeSize(int x) {
    if (x<8) return x;
    int bc = (x >>2) + 2;
    int v = (4 + (x & 3) ) << (bc -3);
//        if (bc >= 5) v += 1 << (bc-5);
    return v;
}


struct Symbol {
    unsigned short low_count;
    unsigned short high_count;
    int count;
};

const int MAX_NUMBER = 32;
const int SYMBOL_COUNT = MAX_NUMBER * 2 + 1;
const int GROUPS = 60*16;
const int NUM_IN_GROUP = 2;

struct ArCoder {
    unsigned short int code;
    unsigned short int low;
    unsigned short int high;
    int underflow_bits;
    bool init;
    int scale[GROUPS];
    Symbol symbols[GROUPS][SYMBOL_COUNT];
    char index[GROUPS][1<<16];
    ArCoder(): init(0) {
        memset(symbols, 0, sizeof(symbols));
        memset(scale, 0, sizeof(scale));
        memset(index, 0, sizeof(index));
    }
    
    void learn(short *src, int size) {
        REP(i, size) {
            //assert(src[i] >= -MAX_NUMBER && src[i] <= MAX_NUMBER);
            int idx = get_ctx(i, src);
            if (abs(src[i]) < MAX_NUMBER) {
                symbols[idx][src[i]+MAX_NUMBER].count ++;
            } else {
//                symbols[idx][0].count ++;
//                symbols[idx][src[i]/MAX_NUMBER + MAX_NUMBER].count ++;
//                symbols[idx][src[i] % MAX_NUMBER + MAX_NUMBER].count ++;
            }
        }
        if(!init) init = true;
    }

    void train() {
        norm();
        REP(k, GROUPS) {
            int s = 0;
            REP(i, SYMBOL_COUNT) s += symbols[k][i].count;
            if (s > (1<<14)) {
                float n  = float(s) / (1<<14);
                REP(i, SYMBOL_COUNT) {
                    if (symbols[k][i].count > 0) {
                        symbols[k][i].count = int((symbols[k][i].count) / n);
                        //if (symbols[k][i].count == 0 ) symbols[k][i].count = 1;
                    }
                }
            } else if (s < 500 && s > 0) {
                int n = 1000 / s;
                REP(i, SYMBOL_COUNT) {
                    if (symbols[k][i].count > 0) {
                        symbols[k][i].count = symbols[k][i].count * n;
                        assert(symbols[k][i].count > 0 && symbols[k][i].count < (1<<15));
                    }
                }
            }
            // for fallback
            if (symbols[k][0].count == 0) symbols[k][0].count = 1;
            if (symbols[k][SYMBOL_COUNT-1].count == 0) symbols[k][SYMBOL_COUNT-1].count = 1;
            if (k%16==5)
            FOR(i, 0, SYMBOL_COUNT) {
                if (symbols[k][i].count == 0) symbols[k][i].count = 1;
            }

            scale[k] = 0;
            // FIXME overflow
            REP(i, SYMBOL_COUNT) {
//                assert(symbols[k][i].count > 0 && symbols[k][i].count < (1<<15));
                symbols[k][i].low_count = scale[k];
                symbols[k][i].high_count = symbols[k][i].count + scale[k];
                scale[k] = symbols[k][i].high_count;
                symbols[k][i].count /= 2;
    //            if (symbols[i].high_count > symbols[i].low_count) {
    //                printf("symbol %d: %d - %d\n", i - MAX_NUMBER, symbols[i].low_count, symbols[i].high_count);
    //            }
                FOR(j, symbols[k][i].low_count, symbols[k][i].high_count) {
                    index[k][j] = i;
                }
            }
//            cout << "all " << scale[k] << endl;
        }
    }
  
    void norm() {
        REP(k, GROUPS) {
            int s = 0;
            REP(i, SYMBOL_COUNT) {
                symbols[k][i].count = decodeSize(encodeSize(symbols[k][i].count));
                s += symbols[k][i].count;
            }
        }
    }

    void save(Bits &bits) {
        int used = 0;
        int tmp[SYMBOL_COUNT];
        REP(k, GROUPS) {
            int m = 0;
            REP(i, SYMBOL_COUNT) {
                tmp[i] = encodeSize(symbols[k][i].count);
                if (tmp[i] > m) {
                    m = tmp[i];
                }
            }
            int bps = bitCount(m);
            bits.write(bps, 3); 
//            cout << k << " " << bps << endl;
            if (bps==0) continue;
            REP(i, SYMBOL_COUNT) {
                if (tmp[i] > 0) {
                    bits.write(1, 1);
                    bits.write(tmp[i], bps);
                    used += 1+bps;
                } else {
                    bits.write(0, 1);
                    used += 1;
                }
            }
        }
        cout << "used " << used << endl;
    }

    void load(Bits &bits) {
        memset(symbols, 0, sizeof(symbols));
        REP(k, GROUPS) {
            int bps = bits.read(3);
//            cout << "load " << k << " " << bps << endl;
            if (bps==0) continue;
            REP(i, SYMBOL_COUNT) {
                if (bits.read_bit()) {
                    symbols[k][i].count = decodeSize(bits.read(bps));
                }
            }
        }
        if(!init) init = true;
    }

    void init_encoder() {
        low = 0;
        high = 0xffff;
        underflow_bits = 0;
    }

    void encode_symbol(int ctx, Symbol* s, Bits &bits) {
        if (s->high_count == s->low_count) {
            printf("wrong %d\n", s->low_count);
            assert(s->high_count > s->low_count);
        }
        
        unsigned int range = high - low + 1;
        high = low + (range * s->high_count / scale[ctx]) - 1;
        low += range * s->low_count / scale[ctx];

        for(;;) {
            if ((high & 0x8000) == (low & 0x8000)) {
                bits.write((high >> 15), 1);
                while (underflow_bits > 0) {
                    bits.write(((~high)>>15)&1, 1);
                    underflow_bits -- ;
                }
            } else if ((low & 0x4000) && !(high & 0x4000)) {
                underflow_bits += 1;
                low  &= 0x3fff;
                high |= 0x4000;
            } else {
                break;
            }
            low <<= 1;
            high <<= 1;
            high |= 1;
        }
    }
    
    void encode(short *src, int size, Bits &bits) {
        REP(i, size) {
            int idx = get_ctx(i, src);
            int idx2 = (idx&0xfff0)+5;
            if (abs(src[i]) < MAX_NUMBER) {
                Symbol *s = symbols[idx] + src[i] + MAX_NUMBER;
                if (s->high_count == s->low_count) {
                    encode_symbol(idx, symbols[idx] + MAX_NUMBER*2, bits); 
                    encode_symbol(idx2, symbols[idx2] + src[i] + MAX_NUMBER, bits); 
                } else {
                    encode_symbol(idx, s, bits); 
                }
            } else {
                encode_symbol(idx, symbols[idx] + 0, bits); 
                encode_symbol(idx2, symbols[idx2] + src[i]/MAX_NUMBER + MAX_NUMBER, bits); 
                encode_symbol(idx2, symbols[idx2] + src[i]%MAX_NUMBER + MAX_NUMBER, bits); 
            }
        }
        learn(src, size);
    }

    void flush(Bits &bits) {
        int b = (low>>14) & 1;
        bits.write(b, 1);
        underflow_bits++;
        while (underflow_bits-- > 0) 
            bits.write((~b)&1, 1);
    }

    void init_decode(Bits &bits) {
        code = bits.read(16);
        low = 0;
        high = 0xffff;
    }

    int decode_symbol(Bits &bits, int ctx) {
        unsigned int range = high - low + 1;
        unsigned int count = (((code-low) + 1) * scale[ctx] - 1) / range;
        Symbol *s = &symbols[ctx][index[ctx][count]];
        high = low + range * s->high_count / scale[ctx] - 1;
        low += range * s->low_count / scale[ctx];
        for (;;) {
            if ( (high & 0x8000) == (low & 0x8000)) {
                
            } else if ( (low & 0x4000) == 0x4000 && (high&0x4000)==0) {
                code ^= 0x4000;
                low  &= 0x3fff;
                high |= 0x4000;
            } else {
                break;
            }
            low <<= 1;
            high <<= 1;
            high |= 1;
            code <<= 1;
            code |= bits.read_bit();
        }
        return index[ctx][count] - MAX_NUMBER;
    }

    int get_ctx(int i, short *src) {
            int p1 = (i>0 ? (src[i-1]<=-1 ? 0 : (src[i-1]>2 ? 3 : src[i-1]+1)): 0);
            int p2 = (i>1 ? (src[i-2]<=-1 ? 0 : (src[i-2]>2 ? 3 : src[i-2]+1)): 0);
            int idx = (i/NUM_IN_GROUP + i%2)*16 + p1 * 4 + p2;
            return idx;
    }

    void decode(short *dst, int size, Bits &bits) {
        REP(i, size) {
            int idx = get_ctx(i, dst);
            int v = decode_symbol(bits, idx);
            if (v == -MAX_NUMBER) {
                v = decode_symbol(bits, (idx&0xfff0)+5) * MAX_NUMBER + decode_symbol(bits, (idx&0xfff0)+5);
            } else if (v == MAX_NUMBER) {
                v = decode_symbol(bits, (idx&0xfff0)+5);
            }
            dst[i] = v; 
        }
        learn(dst, size);
    }
};

void test_arcoding() {
    short nums[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 13, 15};
    struct ArCoder *coder = new ArCoder;
    coder->learn(nums, 15);

    short buf[300];
    struct Bits w(buf);
    coder->save(w);
    coder->train();
    coder->init_encoder();
    coder->encode(nums, 15, w);
    coder->flush(w);
    w.flush();
//    REP(i, 300) cout << buf[i] << " " ; cout << endl;

    short rbuf[20];
    struct Bits r(buf);
    coder->load(r);
    coder->train();
    coder->init_decode(r);
    coder->decode(rbuf, 15, r);
    REP(i, 15) {
        if (nums[i] != rbuf[i]) printf("fail %d %d != %d\n", i, nums[i], rbuf[i]);
    }

    delete coder;
}


const int TRY_STEP = 7;
const int STEP = 8;
const int NUM_LIMIT = 30;
const int NUM_BITS = 11;
const int NUM_LIMIT2 = 3;
const int NUM_SIZE2 = NUM_LIMIT2*2+1;
const int TREE_NUM = 8;
const int SZ[] = {NUM_LIMIT*2+2, NUM_SIZE2*NUM_SIZE2+1, 0, 9*9+1, 0, 27*27+1, 0, 81*81+1};

const int MAX_SLOTS = 1<<12;
const int MAX_TREE_SIZE = 81*81+1;
const int LUT_SIZE=16;

#define next_step(step) ((step) % 3==0 ? (step)/3 : ((step) % 2==0 ? (step)/2 : 1))

struct Huffman {
    int *sizes[TREE_NUM];
    int osizes[TREE_NUM];
    int bps[TREE_NUM];
    int opos[TREE_NUM];
    
    int order[MAX_TREE_SIZE*2];
    int parent[MAX_TREE_SIZE];
        
    int HTCodes[MAX_TREE_SIZE*3];
    int HTCodesPos;
    int *HTTree[TREE_NUM];
    int HTData[TREE_NUM][1<<LUT_SIZE];

    Huffman() {
        memset(HTCodes, 0, sizeof(HTCodes));
        HTCodesPos = 0;
        memset(HTTree, 0, sizeof(HTTree));
        memset(HTData, 0, sizeof(HTData));
        REP(i, TREE_NUM) {
            osizes[i] = 0;
            bps[i] = 0;
            opos[i] = 0;
            if (SZ[i]==0) {
                sizes[i] = NULL;
                continue;
            }
//            cout << i << " alloc " << SZ[i] << endl;
            sizes[i] = new int[SZ[i]];
            memset(sizes[i], 0, sizeof(int) * SZ[i]);
        }
    };
    ~Huffman() {
        REP(i, TREE_NUM) if (sizes[i]) {
            delete []sizes[i];
            sizes[i] = NULL;
        }
    }
   
    template <int step>
    void _add(short *d) {
//        assert(SZ[c-1] > 0);
        const int index = step-1;
        if (is_valid<step>(d)) {
            int k = gen_key<step>(d);
//            if (k >= SZ[index]-1 || in < 0) {
//                REP(j, c) cout << d[j] << " "; cout << endl;
//                cout << "sizes " << (index) << " " << in << endl;
//            }
//            if (step==2 && sizes[index][k] == 0) printf("add %d %d %d \n", step, d[0], d[1]);
            sizes[index][k] ++;
        } else {
            osizes[index] ++;
            if (step > 1) {
                const int nstep = next_step(step);
                REP(j, step/nstep) _add<nstep>(d+j*nstep);
            }
        }
    }

    template <int size, int step>
    void add(short *src) {
        REP(i, size/step) _add<step>(src+i*step);
        src += size / step * step;
        const int left = size % step;
        if (left) {
            add<left, next_step(step)>(src);
        }
    }


    // must call after build
    void write(Bits &bits) {
        REP(ii, TREE_NUM) {
            if (!SZ[ii]) continue;
            bits.write(bps[ii], 8);
            bits.write(opos[ii]&0xffff, 16);
            bits.write(opos[ii] >> 16, 8);
            if (opos[ii] ==0) continue;
            
            int cnt = 0, last=0, md=1;
            REP(j, opos[ii]+1) if (sizes[ii][j]>0) {
                cnt ++;
                if (j-last > md) {
                    md = j-last;
                }
                last = j;
            }
            int cbps = bitCount(md);
            bits.write(cbps, 8);
            bits.write(cnt, 16);
            last=0;
            REP(j, opos[ii]+1) if (sizes[ii][j]>0) {
                bits.write(j-last, cbps);
                bits.write(sizes[ii][j], bps[ii]);
                last = j;
            }
        }
    }

    void read(Bits &bits) {
        REP(ii, TREE_NUM) {
            if (!SZ[ii]) continue;
            memset(sizes[ii], 0, sizeof(int)*SZ[ii]);
            bps[ii] = bits.read(8);
            opos[ii] = bits.read(16);
            opos[ii] += bits.read(8) << 16;
            if (opos[ii] ==0) continue;
            
            int cbps = bits.read(8);
            int cnt = bits.read(16);
            int last = 0;
            REP(j, cnt) {
                int df = bits.read(cbps);
                sizes[ii][last+df] = bits.read(bps[ii]);
                last += df;
            }
        }
    }

    void prebuild() {
        for(int ii=TREE_NUM-1; ii>=0; ii--) {
            if (SZ[ii]==0 && sizes[ii] != 0) {
                cout << ii << " invalid poiter " << sizes[ii]<< endl;
                continue;
            }
            if (sizes[ii] == NULL || SZ[ii]==0) continue;

            int *Size = &sizes[ii][0];
            int n = osizes[ii];
            REP(j, SZ[ii]) n += Size[j];
            int limit = n / MAX_SLOTS;
//            cout << ii << " " << n << " limit " << limit << endl;

            REP(j, SZ[ii]) if (Size[j] > 0 && Size[j] < limit) {
                osizes[ii] += Size[j];
                if (ii > 0) {
                    int step = next_step(ii+1);
                    short v[8];
                    switch (ii+1) {
                    case 8: decode_key<8>(v, j); break;
                    case 6: decode_key<6>(v, j); break;
                    case 4: decode_key<4>(v, j); break;
                    case 2: decode_key<2>(v, j); break;
                    }
                    //cout << "down to "<< step << " "; REP(i, ii+1) cout << v[i] << " " ; cout << endl;
                    REP(i, (ii+1)/step) {
                        switch (step) {
                        case 4: sizes[step-1][gen_key<4>(v+i*step)] += Size[j]; break;
                        case 2: sizes[step-1][gen_key<2>(v+i*step)] += Size[j]; break; 
                        case 1: sizes[step-1][gen_key<1>(v+i*step)] += Size[j]; break;
                        }
                    }
                }
                Size[j]=0;
            }
            int sizeNo = SZ[ii]-1;
//            cout << "over " << ii << " " << Size[sizeNo] << " " << SZ[ii] << endl;
            assert(Size[sizeNo] == 0);
            while (sizeNo && Size[sizeNo-1] ==0) sizeNo --;

            sizes[ii][sizeNo] = osizes[ii];
            opos[ii] = sizeNo++;
            
            if (sizeNo == 1) continue;
 //           cout << ii << " have " << sizeNo << endl;
//            REP(k, sizeNo) cout << Size[k] << " "; cout << endl;

            int maxSize = 0;
            REP(j, sizeNo) if (Size[j]) {
                Size[j] = encodeSize(Size[j]);
                if (Size[j] > maxSize) maxSize = Size[j];
            }
            bps[ii] = bitCount(maxSize);
        }
    }

    void buildTree() {
        HTCodesPos = 0;
        for(int ii=TREE_NUM-1; ii>=0; ii--) {
            if (sizes[ii] == NULL) continue;
            int sizeNo = opos[ii]+1;
            if (sizeNo == 1) continue;
            if (sizeNo > MAX_TREE_SIZE) {
                cout << "too many lines" << sizeNo << endl;
            }
            //cout << "sizeNo" << sizeNo << " " << SZ[ii] << endl;
            
            int *Size = &sizes[ii][0];
            priority_queue <PII> pq;
            REP(j, sizeNo) if (Size[j]) pq.push(MP(-decodeSize(Size[j]), j));

//            cout << "size " << ii << " " << pq.size() << endl;
            if (pq.size() == 1) {
                HTTree[ii] = &HTCodes[HTCodesPos];
                HTTree[ii][pq.top().second] = 1 << 26;
                int v = (1<<26) + pq.top().second; 
                REP(j, 1<<LUT_SIZE) HTData[ii][j] = v; 
                HTCodesPos += sizeNo;
                continue ;
            }
            
            int orderNo = 0;
            int pos = MAX_TREE_SIZE;
            memset(order, 0, sizeof(order));
            memset(parent, 0, sizeof(parent));
            while (pq.size() > 1) {
                PII p1 = pq.top(); pq.pop();
                PII p2 = pq.top(); pq.pop();

                parent[orderNo>>1] = pos;
                order[orderNo++] = p1.second;
                order[orderNo++] = p2.second;

                pq.push(MP(p1.first + p2.first, pos++));
            }

            HTTree[ii] = &HTCodes[HTCodesPos];
            HTCodesPos += sizeNo;
            int *tree = HTTree[ii];
            memset(tree, 0, sizeof(int)*SZ[ii]);
            tree[pos-1] = 0;

            orderNo -= 2;
            for (; orderNo>=0; orderNo -=2) {
                int p = parent[orderNo >> 1];
                int x1 = order[orderNo];
                int x2 = order[orderNo+1];
                int width = tree[p] >> 26;
                int base  = tree[p] & 0x3ffffff;
                if (width+1 > LUT_SIZE) {
                    cout << "width over flow " << (width+1) << " " << endl;
                    if (tree[opos[ii]] == 0) {
                        tree[opos[ii]] = tree[p];
                    }
                    break;
                }
                tree[x1] = ((width+1) << 26) + (base << 1);
                tree[x2] = ((width+1) << 26) + (base << 1) + 1;
            }
//            cout << ii << " " << tree[opos[ii]] << " " << Size[opos[ii]] << endl;
//            cout << "encoding :" << ii << endl;
            REP(j, sizeNo) {
//                if (tree[j] > 0) cout << (j) << " " << Size[j] << " " << (tree[j]>>26) << " " << ((tree[j] & 0x3ffffff)) << endl;
            }

            memset(HTData[ii], 0, sizeof(HTData[ii]));
            REP(j, sizeNo) {
                int v = tree[j];
                if (v==0) continue;
                int width = v >> 26;
                assert(width <= LUT_SIZE);
                int base = (v << (LUT_SIZE-width)) & 0x3ffffff;
                int vv = (width << 26) + j;
//                cout << j << " base " << base << " - " << (base + (1<<(LUT_SIZE-width))) << endl;
                REP(k, 1<<(LUT_SIZE-width)) {
                    assert(HTData[ii][base+k] ==0);
                    HTData[ii][base+k] = vv;
                }
            }
            REP(j, 1<<LUT_SIZE) {
                if (HTData[ii][j] == 0) {
                    cout << j << " shoud not 0" << endl;
                }
//                assert(HTData[ii][j]>0);
            }
        }
    }
  
    template <int size>
    inline bool is_valid(short *d) {
        if (size == 1) {
            return -NUM_LIMIT <= *d && *d <= NUM_LIMIT; 
        }
        if (size == 2) {
            return -NUM_LIMIT2 <= d[0] && d[0] <= NUM_LIMIT2 && -NUM_LIMIT2 <= d[1] && d[1] <= NUM_LIMIT2;
        }
        for(int i=0; i<size-1; i+= 2) {
            if (d[i] > 1 || d[i] < -1) return false;
            if (d[i+1] > 1 || d[i+1] < -1) return false;
        }
        return true;
    }

    template <int size>
    inline int gen_key(short *d) {
        if (size == 1) return *d + NUM_LIMIT;
        if (size == 2) return (d[0] + NUM_LIMIT2) * NUM_SIZE2 + d[1] + NUM_LIMIT2;
        int in = 0;
        REP(i, size) { in *= 3; in += d[i]+1;}
        assert(in >= 0 && in < SZ[size-1]-1);
        return in;
    }

    template <int size>
    inline void decode_key(short *dst, int v) {
        if (size == 1) {
            *dst = v - NUM_LIMIT;
        } else if (size==2) {
            dst[0] = v / NUM_SIZE2 - NUM_LIMIT2;
            dst[1] = v % NUM_SIZE2 - NUM_LIMIT2;
        } else {
            REP(j, size) {
                dst[size-j-1] =  v % 3 -1;
                v /= 3;
            }
        }
    }

    template <int size>
    int _calc(short *src) {
        if (is_valid<size>(src)) {
            int idx = gen_key<size>(src);
            if (idx < opos[size-1] && HTTree[size-1][idx] > 0) {
                return HTTree[size-1][idx] >> 26;
            }
        }

        int idx = opos[size-1];
        assert(HTTree[size-1][idx] > 0);
        int bits = HTTree[size-1][idx] >> 26;

        if (size == 1) {
            bits += 1 + 14;
        } else {
            const int step = next_step(size);
            REP(i, size/step) bits += _calc<step>(src+i*step);
        }
        return bits;
    }

    template <int size, int step>
    int calc(short *src) {
        int bits = 0;
        REP(i, size/step) bits += _calc<step>(src+i*step);
        src += size / step * step;
        const int left = size % step;
        if (left) {
            bits += calc<left, next_step(step)>(src);
        }
        return bits;
    }

    template <int size>
    void _encode(short *src, Bits &bits) {
        int MASK = (1<<26) -1;
        if (is_valid<size>(src)) {
            int idx = gen_key<size>(src);
            if (idx < opos[size-1] && HTTree[size-1][idx] > 0) {
//                cout << size << "  _encode " << (HTTree[size-1][idx] >> 26) << " " << idx << 
  //                  " " << (HTTree[size-1][idx] & MASK) << endl;
                bits.write(HTTree[size-1][idx] & MASK, HTTree[size-1][idx] >> 26);
                return;
            }
        }
        
        int idx = opos[size-1];
//        cout << "other " << idx << endl;
        if (HTTree[size-1][idx] ==0) {
            printf("wrong %d %d %d\n", size, src[0], src[1]);
        }
        assert(HTTree[size-1][idx] > 0);
        bits.write(HTTree[size-1][idx] & MASK, HTTree[size-1][idx] >> 26);

        if (size == 1) {
            bits.write(*src > 0 ? 1 : 0, 1);
            bits.write(abs(*src), NUM_BITS); 
        } else {
            const int step = next_step(size);
            REP(i, size/step) _encode<step>(src+i*step, bits);
        }
    }
   
    template <int size, int step>
    void encode(short *src, Bits &bits) {
        REP(i, size/step) _encode<step>(src+i*step, bits);
        src += size / step * step;
        const int left = size % step;
        if (left) {
            encode<left, next_step(step)>(src, bits);
        }
    }

    template <int size>
    void _decode(short *dst, Bits &bits) {
//        assert(size >0);
  //      if (!size) return;

        int MASK = (1<<26) -1;
        int d = bits.peek(LUT_SIZE);
        int v = HTData[size-1][d];
        assert(v > 0);
        int width = v >> 26;
        v &= MASK;
//        cout << size << " _decode " << width << " " << v << " " << d << endl;
        
        bits.shift(width);

        if (v != opos[size-1]) {
            decode_key<size>(dst, v);
        } else {
            if (size == 1) {
                int flag = bits.read_bit();
                int d = bits.read(NUM_BITS);
                *dst = flag ? d : -d;
            } else {
                const int step = next_step(size);
                REP(i, size/step) _decode<step>(dst+i*step, bits);
            }
        }
    }
    
    template <int size, int step>
    void decode(short *dst, Bits &bits) {
        REP(i, size/step) _decode<step>(dst+i*step, bits);
        dst += size / step * step;
        const int left = size % step;
        if (left) {
            decode<left, next_step(step)>(dst, bits);
        }
    }
};

void test_huffman() {
    const int STEP = 2;
    Huffman *huffman = new Huffman;
    short num[30] = {0, 0, -1, -3, 0, 0, 0, 0, 1, 0, 0, 0, -2, -1, 0, 2, 0, 0, 0, 0, 0, 0, 1, 30, 0};
    huffman->add<30, STEP>(num);
    huffman->prebuild();
    huffman->buildTree();
    
    short buf[3000];
    memset(buf, 0, sizeof(buf));
    Bits bits(buf);
    huffman->write(bits);
    huffman->encode<30, STEP>(num, bits);
    bits.flush();
//    REP(i, 300) cout << buf[i] << " "; cout << endl;
    delete huffman;

    short out[30];
    Bits bits2(buf);
    Huffman *huffman2 = new Huffman;
    huffman2->read(bits2);
    huffman2->buildTree();
    huffman2->decode<30, STEP>(out, bits2);
    REP(i, 30) if (out[i] != num[i]) cout << "fail " << i << " " << num[i] << " != " << out[i] << endl;
    delete huffman2;
}


class DATCompression2 {
public:
    inline int calc_diff(short v, short ov) {
        if (v < 0) v=0; if (v>16383) v=16383;
        int df = (v - ov);
        return df * df;
    }

    template <int L, int SCALE>
    int try_compress(short *src, int size, int *avg, short *dst, int step) {
        int64_t vdiff = 0;
        int zero = 0;
        int half = SCALE/2;
        short *old_src = src, *old_dst = dst;
        size /= step;
        REP(x, size) {
            short *tmp = dst;
            REP(j, L) tmp[j] = (src[j] - avg[j] + (src[j] > avg[j] ? half : -half)) / SCALE;
            
            // smooth
            vdiff += calc_diff(tmp[0]*SCALE + avg[0], src[0]);
            short prev = 0;
            if (vdiff < 36*x*L-2000) {
                for(int j=L-1;j>0;j--) {
                    short d = tmp[j] - tmp[j-1];
                    if (d * prev < 0) {
                        if (d < 0) {
                            d ++;
                            dst[j+1] --;
                        } else {
                            d --;
                            dst[j+1] ++;
                        }
                        if (dst[j+1]==0) zero ++;
                    }
                    if (d==0) zero ++;
                    vdiff += calc_diff((tmp[j-1]+d) * SCALE + avg[j], src[j]);
                    dst[j] = d;
                    prev = d;
                }
            } else {
                for(int j=L-1;j>0;j--) {
                    short d = tmp[j] - tmp[j-1];
                    if (d != 0 && (j&1) && d+prev==0 && d * prev==-1) {
                        d = 0;
                        dst[j+1] = 0;
                        zero ++;
                    }
                    if (d==0) zero ++;
                    vdiff += calc_diff((tmp[j-1]+d) * SCALE + avg[j], src[j]);
                    dst[j] = d;
                    prev = d;
                }
            }
            
            src += L*step;
            dst += L;
        }

        int limit = 36 * size  * L;
        if (vdiff >= limit) zero = 0;

        int mdf = SCALE * SCALE;
        src = old_src;
        dst = old_dst;
        int ones = 0;
        REP(x, size) {
            int last=dst[0];
            for(int i=1; i<L-1; i+=2) {
                int s = dst[i]+dst[i+1];
                if (dst[i]*dst[i+1]==0 && abs(s) == 1 && dst[i+1]==0) {
                    int df = calc_diff(last*SCALE+avg[i], src[i]);
                    if (df < mdf) {
                        int odf = calc_diff((last+dst[i])*SCALE + avg[i], src[i]);
                        if (vdiff + df - odf < limit) {
                            dst[i+1] = dst[i];
                            dst[i] = 0;
                            vdiff += df - odf;
                            ones ++;
                        }
                    }
                }
                last += s;
            }
            if (vdiff > limit - mdf/2) break;
            src += L*step;
            dst += L;
        }
        zero += ones / 4;
        return zero;
    }

    VI compress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
//        cout << X << Y << L << " " << SZ(dat) << endl;
        src += 3;

        myvector myoutput(SZ(dat)*2+1000);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;

        switch (L) {
        case 55: dst += doCompress<55>(dst, src, X, Y); break;
        case 56: dst += doCompress<56>(dst, src, X, Y); break;
        case 57: dst += doCompress<57>(dst, src, X, Y); break;
        case 58: dst += doCompress<58>(dst, src, X, Y); break;
        case 59: dst += doCompress<59>(dst, src, X, Y); break;
        case 60: dst += doCompress<60>(dst, src, X, Y); break;
        }

#ifdef KEEP_HALF
        myoutput.myresize((dst - (short*)&myoutput[0]+1));
#else
        myoutput.myresize((dst - (short*)&myoutput[0]+1)/2);
#endif
        VI output;
        output.swap(myoutput);
        return output;
    }

    template <int L>
    int doCompress(short *dst, short *src, int X, int Y) {
        int XBLOCKS = 9;
        int YBLOCKS = 9;
        while (X % XBLOCKS != 0) XBLOCKS --;
        while (Y % YBLOCKS != 0) YBLOCKS --;
//        cout << "BLOCKS " << XBLOCKS << " " << YBLOCKS << endl;    

        short *start=dst;
        *dst++ = XBLOCKS;
        *dst++ = YBLOCKS;

        int NX = X / XBLOCKS, NY = Y / YBLOCKS;
        short *buf = new short[NX*NY*L];
        REP(x, XBLOCKS) {
            REP(y, YBLOCKS) {
                REP(i, NX) {
                    memcpy(buf+i*NY*L, src+((x*NX+i)*Y+y*NY)*L, sizeof(short)*NY*L);
                }
                dst += compress_block<L>(buf, NX*NY, dst);
            }
        }
        delete []buf;
        return dst - start;
    }

    template <int L>
    int call_try_compress(short *src, int N, int *avg, short *buf, int scale, int step) {
        int z = 0;
        switch (scale) {
        case 20: z = try_compress<L, 20>(src, N, avg, buf, step); break;
        case 19: z = try_compress<L, 19>(src, N, avg, buf, step); break;
        case 18: z = try_compress<L, 18>(src, N, avg, buf, step); break;
        case 17: z = try_compress<L, 17>(src, N, avg, buf, step); break;
        case 16: z = try_compress<L, 16>(src, N, avg, buf, step); break;
        case 15: z = try_compress<L, 15>(src, N, avg, buf, step); break;
        case 14: z = try_compress<L, 14>(src, N, avg, buf, step); break;
        case 13: z = try_compress<L, 13>(src, N, avg, buf, step); break;
        }
        return z;
    }

    template <int L>
    int compress_block(short *src, int N, short *dst) {
        int avg[60] = {0}, c=N;
        if (c>40000) c = 40000;
        int step = N/c;
        REP(i, L) avg[i] = 0;
        REP(i, c) {
            REP(j, L) avg[j] = avg[j] + src[i*step*L+j];
        }
        REP(i, L) avg[i] = (avg[i]) / c;
       
        int zero = 0, scale = 0;
        short *best=NULL, *buf=NULL;

        for(int ii=17; ii>12 && ii<21; ii++) {
            if (buf == NULL) buf = new short[N*L];
            int z = call_try_compress<L>(src, N, avg, buf, ii, TRY_STEP);
            //cout << "try " << ii << " " << (float(z)/X/Y/L) << endl; 
            if (z == 0) {
                if (ii > 18) break;
                // scan down
                if (ii == 18) ii -= 3;
                else ii -= 2;
            } else if (z > zero) {
                zero = z;
                scale = ii;
                short *p = best;
                best = buf;
                buf = p;
                if (ii < 17) ii -= 2; // scan down
            } else if (z <= zero) {
                break;
            }
        }
        if (buf != NULL) delete []buf;

        if (TRY_STEP > 1) {
            zero = call_try_compress<L>(src, N, avg, best, scale, 1);
            if (zero == 0) {
                scale --;
                cout << "try " << scale << endl;
                zero = call_try_compress<L>(src, N, avg, best, scale, 1);
            }
        }

        // output
        short *start = dst;
        REP(i, L) dst[i] = avg[i];
        dst += L;
        *dst++ = scale;
        int *length = (int*)dst;
        dst += 2;

        struct Bits bits(dst);
        REP(x, N) {
            bits.write(best[x*L]&0x3f, 6);
            best[x*L] >>= 6;
        }

        static struct ArCoder *coder = NULL;
        if (coder == NULL) {
            coder = new ArCoder;
//            cout << "sizeof " << (sizeof(coder) >> 10) << endl;
            REP(x, N) coder->learn(best+x*L, L);
            coder->save(bits);
        }
        coder->train();
        
        coder->init_encoder();
        REP(x, N) coder->encode(best+x*L, L, bits);
        coder->flush(bits);
        bits.flush();
        *length = bits.p - start;
        
        /*
        static struct ArCoder *coder2 = NULL;
        struct Bits r(dst);
        REP(x, N) {
            best[x*L] <<= 6;
            best[x*L] += r.read(6);
        }
        if (coder2 == NULL) {
            coder2 = new ArCoder;
            coder2->load(r);
        }
        coder2->train();
        coder2->init_decode(r);
        short tmp[60];
        REP(x, N) {
            coder2->decode(tmp, L, r);
            FOR(i, 1, L) {
                if (tmp[i] != best[i+x*L]) {
                    printf("fail %d %d %d!=%d\n", x, i, best[i+x*L], tmp[i]);
                    x = N;
                    break;
                }
            }
        }*/

        delete []best;
//        cout << "enc block " << *length << endl;
        return *length; // STEP
    }

    VI decompress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
        src += 3;

        myvector myoutput((X*Y*L+4)/2);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;

        switch (L) {
//        case 55: doDecompress<55>(dst, src, X, Y); break;
//        case 56: doDecompress<56>(dst, src, X, Y); break;
        case 57: doDecompress<57>(dst, src, X, Y); break;
//        case 58: doDecompress<58>(dst, src, X, Y); break;
        case 59: doDecompress<59>(dst, src, X, Y); break;
//        case 60: doDecompress<60>(dst, src, X, Y); break;
        }

        VI output;
        output.swap(myoutput);
        return output;
    }
   
    template <int L>
    void doDecompress(short *dst, short *src, int X, int Y) {
        int XBLOCKS = *src++;
        int YBLOCKS = *src++;
//        cout << " dec BLOCKS " << XBLOCKS << " " << YBLOCKS << endl;    
  //      cout << "block " << BLOCKS << endl;

        int NX = X/XBLOCKS, NY= Y/YBLOCKS;
        REP(x, XBLOCKS) {
        REP(y, YBLOCKS) {
            src += decompress_block<L>(src, dst, x, y, NX, NY, Y);
        }
        }
    }

    template <int L>
    int decompress_block(short *src, short *dst, int x, int y, int NX, int NY, int Y) {
        short* avg = src;
//          cout << endl;REP(i, L) cout << avg[i] << " "; cout << endl;
        src += L;
        int SCALE = *src++;
        int length = *(int*)src;
        src += 2;;

        struct Bits r(src);
        REP(x1, NX) {
            short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
            REP(y1, NY) {
                buf[y1*L] = r.read(6);
            }
        }
        static struct ArCoder *coder = NULL;
        if (coder == NULL) {
            coder = new ArCoder;
            coder->load(r);
        }
        coder->train();
        
        coder->init_decode(r);
        short tmp[60];
        REP(x1, NX) {
            short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
            REP(y1, NY) {
                coder->decode(tmp, L, r);
                tmp[0] <<= 6;
                tmp[0] += buf[0];
                
                int last = 0;
                REP(j, L) {
                    last += tmp[j];
                    buf[j] = avg[j] + last * SCALE;
                    if(buf[j] < 0) buf[j] = 0;
                    if(buf[j] > 16383) buf[j] = 16383;
                }
                buf += L;
            }
        }
        r.rollback();
//        cout << "block size " << (length) << endl;
        return length;
    }
};
