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

#ifdef USE_EXPECT
#define likely(x)    __builtin_expect((x), 1)
#define unlikely(x) __builtin_expect((x), 0)
#else
#define likely(x)    (x)
#define unlikely(x) (x)
#endif

struct Symbol {
    int code, len;
};

Symbol enc_table[4] = {
    {0x2, 2}, // -1
    {0x0, 1}, // 0
    {0x6, 3}, // 1
    {0x7, 3}, // xx
};

Symbol dec_table[8] = {
    {0, 1}, {0, 1},{0, 1},{0, 1},  // 0
    {-1, 2}, {-1, 2}, // -1
    {1, 3},  // 1
    {2, 4},  // xxx
};


Symbol enc_table2[9] = {
    {0xf, 4}, {0x5, 3}, {0x0, 1}, // -1
    {0x4, 3}, {0x0, 1}, {0x6, 3}, // 0
    {0x0, 1}, {0xe, 4}, {0xf, 4}, // 1
};

Symbol dec_table2[16] = {
    {0x11, 1},{0x11, 1},{0x11, 1}, {0x11, 1}, {0x11, 1}, {0x11, 1}, {0x11, 1}, {0x11, 1},
    {0x10, 3},{0x10, 3},{0x01, 3}, {0x01, 3}, {0x12, 3}, {0x12, 3}, {0x21, 4}, {0x00, 6},
};

Symbol enc_table4[8] = {
//    {0x0, 1},
    {0x8, 4}, {0x9, 4}, {0xa, 4}, {0xb, 4},
    {0x18, 5},{0x19,5}, {0x1a,5}, {0x1b,5},
};

Symbol dec_table4[32] = {
    {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, 
    {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, {0x1111, 1}, 
    {0x0111, 4}, {0x0111, 4}, {0x1011, 4}, {0x1011, 4}, {0x1101, 4}, {0x1101, 4}, {0x1110, 4}, {0x1110, 4}, 
    {0x2111, 5}, {0x1211, 5}, {0x1121, 5}, {0x1112, 5}, {0x1111, 3}, {0x1101, 3}, {0x1110, 3}, {0x1110, 3}, 
};

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

#define PII pair<int, int>
#define MP(x,y) make_pair<int,int>(x,y)

const int NUM_LIMIT = 10;
const int TREE_NUM = 12;
const int MAX_SLOTS = 1<<12;
const int MAX_TREE_SIZE = 81*81*81+1;
const int SZ[] = {NUM_LIMIT*2+2, 3*3+1, 0, 9*9+1, 0, 27*27+1, 0, 81*81+1, 0, 0, 0, 81*81*81+1};
const int LUT_SIZE=16;
int order[MAX_TREE_SIZE*2];
int parent[MAX_TREE_SIZE];
    
int HTCodes[81*81*81*2];
int HTCodesPos;
int *HTTree[TREE_NUM];
int HTData[TREE_NUM][1<<LUT_SIZE];

struct Huffman {
    int *sizes[TREE_NUM];
    int osizes[TREE_NUM];
    int bps[TREE_NUM];
    int opos[TREE_NUM];

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
    
    inline int next_step(int c) {
        return c % 3==0 ? c/3 : (c % 2==0 ? c/2 : 1);
    }

    void _add(short *d, int c) {
        assert(c == 1 || c==2 || c==4 || c==6 || c==8 || c==12);
        assert(SZ[c-1] > 0);
        int index = c-1;
        if (is_valid(d, c)) {
            int k = gen_key(d, c);
//            if (k >= SZ[index]-1 || in < 0) {
//                REP(j, c) cout << d[j] << " "; cout << endl;
//                cout << "sizes " << (index) << " " << in << endl;
//            }
//            if (sizes[index][k] == 0) cout << "_add " << *d << " " << c << endl;
            sizes[index][k] ++;
        } else {
            osizes[index] ++;
            if (c > 1) {
                int step = next_step(c);
                REP(j, c/step) _add(d+j*step, step);
            }
        }
    }

    void add(short *src, int size, int step) {
        REP(i, size/step) _add(src+i*step, step);
        src += size / step * step;
        int left = size % step;
        if (left) {
            add(src, left, next_step(step));
        }
    }

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
        return (4 + (x & 3) ) << ((x >>2) -1);
    }

    // must call after build
    void write(Bits &bits) {
        REP(ii, 12) {
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
        REP(ii, 12) {
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
        for(int ii=11; ii>=0; ii--) {
            if (SZ[ii]==0 && sizes[ii] != 0) {
                cout << ii << " invalid poiter " << sizes[ii]<< endl;
                continue;
            }
            if (SZ[ii]==0) assert(sizes[ii] == NULL);
            if (sizes[ii] == NULL || SZ[ii]==0) continue;

            int *Size = &sizes[ii][0];
            int n = osizes[ii];
            REP(j, SZ[ii]) n += Size[j];
            int limit = n / MAX_SLOTS;
//            cout << ii << " " << n << " limit " << limit << endl;

            REP(j, SZ[ii]) if (Size[j] > 0 && Size[j] < limit) {
                osizes[ii] += Size[j];
                /*if (ii > 0) {
                    int step = next_step(ii+1);
                    int base = 1, k=j;
                    REP(i, step) base *= 3;
                    REP(i, (ii+1) / step) {
                        sizes[step-1][k % base] += Size[j];
                        k /= base;
                    }
                }*/
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
        for(int ii=11; ii>=0; ii--) {
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
                    cout << "width over flow " << (width+1) << " " << Size[p] << endl;
                    break;
                }
                assert(width+1 <= LUT_SIZE);
                tree[x1] = ((width+1) << 26) + (base << 1);
                tree[x2] = ((width+1) << 26) + (base << 1) + 1;
            }

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
                if (HTData[j] == 0) {
                    cout << j << "shoud not 0" << endl;
                }
                assert(HTData[j]>0);
            }
        }
    }
  
    inline bool is_valid(short *d, int size) {
        if (size == 1) {
            return -NUM_LIMIT <= *d && *d <= NUM_LIMIT; 
        }
        for(int i=0; i<size-1; i+= 2) {
            if (d[i] > 1 || d[i] < -1) return false;
            if (d[i+1] > 1 || d[i+1] < -1) return false;
        }
        return true;
    }

    inline int gen_key(short *d, int size) {
        if (size == 1) return *d + NUM_LIMIT;
        int in = 0;
        REP(i, size) { in *= 3; in += d[i]+1;}
        assert(in >= 0 && in < SZ[size-1]-1);
        return in;
    }

    inline void decode_key(short *dst, int size, int v) {
        if (size == 1) {
            *dst = v - NUM_LIMIT;
        } else {
            REP(j, size) {
                dst[size-j-1] =  v % 3 -1;
                v /= 3;
            }
        }
    }

    void _encode(short *src, int size, Bits &bits) {
        int MASK = (1<<26) -1;
        if (is_valid(src, size)) {
            int idx = gen_key(src, size);
            if (idx < opos[size-1] && HTTree[size-1][idx] > 0) {
//                cout << size << "  _encode " << (HTTree[size-1][idx] >> 26) << " " << idx << 
  //                  " " << (HTTree[size-1][idx] & MASK) << endl;
                bits.write(HTTree[size-1][idx] & MASK, HTTree[size-1][idx] >> 26);
                return;
            }
        }
        
        int idx = opos[size-1];
//        cout << "other " << idx << endl;
        assert(HTTree[size-1][idx] > 0);
        bits.write(HTTree[size-1][idx] & MASK, HTTree[size-1][idx] >> 26);

        if (size == 1) {
            bits.write(*src > 0 ? 1 : 0, 1);
            bits.write(abs(*src), 14); 
        } else {
            int step = next_step(size);
            REP(i, size/step) _encode(src+i*step, step, bits);
        }
    }
    
    void encode(short *src, int size, int step, Bits &bits) {
        REP(i, size/step) _encode(src+i*step, step, bits);
        src += size / step * step;
        int left = size % step;
        if (left) {
            encode(src, left, next_step(step), bits);
        }
    }

    void _decode(short *dst, int size, Bits &bits) {
        assert(size >0);
        if (!size) return;

        int MASK = (1<<26) -1;
        int d = bits.peek(LUT_SIZE);
        int v = HTData[size-1][d];
        assert(v > 0);
        int width = v >> 26;
        v &= MASK;
//        cout << size << " _decode " << width << " " << v << " " << d << endl;
        
        bits.shift(width);

        if (v != opos[size-1]) {
            decode_key(dst, size, v);
        } else {
            if (size == 1) {
                int flag = bits.read_bit();
                int d = bits.read(14);
                *dst = flag ? d : -d;
            } else {
                int step = next_step(size);
                REP(i, size/step) _decode(dst+i*step, step, bits);
            }
        }
    }
    
    void decode(short *dst, int size, int step, Bits &bits) {
        REP(i, size/step) _decode(dst+i*step, step, bits);
        dst += size / step * step;
        int left = size % step;
        if (left) {
            decode(dst, left, next_step(step), bits);
        }
    }
};

void test_huffman() {
    const int STEP = 6;
    Huffman *huffman = new Huffman;
    short num[30] = {0, 0, -1, -3, 0, 0, 0, 0, 1, 0, 0, 0, -2, -1, 0, 2, 0, 0, 0, 0, 0, 0, 1, 30, 0};
    huffman->add(num, 30, STEP);
    huffman->prebuild();
    huffman->buildTree();
    
    short buf[3000];
    memset(buf, 0, sizeof(buf));
    Bits bits(buf);
    huffman->write(bits);
    huffman->encode(num, 30, STEP, bits);
    bits.flush();
//    REP(i, 300) cout << buf[i] << " "; cout << endl;
    delete huffman;

    short out[30];
    Bits bits2(buf);
    Huffman *huffman2 = new Huffman;
    huffman2->read(bits2);
    huffman2->buildTree();
    huffman2->decode(out, 30, STEP, bits2);
    REP(i, 30) if (out[i] != num[i]) cout << "fail " << i << " " << num[i] << " != " << out[i] << endl;
    delete huffman2;
}

const int MAX_BITS = 14;

class DATCompression2 {
public:
    inline void write_byte(int d, Bits &bits, int base) {
        bits.write(d > 0 ? 1 : 0, 1);
        d = abs(d) - base;
        int c = 0;
        while (d > 0 && c < MAX_BITS) {
            bits.write(1, 1);
            d --;
            c ++;
        }
        if (c == MAX_BITS) bits.write(d, 14);
        bits.write(0, 1);
    }

    inline int read_byte(Bits &bits, int base) {
        int flag = bits.read_bit();
        int r = base;
        int n = bits.peek(1);
        int c = 0;
        while (n && c < MAX_BITS) {
            bits.shift(1);
            r ++;
            c ++;
            n = bits.peek(1);
        }
        if (c == MAX_BITS) {
            r += bits.peek(14);
            bits.shift(14);
        }
        bits.shift(1);
        return flag ? r : -r;
    }
    
    inline void encode_byte(int d, Bits &bits) {
        if (d >= -1 && d <= 1) {
            bits.write(enc_table[d+1].code, enc_table[d+1].len);
        } else {
            bits.write(enc_table[3].code, enc_table[3].len);
            write_byte(d, bits, 2);
        }
    }

    inline int decode_byte(Bits &bits) {
        int d = bits.peek(3);
        if (dec_table[d].len < 4) {
            bits.shift(dec_table[d].len);
            return dec_table[d].code;
        } else{
            bits.shift(3);
            return read_byte(bits, 2);
        }
    }

    inline void encode_two_byte(int d1, int d2, Bits &bits) {
        if (d1 < -1 || d1 > 1 || d2 < -1 || d2 > 1
            || (d1==-1 && d2==-1) || (d1==1 && d2==1)) {
            bits.write(0xf, 4);
            write_byte(d1, bits, 0);
            write_byte(d2, bits, 0);
        } else {
            int index = (d1+1)*3 + d2+1;
            bits.write(enc_table2[index].code, enc_table2[index].len);
        }
    }

    inline void decode_two_byte(int &d1, int &d2, Bits &bits) {
        int d = bits.peek(4);
        if (dec_table2[d].len <= 4) {
            bits.shift(dec_table2[d].len);
            d1 = (dec_table2[d].code >> 4) - 1;
            d2 = (dec_table2[d].code & 0xf) - 1;
        } else {
            bits.shift(4);
            d1 = read_byte(bits, 0);
            d2 = read_byte(bits, 0);
        }
    }

    inline void encode_byte4(int d1, int d2, int d3, int d4, Bits &bits) {
        // four 0
        int ord = d1 | d2 | d3 | d4;
        if (ord == 0) {
            bits.write(0, 1);
            return; 
        }
        // one -1 or 1
        int s = d1 + d2 + d3 + d4;
        if ((ord == 1 && s == 1) || (s == -1 && ((d1+1)|(d2+1)|(d3+1)|(d4+1)) == 1)) {
            int index = int((s+1)/2) * 4 + abs(d2) + abs(d3)*2 + abs(d4)*3;
//            cout << d1 << d2 << d3 << d4 << " " << index << endl;
            bits.write(enc_table4[index].code, enc_table4[index].len);
        }else{
            // all others, fallback
            bits.write(7, 3);
            encode_two_byte(d1, d2, bits);
            encode_two_byte(d3, d4, bits);
        }
    }

    inline void decode_byte4(int &d1, int &d2, int &d3, int &d4, Bits &bits) {
        int d = bits.peek(5);
        if (dec_table4[d].len == 1) {
            bits.shift(1);
            d1 = d2 = d3 = d4 = 0;
        } else if (dec_table4[d].len == 3) {
            bits.shift(3);
            decode_two_byte(d1, d2, bits);
            decode_two_byte(d3, d4, bits);
        } else {
            bits.shift(dec_table4[d].len);
            int code = dec_table4[d].code;
            d1 = ((code >> 12) & 0xf) - 1;
            d2 = ((code >> 8) & 0xf) - 1;
            d3 = ((code >> 4) & 0xf) - 1;
            d4 = (code & 0xf) - 1;
        }
    }

    inline int calc_diff(short v, short ov) {
//        if (v < 0) v=0; if (v>16383) v=16383;
        int df = (v - ov);
        return df * df;
    }

    template <int SCALE>
    int try_compress(short *src, int size, int L, int *avg, short *dst, int step) {
        int64_t vdiff = 0;
        int zero = 0;
        int half = SCALE/2;
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
                    if (d != 0 && d+prev==0 && d * prev==-1) {
                        d = 0;
                        dst[j+1] = 0;
                        zero ++;
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
        if (vdiff >= 36 * size  * L) zero = 0;
        return zero;
    }

    VI compress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
//        cout << X << Y << L << " " << SZ(dat) << endl;
        src += 3;

        int XBLOCKS = 9;
        int YBLOCKS = 9;
        while (X % XBLOCKS != 0) XBLOCKS --;
        while (Y % YBLOCKS != 0) YBLOCKS --;

        myvector myoutput(SZ(dat)+1000);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        *dst++ = XBLOCKS;
        *dst++ = YBLOCKS;
        
        int NX = X / XBLOCKS, NY = Y / YBLOCKS;
        short *buf = new short[NX*NY*L];
        REP(x, XBLOCKS) {
            REP(y, YBLOCKS) {
                REP(i, NX) {
                    memcpy(buf+i*NY*L, src+((x*NX+i)*Y+y*NY)*L, sizeof(short)*NY*L);
                }
                dst += compress_block(buf, NX*NY, L, dst);
            }
        }
        delete []buf;

        myoutput.myresize((dst - (short*)&myoutput[0]+1)/2);
        VI output;
        output.swap(myoutput);
        return output;
    }

    int call_try_compress(short *src, int N, int L, int *avg, short *buf, int scale, int step) {
        int z = 0;
        switch (scale) {
        case 20: z = try_compress<20>(src, N, L, avg, buf, step); break;
        case 19: z = try_compress<19>(src, N, L, avg, buf, step); break;
        case 18: z = try_compress<18>(src, N, L, avg, buf, step); break;
        case 17: z = try_compress<17>(src, N, L, avg, buf, step); break;
        case 16: z = try_compress<16>(src, N, L, avg, buf, step); break;
        case 15: z = try_compress<15>(src, N, L, avg, buf, step); break;
        case 14: z = try_compress<14>(src, N, L, avg, buf, step); break;
        case 13: z = try_compress<13>(src, N, L, avg, buf, step); break;
        }
        return z;
    }

    int compress_block(short *src, int N, int L, short *dst) {
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
            int z = call_try_compress(src, N, L, avg, buf, ii, 16);
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

        int z = call_try_compress(src, N, L, avg, best, scale, 1);
        if (z==0) {
            scale --;
            cout << "try " << scale << endl;
            z = call_try_compress(src, N, L, avg, best, scale, 1);
        }

        // output
        short *start = dst;
        REP(i, L) dst[i] = avg[i];
        dst += L;
        
        const int STEP = 8;
        Huffman *hm = new Huffman;
        short *tmp = best;
        REP(x, N) {
            short mod = (tmp[0]+32) % 64, cc = (tmp[0]+32)/64;
            if (mod < 0) {
                cc -= 1;
            }
            hm->add(&cc, 1, 1);
            hm->add(tmp+1, L-1, STEP);
            tmp += L;
        }

        hm->prebuild();
        hm->buildTree();

        *dst++ = scale;
        *dst++ = STEP;
        struct Bits bits(dst);
        hm->write(bits);
        tmp = best;
        REP(x, N) {
            short mod = (tmp[0]+32) % 64, cc = (tmp[0]+32)/64;
            if (mod < 0) {
                mod += 64;
                cc -= 1;
            }
            bits.write(mod, 6);
            hm->encode(&cc, 1, 1, bits);
            hm->encode(tmp+1, L-1, STEP, bits);
            tmp += L;
        }
        bits.flush();
        int length =  bits.p - start;

/*        // test 
        {
        short *t=best;
        struct Bits bits(dst);
        struct Huffman *hm = new Huffman;
        hm->read(bits);
        hm->buildTree();
        short tmp[60];
        REP(x, 0) {
            tmp[0] = bits.read(6) - 32;
            short v = 0;
            hm->decode(&v, 1, 1, bits);
            tmp[0] += v * 64;
            hm->decode(tmp+1, L-1, STEP, bits); 
            int exit = false;
            REP(i, L) if (t[i] != tmp[i]) {
                    cout << x << " " << i << " " << t[i] << " != " << tmp[i] << endl;
                    REP(k, L) cout << t[k] << " " ; cout << endl;
                    REP(k, L) cout << tmp[k] << " "; cout << endl;
                    exit = true;
                    break;
                }
            if (exit) break;
            t += L;
        }
        delete hm;
        }
       */

        delete hm;
        delete []best;
        return length;
    }

    VI decompress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
        src += 3;
        int XBLOCKS = *src++;
        int YBLOCKS = *src++;
        int NX = X/XBLOCKS, NY= Y/YBLOCKS;
  //      cout << "block " << BLOCKS << endl;

        myvector myoutput((X*Y*L+4)/2);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        
        REP(x, XBLOCKS) {
        REP(y, YBLOCKS) {
            short* avg = src;
//          cout << endl;REP(i, L) cout << avg[i] << " "; cout << endl;
            src += L;
            int SCALE= *src++;
            int STEP = *src++;
            struct Bits bits(src);
            struct Huffman *hm = new Huffman;
            hm->read(bits);
            hm->buildTree();

            short tmp[60];
            REP(x1, NX) {
                short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
                REP(y1, NY) {
                    tmp[0] = bits.peek(6) - 32;
                    bits.shift(6);
                    short v = 0;
                    hm->decode(&v, 1, 1, bits);
                    tmp[0] += v * 64;
                    hm->decode(tmp+1, L-1, STEP, bits);
                    
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
            delete hm;
            bits.rollback();
            src = bits.p; // next block
        }
        }

        VI output;
        output.swap(myoutput);
        return output;
    }
};
