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
void test_huffman() {
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
