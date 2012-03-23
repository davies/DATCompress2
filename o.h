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
#include <math.h>

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

const unsigned int MinLength = 0x01000000U;   // threshold for renormalization
const unsigned int MaxLength = 0xFFFFFFFFU;      // maximum AC interval length
const unsigned int LengthShift = 15;     // length bits discarded before mult.
const unsigned int MaxCount    = 1 << LengthShift;  // for adaptive models
const unsigned int LOOKUP_LIMIT = 16;

struct Model {
    unsigned int *distribution, *count, *dtable;
    unsigned total_count, update_cycle, update_left;
    unsigned ns, last_symbol, table_size, table_shift;
    Model():distribution(NULL), count(NULL), dtable(NULL),
            total_count(0),update_cycle(0), update_left(0), 
            ns(0),last_symbol(0), table_size(0), table_shift(0) {}
   
    void init(unsigned int size) {
        last_symbol = size - 1;
        ns = size;
        if (ns > LOOKUP_LIMIT) {
            int table_bits = 3;
            while (ns > (1U << (table_bits + 2))) ++table_bits;
            table_size  = (1 << table_bits) + 4;
            table_shift = LengthShift - table_bits;
            distribution = new unsigned[ns*2 + table_size+6];
            dtable = distribution + ns * 2; 
        } else {
            dtable = NULL;
            table_size = table_shift = 0;
            distribution = new unsigned[ns*2];
        }
        count = distribution + ns;

        total_count = 0;
        update_cycle = ns;
        for (unsigned k = 0; k < ns; k++) count[k] = 1;
        update(false);
        update_left = update_cycle = (ns + 6) >> 1;
    }

    inline void learn(int symbol, bool enc) {
        count[symbol]+=128;
        if (--update_left == 0) update(enc);
    }

    void update(bool enc) {
        if ((total_count += update_cycle*128) > MaxCount) {
            total_count = 0;
            REP(n, ns) total_count += (count[n] = (count[n] + 1) >> 1);
        }
        unsigned sum = 0, s = 0;
        unsigned scale = 0x80000000U / total_count;

        if (enc || (table_size == 0)) {
            REP(k, ns) {
                distribution[k] = (scale * sum) >> (31 - LengthShift);
                sum += count[k];
            }
        } else {
            REP(k, ns) {
                distribution[k] = (scale * sum) >> (31 - LengthShift);
                sum += count[k];
                unsigned w = distribution[k] >> table_shift;
                while (s < w) dtable[++s] = k - 1;
            }
            dtable[0] = 0;
            while (s <= table_size) dtable[++s] = ns - 1;
        }

        update_cycle = (5 * update_cycle) >> 2;
        unsigned max_cycle = (ns + 6) << 2;
        if (update_cycle > max_cycle) update_cycle = max_cycle;
        update_left = update_cycle;
    }
};

const int MAX_NUMBER = 16;
const int SYMBOL_COUNT = MAX_NUMBER * 2+1;
const int MAX_L = 60;
const int WIDTH = 3;
const int GROUPS = 60*WIDTH*WIDTH;

struct ArCoder {
    unsigned char *buffer, *current;
    unsigned int base, value, length;
    Model models[GROUPS];

    ArCoder() {
        REP(i, GROUPS) models[i].init(SYMBOL_COUNT); 
    }
    
    inline void write_bit() {
        unsigned char *p = current -1;
        while (*p == 0xFF) *p--=0;
        ++*p;
    }

    inline void enc_renorm() {
        do {
            *current++ = base >> 24;
            base <<= 8;
        } while ((length <<= 8) < MinLength);
    }

    inline void dec_renorm() {
        do {
            value = (value << 8) | (*++current);
        } while ((length <<= 8) < MinLength);
    }

    inline void put_bits(unsigned int data, int bits) {
        unsigned old = base;
        base += data * (length >>= bits);
        if (old > base) write_bit();
        if (length < MinLength) enc_renorm();
    }

    inline unsigned int get_bits(int bits) {
        unsigned s = value / (length >>= bits);
        value -= length * s;
        if (length < MinLength) dec_renorm();
        return s;
    }

    void init_encoder(short *dst) {
        buffer = (unsigned char*)dst;
        base = 0;
        length = MaxLength;
        current = buffer;
    }

    void encode_byte(unsigned int data, Model &M) {
        unsigned x, old = base;
        if (data == M.last_symbol) {
            x = M.distribution[data] * (length >> LengthShift);
            base   += x;                                           
            length -= x;                                     
        } else {
            x = M.distribution[data] * (length >>= LengthShift);
            base   += x;                                      
            length  = M.distribution[data+1] * length - x;
        }

        if (old > base) write_bit();          
        if (length < MinLength) enc_renorm();    
        M.learn(data, true);
    }

    inline int gen_code(int v) {
        return v < -1 ? 0 : (v > 1 ? 2 : (v + 1));
    }

    inline int gen_index(int i, int code0, int code1) {
        return i*WIDTH*WIDTH + code0*WIDTH + code1;
    }

    void encode_one(int ctx, int v) {
        if (v==0) {
            encode_byte(SYMBOL_COUNT-1, models[ctx]);
            return ;
        }
        while (abs(v) >= MAX_NUMBER) {
            encode_byte(0, models[ctx]);
            put_bits(v & 0xf, 4);
            v >>= 4;
        }
        encode_byte(v + MAX_NUMBER, models[ctx]);
    }

    void encode(short *src, int size) {
        if (size == 1) {
            encode_one(1, src[0]);
            return;
        }
        int code0=0, code1 = 0;
        for(int i=0; i<size-1; i+=2) {
            encode_one(gen_index(i, code0, code1), src[i]);
            code0 = gen_code(src[i]);
            encode_one(gen_index(i+1, code1, code0), src[i+1]);
            code1 = gen_code(src[i+1]);
        }
        if (size%2) {
            encode_one(gen_index(size-1, code0, code1), src[size-1]);
        }
//        cout << "encode " ; REP(i, size) cout << src[i] << " "; cout << endl;
    }

    int flush() {
        unsigned old = base;
        if (length > 2 * MinLength) {
            base  += MinLength;
            length = MinLength >> 1;
        } else {
            base  += MinLength >> 1;
            length = MinLength >> 9;
        }

        if (old > base) write_bit();
        enc_renorm();
        return (current - buffer+1)/2;
    }

    void init_decode(short *dst) {
        buffer = (unsigned char*)dst;
        length = MaxLength;
        value = (unsigned(buffer[0]) << 24)|(unsigned(buffer[1]) << 16) |
          (unsigned(buffer[2]) <<  8)| unsigned(buffer[3]);
        current = buffer + 3;
    }

    int decode_byte(Model &M) {
        unsigned n, s, x, y = length;

        if (M.dtable) {              // use table look-up for faster decoding

            unsigned dv = value / (length >>= LengthShift);
            unsigned t = dv >> M.table_shift;

            s = M.dtable[t];         // initial decision based on table look-up
            n = M.dtable[t+1] + 1;

            while (n > s + 1) {                        // finish with bisection search
                unsigned m = (s + n) >> 1;
                if (M.distribution[m] > dv) n = m; else s = m;
            }
            // compute products
            x = M.distribution[s] * length;
            if (s != M.last_symbol) y = M.distribution[s+1] * length;
        }

        else {                                  // decode using only multiplications

            x = s = 0;
            length >>= LengthShift;
            unsigned m = (n = M.ns) >> 1;
            // decode via bisection search
            do {
                unsigned z = length * M.distribution[m];
                if (z > value) {
                    n = m;
                    y = z;                                             // value is smaller
                }
                else {
                    s = m;
                    x = z;                                     // value is larger or equal
                }
            } while ((m = (s + n) >> 1) != s);
        }

        value -= x;                                               // update interval
        length = y - x;

        if (length < MinLength) dec_renorm();        // renormalization
        
        M.learn(s, false);
        return s;
    }
        
    int decode_one(int ctx) {
        int r = decode_byte(models[ctx]);
        if (r == SYMBOL_COUNT-1) return 0; 
        if (r != 0) {
            return r - MAX_NUMBER;
        }
        int v = 0, c = 0;
        do {
            v |= get_bits(4) << c;
            c += 4;
            r = decode_byte(models[ctx]);
        } while (r == 0);
        v += (r-MAX_NUMBER) << c; 
        return v;
    }

    void decode(short *dst, int size) {
        if (size == 1) {
            dst[0] = decode_one(1);
            return;
        }
        int code0=0, code1 = 0;
        for(int i=0; i<size-1; i+=2) {
            dst[i] = decode_one(gen_index(i, code0, code1));
            code0 = gen_code(dst[i]);
            dst[i+1] = decode_one(gen_index(i+1, code1, code0));
            code1 = gen_code(dst[i+1]);
        }
        if (size % 2) {
            dst[size-1] = decode_one(gen_index(size-1, code0, code1));
        }
    }
};

void test_arcoding() {
    short nums[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 13, 15};
    struct ArCoder *coder = new ArCoder;
//    coder->learn(nums, 15);

    short buf[300];
//    struct Bits w(buf);
//    coder->save(w);
//    coder->train();
    coder->init_encoder(buf);
    coder->encode(nums, 15);
    int r = coder->flush();
    cout << r << endl;
//    w.flush();
//    REP(i, 300) cout << buf[i] << " " ; cout << endl;

    short rbuf[20];
//    struct Bits r(buf);
//    coder->load(r);
//    coder->train();
    coder->init_decode(buf);
    coder->decode(rbuf, 15);
    REP(i, 15) {
        if (nums[i] != rbuf[i]) printf("fail %d %d != %d\n", i, nums[i], rbuf[i]);
    }

    delete coder;
}


const int TRY_STEP = 7;

class DATCompression2 {
public:
    inline int calc_diff(short v, short ov) {
        if (v < 0) v=0; if (v>16383) v=16383;
        int df = (v - ov);
        return df * df;
    }

    int try_compress(short *src, int size, int L, int SCALE, int *avg, short *dst, int step, bool safe) {
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
            } else if (safe) {
                for(int j=L-1;j>0;j--) {
                    short d = tmp[j] - tmp[j-1];
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
        
        dst += doCompress(dst, src, X, Y, L);

#ifdef KEEP_HALF
        myoutput.myresize((dst - (short*)&myoutput[0]+1));
#else
        myoutput.myresize((dst - (short*)&myoutput[0]+1)/2);
#endif
        VI output;
        output.swap(myoutput);
        return output;
    }

    int doCompress(short *dst, short *src, int X, int Y, int L) {
        int XBLOCKS = 10;
        int YBLOCKS = 10;
        while (X % XBLOCKS != 0) XBLOCKS --;
        while (Y % YBLOCKS != 0) YBLOCKS --;
//        cout << "BLOCKS " << XBLOCKS << " " << YBLOCKS << endl;    

        short *start=dst;
        *dst++ = XBLOCKS;
        *dst++ = YBLOCKS;

        int NX = X / XBLOCKS, NY = Y / YBLOCKS;
        short *buf = new short[NX*NY*L];
        short *avg = NULL;
        struct ArCoder *coder = new ArCoder;
        REP(x, XBLOCKS) {
            REP(y, YBLOCKS) {
                REP(i, NX) {
                    memcpy(buf+i*NY*L, src+((x*NX+i)*Y+y*NY)*L, sizeof(short)*NY*L);
                }
                dst += compress_block(buf, NX*NY, L, dst, &avg, coder);
            }
        }
        delete []buf;
        delete []avg;
        delete coder;
        return dst - start;
    }

    int compress_block(short *src, int N, int L, short *dst, short **last_avg, struct ArCoder *coder) {
        int avg[60] = {0}, c=N;
        if (c>40000) c = 40000;
        int step = N/c;
        REP(i, L) avg[i] = 0;
        REP(i, c) {
            REP(j, L) avg[j] = avg[j] + src[i*step*L+j];
        }
        REP(i, L) avg[i] = (avg[i]) / c;

        int zero = 0, scale = 0;
        int safe = 0;
        short *best=NULL, *buf=NULL;
AGAIN:
        for(int ii=17; ii>12 && ii<21; ii++) {
            if (buf == NULL) buf = new short[N*L];
            int z = try_compress(src, N, L, ii, avg, buf, TRY_STEP, safe);
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
        if (zero ==0 && !safe) {
            safe = 1;
            goto AGAIN;
        }

        if (buf != NULL) delete []buf;

        if (TRY_STEP > 1) {
            zero = try_compress(src, N, L, scale, avg, best, 1, safe);
            while (zero == 0) {
                scale --;
                cout << "try " << scale << endl;
                zero = try_compress(src, N, L, scale, avg, best, 1, safe);
            }
        }
        assert(scale > 0);

        // output
        short *start = dst;
        *dst++ = scale;
        int *length = (int*)dst;
        dst += 2;
        
        struct Bits bits(dst);
        REP(x, N) {
            bits.write(best[x*L]&0xf, 4);
            best[x*L] >>= 4;
        }
        bits.flush();
        dst = bits.p;
        
        if (*last_avg == NULL) {
            *last_avg = new short[L];
            REP(i, L) dst[i] = avg[i];
            dst += L;
            coder->init_encoder(dst);
        } else {
            short t[L];
            REP(i, L) t[i] = avg[i] - (*last_avg)[i];
            for(int i=L-1;i>0; i--) t[i] -= t[i-1];
            //for(int i=L-1;i>1; i--) t[i] -= t[i-1];
            
            coder->init_encoder(dst);
            coder->put_bits(t[0] & 0xf, 4);
            t[0] >>= 4;
            REP(i, L) coder->encode(t+i, 1);
//            cout << "avg used " << (coder->current - coder->buffer) << endl;
 //           REP(i, L) cout << t[i] << " " ; cout << endl;
        }
        REP(i, L) (*last_avg)[i] = avg[i];
        
        REP(x, N) coder->encode(best+x*L, L);
        *length = coder->flush() + dst - start;
//        cout << (*length*16/N) << endl; 
        delete []best;
//        cout << "enc block " << *length << endl;
        return *length;
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

        int XBLOCKS = *src++;
        int YBLOCKS = *src++;
        int NX = X/XBLOCKS, NY= Y/YBLOCKS;
        short *avg = NULL;
        struct ArCoder *coder = new ArCoder;
        REP(x, XBLOCKS) {
            REP(y, YBLOCKS) {
                src += decompress_block(src, dst, x, y, NX, NY, Y, L, &avg, coder);
            }
        }
        delete []avg;

        VI output;
        output.swap(myoutput);
        return output;
    }
   
    int decompress_block(short *src, short *dst, int x, int y, int NX, int NY, int Y, int L,
        short **last_avg, struct ArCoder *coder) {
        int SCALE = *src++;
        int length = *(int*)src;
        src += 2;

        struct Bits r(src);
        REP(x1, NX) {
            short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
            REP(y1, NY) {
                buf[y1*L] = r.read(4);
            }
        }
        r.rollback();
        src = r.p;
      
        short avg[L];
        if (*last_avg == NULL) {
            *last_avg = new short[L];
            REP(i, L) avg[i] = src[i];
            src += L;
            coder->init_decode(src);
        } else {
            coder->init_decode(src);
            int header = coder->get_bits(4);
            REP(i, L) coder->decode(avg+i, 1);
            avg[0] <<= 4;
            avg[0] += header;
            //FOR(i, 2, L) avg[i] += avg[i-1];
            FOR(i, 1, L) avg[i] += avg[i-1];
            REP(i, L) avg[i] += (*last_avg)[i];
        }
    //          cout << endl;REP(i, L) cout << avg[i] << " "; cout << endl;
        REP(i, L) (*last_avg)[i] = avg[i];


        short tmp[60];
        REP(x1, NX) {
            short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
            REP(y1, NY) {
                coder->decode(tmp, L);
                tmp[0] <<= 4;
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
//        cout << "block size " << (length) << endl;
        return length;
    }
};
