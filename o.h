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


const unsigned int MinLength = 0x01000000U;   // threshold for renormalization
const unsigned int MaxLength = 0xFFFFFFFFU;      // maximum AC interval length
const unsigned int LengthShift = 15;     // length bits discarded before mult.
const unsigned int MaxCount    = 1 << LengthShift;  // for adaptive model2
const int LOOKUP_LIMIT = 16;

struct Model {
    unsigned int *distribution, *symbol_count, *decoder_table;
    unsigned total_count, update_cycle, symbols_until_update;
    unsigned data_symbols, last_symbol, table_size, table_shift;
    Model():distribution(NULL), symbol_count(NULL), decoder_table(NULL),
            total_count(0),update_cycle(0), symbols_until_update(0), 
            data_symbols(0),last_symbol(0), table_size(0), table_shift(0) {}
   
    void set_alphabet(int size) {
        set_distribution(NULL, size);
    }
 
    void set_distribution(double *probability, unsigned int size) {
        last_symbol = size - 1;
        data_symbols = size;
        if (data_symbols > LOOKUP_LIMIT) {
            int table_bits = 3;
            while (data_symbols > (1U << (table_bits + 2))) ++table_bits;
            table_size  = (1 << table_bits) + 4;
            table_shift = LengthShift - table_bits;
            distribution = new unsigned[data_symbols*2 + table_size+6];
            decoder_table = distribution + data_symbols * 2; 
        } else {
            decoder_table = NULL;
            table_size = table_shift = 0;
            distribution = new unsigned[data_symbols*2];
        }
        symbol_count = distribution + data_symbols;

     /* unsigned s = 0;
      double sum = 0.0, p = 1.0 / double(data_symbols);

      for (unsigned k = 0; k < data_symbols; k++) {
        if (probability) p = probability[k];
//        if ((p < 0.0001) || (p > 0.9999)) AC_Error("invalid symbol probability");
        distribution[k] = unsigned(sum * (1 << LengthShift));
        sum += p;
        if (table_size == 0) continue;
        unsigned w = distribution[k] >> table_shift;
        while (s < w) decoder_table[++s] = k - 1;
      }

      if (table_size != 0) {
        decoder_table[0] = 0;
        while (s <= table_size) decoder_table[++s] = data_symbols - 1;
      }*/

        total_count = 0;
        update_cycle = data_symbols;
        for (unsigned k = 0; k < data_symbols; k++) symbol_count[k] = 1;
        update(false);
        symbols_until_update = update_cycle = (data_symbols + 6) >> 1;
    }

    void update(bool enc) {
        //      cout << "update " << total_count << endl;
        if ((total_count += update_cycle) > MaxCount) {
            total_count = 0;
            for (unsigned n = 0; n < data_symbols; n++)
                total_count += (symbol_count[n] = (symbol_count[n] + 1) >> 1);
        }
        // compute cumulative distribution, decoder table
        unsigned k, sum = 0, s = 0;
        unsigned scale = 0x80000000U / total_count;

        if (enc || (table_size == 0))
            for (k = 0; k < data_symbols; k++) {
                distribution[k] = (scale * sum) >> (31 - LengthShift);
                sum += symbol_count[k];
            }
        else {
            for (k = 0; k < data_symbols; k++) {
                distribution[k] = (scale * sum) >> (31 - LengthShift);
                sum += symbol_count[k];
                unsigned w = distribution[k] >> table_shift;
                while (s < w) decoder_table[++s] = k - 1;
            }
            decoder_table[0] = 0;
            while (s <= table_size) decoder_table[++s] = data_symbols - 1;
        }
        // set frequency of model updates
        update_cycle = (5 * update_cycle) >> 2;
        unsigned max_cycle = (data_symbols + 6) << 3;
        if (update_cycle > max_cycle) update_cycle = max_cycle;
        symbols_until_update = update_cycle;
    }
};

const int MAX_NUMBER = 16;
const int SYMBOL_COUNT = MAX_NUMBER * 2+1;
const int MAX_L = 60;
const int WIDTH = 5;
const int GROUPS = 60*WIDTH*WIDTH;

struct ArCoder {
    unsigned char *buffer, *ac_pointer;
    unsigned int base, value, length, buffer_size;

    Model model0[MAX_L];
    Model model1[MAX_L*WIDTH];
    Model model2[GROUPS];

    ArCoder() {
        REP(i, GROUPS) {
//            model2[i].set_distribution(NULL, SYMBOL_COUNT);
              model2[i].set_alphabet(SYMBOL_COUNT);
        }
    }
    
    inline void propagate_carry() {
        unsigned char *p = ac_pointer -1;
        for (p = ac_pointer - 1; *p == 0xFFU; p--) *p = 0;
        ++*p;
    }

    inline void renorm_enc_interval() {
        do {
            *ac_pointer++ = (unsigned char)(base >> 24);
            base <<= 8;
        } while ((length <<= 8) < MinLength);
    }

    inline void renorm_dec_interval() {
        do {
            value = (value << 8) | unsigned(*++ac_pointer);
        } while ((length <<= 8) < MinLength);
    }

    inline void put_bits(unsigned int data, int bits) {
        unsigned init_base = base;
        base += data * (length >>= bits);            // new interval base and length

        if (init_base > base) propagate_carry();                 // overflow = carry
        if (length < MinLength) renorm_enc_interval();        // renormalization
    }

    inline unsigned int get_bits(int bits) {
        unsigned s = value / (length >>= bits);      // decode symbol, change length

        value -= length * s;                                      // update interval
        if (length < MinLength) renorm_dec_interval();        // renormalization

        return s;
    }

    void init_encoder(short *dst) {
        buffer = (unsigned char*)dst;
        base = 0;
        length = MaxLength;
        ac_pointer = buffer;
    }

    void encode_byte(unsigned int data, Model &M) {
        unsigned x, init_base = base;
        // compute products
        if (data == M.last_symbol) {
            x = M.distribution[data] * (length >> LengthShift);
            base   += x;                                            // update interval
            length -= x;                                          // no product needed
        }
        else {
            x = M.distribution[data] * (length >>= LengthShift);
            base   += x;                                            // update interval
            length  = M.distribution[data+1] * length - x;
        }

        if (init_base > base) propagate_carry();                 // overflow = carry

        if (length < MinLength) renorm_enc_interval();        // renormalization

        ++M.symbol_count[data];
        if (--M.symbols_until_update == 0) M.update(true);  // periodic model update

    }

    void encode_symbol(int idx, int v) {
        while (abs(v) >= MAX_NUMBER) {
            encode_byte(0, model2[idx]);
            put_bits(v & 0xf, 4);
            v >>= 4;
        }
        encode_byte(v + MAX_NUMBER, model2[idx]);
    }
    
    void encode(short *src, int size) {
        int code0=0, code1 = 0;
        for(int i=0; i<size-1; i+=2) {
            int idx = i*25 + code0*5 + code1;
            int v = src[i];
            encode_symbol(idx, v);
            code0 = v < -2 ? 0 : (v > 2 ? 4 : (v + 2));
            
            idx = (i+1)*25 + code1*5 + code0;
            v = src[i+1];
            encode_symbol(idx, v);
            code1 = v < -2 ? 0 : (v > 2 ? 4 : (v + 2));
        }
        if (size%2) {
            int idx = (size-1)*25 + code0*5 + code1;
            int v = src[size-1];
            encode_symbol(idx, v);
        }
//        cout << "encode " ; REP(i, size) cout << src[i] << " "; cout << endl;
    }

    int flush() {
        unsigned init_base = base;            // done encoding: set final data bytes

        if (length > 2 * MinLength) {
            base  += MinLength;                                     // base offset
            length = MinLength >> 1;             // set new length for 1 more byte
        }
        else {
            base  += MinLength >> 1;                                // base offset
            length = MinLength >> 9;            // set new length for 2 more bytes
        }

        if (init_base > base) propagate_carry();                 // overflow = carry

        renorm_enc_interval();                // renormalization = output last bytes

        unsigned code_bytes = unsigned(ac_pointer - buffer);

        return code_bytes;                                   // number of bytes used
    }

    void init_decode(short *dst) {
        buffer = (unsigned char*)dst;
        length = MaxLength;
        value = (unsigned(buffer[0]) << 24)|(unsigned(buffer[1]) << 16) |
          (unsigned(buffer[2]) <<  8)| unsigned(buffer[3]);
        ac_pointer = buffer + 3;
    }

    int decode_byte(Model &M) {
        unsigned n, s, x, y = length;

        if (M.decoder_table) {              // use table look-up for faster decoding

            unsigned dv = value / (length >>= LengthShift);
            unsigned t = dv >> M.table_shift;

            s = M.decoder_table[t];         // initial decision based on table look-up
            n = M.decoder_table[t+1] + 1;

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
            unsigned m = (n = M.data_symbols) >> 1;
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

        if (length < MinLength) renorm_dec_interval();        // renormalization

        ++M.symbol_count[s];
        if (--M.symbols_until_update == 0) M.update(false);  // periodic model update

        return s;
    }


    int decode_symbol(int idx) {
        int r = decode_byte(model2[idx]) - MAX_NUMBER;
        if (r != -MAX_NUMBER) return r;
        int v = 0, c = 0;
        do {
            v |= get_bits(4) << c;
            c += 4;
            r = decode_byte(model2[idx]) - MAX_NUMBER;
        } while (r == -MAX_NUMBER);
        v += r << c; 
        return v;
    }

    void decode(short *dst, int size) {
        int code0=0, code1 = 0;
        for(int i=0; i<size-1; i+=2) {
            int idx = i*25 + code0*5 + code1;
            int v = decode_symbol(idx);
            dst[i] = v; 
            code0 = v < -2 ? 0 : (v > 2 ? 4 : (v + 2));
            
            idx = (i+1)*25 + code1*5 + code0;
            v = decode_symbol(idx);
            dst[i+1] = v; 
            code1 = v < -2 ? 0 : (v > 2 ? 4 : (v + 2));
        }
        if (size % 2) {
            int idx = (size-1)*25 + code0*5 + code1;
            dst[size-1] = decode_symbol(idx);
        }
//        cout << "decode " ; REP(i, size) cout << dst[i] << " "; cout  << endl;
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
        bits.flush();
        dst = bits.p;

        static struct ArCoder *coder = NULL;
        if (coder == NULL) {
            coder = new ArCoder;
//            cout << "sizeof " << (sizeof(coder) >> 10) << endl;
//            REP(x, N) coder->learn(best+x*L, L);
//            coder->save(bits);
        }
//        coder->train();
        
        coder->init_encoder(dst);
        REP(x, N) coder->encode(best+x*L, L);
        *length = (coder->flush()+1)/2 + dst - start;
//        cout << (*length*16/N) << endl; 
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
        r.rollback();
        src = r.p;

        static struct ArCoder *coder = NULL;
        if (coder == NULL) {
            coder = new ArCoder;
//            coder->load(r);
        }
  //      coder->train();
        
        coder->init_decode(src);
        short tmp[60];
        REP(x1, NX) {
            short *buf = dst + ((x*NX+x1)*Y+y*NY)*L;
            REP(y1, NY) {
                coder->decode(tmp, L);
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
//        cout << "block size " << (length) << endl;
        return length;
    }
};
