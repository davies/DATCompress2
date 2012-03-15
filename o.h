#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <sys/time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
using namespace std;

typedef long long ll;
typedef vector<int> VI;
typedef vector< VI > VVI;
typedef vector<string> VS;
typedef istringstream ISS;
typedef ostringstream OSS;

//const int SCALE=18;

#define BYTE4
//#define BYTE2

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
//        cout << "write " << b << " " << d << endl;
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
    //    cout << "shift " << b << endl;
        bits -= b;
    }
    int read_bit() {
        int r = peek(1);
        shift(1);
        return r;
    }
};


class DATCompression2 {
public:
    inline void write_byte(int d, Bits &bits, int base) {
        bits.write(d > 0 ? 1 : 0, 1);
        d = abs(d) - base;
        while (d > 0) {
            bits.write(1, 1);
            d --;
        }
        bits.write(0, 1);
    }

    inline int read_byte(Bits &bits, int base) {
        int flag = bits.read_bit();
        int r = base;
        int n = bits.read_bit();
        while (n) {
            r ++;
            n = bits.read_bit();
        }
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
            || d1==-1 && d2==-1 || d1==1 && d2==1) {
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
        if (ord == 1 && s == 1 || s == -1 && ((d1+1)|(d2+1)|(d3+1)|(d4+1)) == 1) {
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

    int try_compress(short *src, int size, int L, int *avg, short *dst, int SCALE) {
        int64_t vdiff = 0;
        int zero = 0;
        int tmp[60];
        REP(x, size) {
            REP(j, L) tmp[j] = (src[j] - avg[j] + (src[j] > avg[j] ? SCALE/2 : -SCALE/2)) / SCALE;
            for(int j=L-1;j>0;j--) tmp[j] = tmp[j] - tmp[j-1];
            
            // smooth
            if (vdiff < 36*x*L-6000) {
                FOR(i, 2, L) if (tmp[i-1] + tmp[i]==0 && tmp[i-1]*tmp[i]== -1) {
                    tmp[i-1] = 0;
                    tmp[i] = 0;
                }
            } else {
                for (int i=2; i<L; i+=2) if (tmp[i-1] + tmp[i]==0 && tmp[i-1]*tmp[i]== -1) {
                    tmp[i-1] = 0;
                    tmp[i] = 0;
                }
            }
            
            // counting
            FOR(i, 1, L) if (tmp[i]==0) zero ++;
            REP(i, L) dst[i] = tmp[i];

            // calc vdiff
            FOR(i, 1, L) tmp[i] += tmp[i-1];
            REP(i, L) {
                int v = tmp[i] * SCALE + avg[i];
                if (v < 0) v=0; if (v>16383) v=16383;
                int df = (v - src[i]);
                vdiff += df * df;
            }

            src += L;
            dst += L;
        }
        if (vdiff >= 36 * size * L) zero = 0;
        return zero;
    }

    VI compress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
        //cout << X << Y << L << " " << SZ(dat) << endl;
        src += 3;
        int avg[60] = {0}, N=X*Y, c=0;
        if (N>10000) N = 10000;
        int step = X*Y/N;
        REP(i, L) avg[i] = 0;
        REP(i, N) {
//            if (src[i*L] < 200) continue;
            REP(j, L) avg[j] = avg[j] + src[i*step*L+j];
            c ++;
        }
        REP(i, L) avg[i] = (avg[i]) / c;
//        REP(i, L) cout << avg[i] << " "; cout << endl;
       
        int zero = 0, scale = 0;
        short *best=NULL, *buf=NULL;

        FOR(ii, 16, 20) {
            if (buf == NULL) buf = new short[X*Y*L];
            int z = try_compress(src, X*Y, L, avg, buf, ii);
            cout << "try " << ii << " " << (float(z)/X/Y/L) << endl; 
            if (z > zero) {
                zero = z;
                scale = ii;
                short *p = best;
                best = buf;
                buf = p;
            } else {
                break;
            }
        }
        if (buf != NULL) delete []buf;

        // output
        myvector myoutput(SZ(dat)+1000);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        *dst++ = scale;
        REP(i, L) dst[i] = avg[i];
        dst += L;

        bool fourbyte = zero > 0.815 * X * Y * L;
        *dst ++ = int(fourbyte);
        //cout << "fourbyte " << fourbyte << endl;
        struct Bits bits(dst);
        short *tmp = best;
        REP(x, X*Y) {
            // encoding
            tmp[0] += 32;
            int mod = tmp[0] % 64, cc = tmp[0]/64;
            if (mod < 0) {
                mod += 64;
                cc -= 1;
            }
            bits.write(mod, 6);
            encode_byte(cc, bits);
            
            if (fourbyte) { 
                REP(j, (L-1)/4) encode_byte4(tmp[j*4+1], tmp[j*4+2], tmp[j*4+3], tmp[j*4+4], bits);
                if ((L-1)%4 > 1) encode_two_byte(tmp[(L-1)/4*4+1], tmp[(L-1)/4*4+2], bits);
            } else {
                REP(j, (L-1)/2) encode_two_byte(tmp[j*2+1], tmp[j*2+2], bits);
            }
            if ((L-1)%2 > 0) encode_byte(tmp[L-1], bits);

            src += L;
            tmp += L;
        }
        bits.flush();
        dst = bits.p;
        myoutput.myresize((dst - (short*)&myoutput[0]+1)/2);
        delete []best;

        VI output;
        output.swap(myoutput);
        return output;
    }

    VI decompress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2], SCALE=src[3];
        src += 4;
        short* avg = src;
        src += L;
//        cout << endl;REP(i, L) cout << avg[i] << " "; cout << endl;

        myvector myoutput((X*Y*L+4)/2);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        
        bool fourbyte = *src++;
        //cout << "fourbyte " << fourbyte << endl;
        struct Bits bits(src);
        int tmp[100];
        REP(x, X*Y) {
            tmp[0] = bits.peek(6) - 32;
            bits.shift(6);
            tmp[0] += decode_byte(bits) * 64;
            
            if (fourbyte) {
                REP(j, (L-1)/4) decode_byte4(tmp[j*4+1], tmp[j*4+2], tmp[j*4+3], tmp[j*4+4], bits);
                if ((L-1)%4 > 1) decode_two_byte(tmp[(L-1)/4*4+1], tmp[(L-1)/4*4+2], bits);
            } else {
                REP(i, (L-1)/2) decode_two_byte(tmp[i*2+1], tmp[i*2+2], bits);
            }
            if ((L-1)%2 > 0) tmp[L-1] = decode_byte(bits);
                
            FOR(i, 1, L) tmp[i] += tmp[i-1];
            REP(j, L) dst[j] = avg[j] + tmp[j] * SCALE;
            REP(j, L) if(dst[j] < 0) dst[j] = 0;
            REP(j, L) if(dst[j] > 16383) dst[j] = 16383;
            dst += L;
        }

        VI output;
        output.swap(myoutput);
        return output;
    }
};
