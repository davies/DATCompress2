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

const int SCALE=20;
const int HEADER=11;
const int HEADER_OFFSET= 1<<(HEADER-1);

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
    {0, 1},  // 0
    {0, 1},
    {0, 1},
    {0, 1},
    {-1, 2}, // -1
    {-1, 2},
    {1, 3},  // 1
    {2, 4},  // xxx
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
    inline void encode_byte(int d, Bits &bits) {
        if (d >= -1 && d <= 1) {
            bits.write(enc_table[d+1].code, enc_table[d+1].len);
        } else {
            bits.write(enc_table[3].code, enc_table[3].len);
            bits.write(d > 0 ? 1 : 0, 1);
            d = abs(d) - 2;
            while (d > 0) {
                bits.write(1, 1);
                d --;
            }
            bits.write(0, 1);
        }
    }

    inline int decode_byte(Bits &bits) {
        int d = bits.peek(3);
        if (dec_table[d].len < 4) {
            bits.shift(dec_table[d].len);
            return dec_table[d].code;
        } else{
            bits.shift(3);
            int flag = bits.read_bit();
            int r = 2;
            int n = bits.read_bit();
            while (n) {
                r ++;
                n = bits.read_bit();
            }
            return flag ? r : -r;
        }
    }

    VI compress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
        //cout << X << Y << L << " " << SZ(dat) << endl;
        src += 3;
        int avg[60] = {0}, N=X*Y, c=0;
        if (N>1000) N = 1000;
        REP(i, L) avg[i] = 0;
        REP(i, N) {
            if (src[i*L] < 1000) continue;
            REP(j, L) avg[j] = avg[j] + src[i*L+j];
            c ++;
        }
        REP(i, L) avg[i] = (avg[i] + c/2) / c;
        int m=avg[0];
        FOR(i, 1, L) if (avg[i]<m) m = avg[i];
        //REP(i, L) avg[i] -= m;
        //REP(i, L) cout << avg[i] << " "; cout << endl;
        
        int tmp[100] = {0};
        myvector myoutput(SZ(dat)+1000);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        REP(i, L) dst[i] = avg[i];
        dst += L;

        struct Bits bits(dst);
        REP(x, X*Y) {
            if (1) {
                REP(j, L) tmp[j] = (src[j] - avg[j] + (src[j] > avg[j] ? SCALE/2 : -SCALE/2)) / SCALE;
    //            cout << "row " << x << " " ; REP(i, L) cout << tmp[i] << " "; cout << endl;
                for(int j=L-1;j>0;j--) tmp[j] = tmp[j] - tmp[j-1];
      //          cout << "rdiff " << x << " " ; REP(i, L) cout << tmp[i] << " "; cout << endl;
                // encoding
                bits.write(tmp[0] + HEADER_OFFSET, HEADER);
                FOR(j, 1, L) encode_byte(tmp[j], bits);
            }else{
                bits.write(0, HEADER);
            }
            src += L;
        }
        bits.flush();
        dst = bits.p;

        myoutput.myresize((dst - (short*)&myoutput[0]+1)/2);
        //cout << "size " << SZ(myoutput) << endl;

        VI output;
        output.swap(myoutput);
//        REP(i, SZ(output)) cout << output[i] << " " ; cout << endl;
        return output;
    }

    VI decompress(VI &dat) {
        short *src = (short*) &dat[0];
        int X=src[0], Y=src[1], L=src[2];
        src += 3;
        short* avg = src;
        src += L;
        //REP(i, L) cout << avg[i] << " "; cout << endl;

        myvector myoutput((X*Y*L+4)/2);
        short *dst = (short*)&myoutput[0];
        *dst++ = X;
        *dst++ = Y;
        *dst++ = L;
        
        struct Bits bits(src);
        int tmp[100];
        REP(x, X*Y) {
            tmp[0] = bits.peek(HEADER) - HEADER_OFFSET;
            bits.shift(HEADER);
            if (1) {
                FOR(i, 1, L) tmp[i] = decode_byte(bits);
    //            cout << "rdiff " << x << " " ; REP(i, L) cout << tmp[i] << " "; cout << endl;
                FOR(i, 1, L) tmp[i] += tmp[i-1];
      //          cout << "rdiff2 " << x << " " ; REP(i, L) cout << tmp[i] << " "; cout << endl;
                REP(j, L) dst[j] = avg[j] + tmp[j] * SCALE;
                REP(j, L) if(dst[j] < 0) dst[j] = 0;
                REP(j, L) if(dst[j] > 16383) dst[j] = 16383;
            }else{
                REP(j, L) dst[j] = 0;
            }
            dst += L;
        }

        VI output;
        output.swap(myoutput);
        return output;
    }
};


int main(int argc, char **argv) {
    DATCompression2 comp;
    /*short r[20];
    int rr[20];
    int t[] = {0,-1,1, 2,-3, 0, 5,-6,-1, 0};
    Bits bits(r);
    REP(i, 10) cout << t[i] << " "; cout << endl;
    REP(i, 10) comp.encode_byte(t[i], bits);
    bits.flush();
    REP(i, 10) cout << r[i] << " "; cout << endl;
    Bits bits2(r);
    REP(i, 10) rr[i] = comp.decode_byte(bits2);
    REP(i, 10) cout << rr[i] << " "; cout << endl;*/
    
    {
/*    int h[256]={-3, -3, 0, -2, -1, 0, -1, -2, -1, -15, 17, -30, 34, -52, 51, -90, 95};
    int r[10000], rr[70], size=20;
    REP(i, size) h[i] = i;
    REP(i, size) cout << h[i] << " "; cout << endl;
    int n = comp.huffman_encode(h, size, r);
    REP(i, n) cout << r[i] << " "; cout << endl;
    int m = comp.huffman_decode(r, size, rr);
    cout << size << " -> " << n << " " << m << endl;
    //REP(i, size) rr[i] -= 128;
    REP(i, size) cout << rr[i] << " "; cout << endl;*/
    }

    for(int i=1; i<argc; i++) {
        FILE *f = fopen(argv[i], "r");
        if (f == NULL) {
            cout << "open failed" << argv[i] << endl; 
            continue;
        }
        myvector input((900*900*59+4)/2);
        short *buffer = (short*)&input[0];
        int n = fread(buffer, sizeof(short), 3, f);
        if (n <= 0) {
            cout << "read failed" << n << endl;
            continue;
        }
        
        int X=buffer[0], Y=buffer[1], L=buffer[2];
        //X = 100;
        //Y = 900;
        n = fread(buffer+3, sizeof(short), X*Y*L, f);
        fclose(f);
        if (n <= 0) {
            cout << "read failed" << n << endl;
            continue;
        }
        if (n < X * Y * L)  {
            cout << " data too short " << n << " < " << (X * Y * L) << endl;
            continue;
        }
        buffer[0] = X;
        buffer[1] = Y;
       
        clock_t start = clock();
        VI result = comp.compress(input);
        VI odata = comp.decompress(result);
        double ratio = double(SZ(odata))/SZ(result);
        double t = float(clock() - start)/CLOCKS_PER_SEC;

        float diff = 0, diffv = 0;
        short *src = &buffer[3], *dst = ((short*)&odata[0]) + 3;
        int N = X*Y*L;
        REP(j, N) {
            short d = src[j] - dst[j];
            if (dst[j] < 0 || dst[j] > 16383) {
                cout << j << " overflow " << dst[j] << endl;
            }
            if (abs(d) >10) {
                cout << j << " " << d << " " << src[j] << " " << dst[j] << endl;
            }
            diff += d;
            diffv += d*d;
        }
        cout << argv[i] << " " << X << " " << Y << " " << L << " ratio " << ratio << " used " << t << " diff " << (diff / N) << " " << (diffv / N) << endl;
    }
    return 0;
}
