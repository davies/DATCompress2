#include "o.h"

int main(int argc, char **argv) {
    DATCompression2 comp;
//    cout << (-4 % 64) << " " << (-4 / 64) << endl;
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
    //
    FOR(i, -3, 3) {
        short buf[20];
        Bits writer(buf);
        comp.encode_byte(i, writer);
        writer.flush();
        Bits reader(buf);
        int x = comp.decode_byte(reader);
        if (x!=i) {
            printf("failed %d %d\n", i,x);
        }
        FOR(j, -3, 3) {
            if (i+j == 0 && i*j == -1) continue;
            short buf[20];
            Bits writer(buf);
            comp.encode_two_byte(i, j, writer);
            writer.flush();
            Bits reader(buf);
            int x,y;
            comp.decode_two_byte(x, y, reader);
            if (x!=i || y!=j) {
                printf("failed %d %d %d %d\n", i, j, x, y);
            }
            FOR(k, -1, 1) {
                FOR(l, -1, 1) {
                    short buf[20];
                    Bits writer(buf), reader(buf);
                    comp.encode_byte4(i, j, k, l, writer);
                    writer.flush();
                    int x,y,z,w;
                    comp.decode_byte4(x, y, z, w, reader);
                    if (x!=i || y!=j || k!=z || l!=w) {
                        printf("failed %d %d %d %d %d %d %d %d\n", i, j, k, l, x, y, z, w);
                    }
                }
            }
        }
    }

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
        X = 900;
        Y = 900;
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

        int diff = 0;
        uint64_t diffv = 0;
        short *src = &buffer[3], *dst = ((short*)&odata[0]) + 3;
        int N = X*Y*L;
        REP(j, N) {
            short d = src[j] - dst[j];
            if (dst[j] < 0 || dst[j] > 16383) {
                cout << j << " overflow " << dst[j] << endl;
            }
            if (abs(d) >50) {
                cout << j << " " << d << " " << src[j] << " " << dst[j] << endl;
            }
            diff += d;
            diffv += d*d;
        }
        cout << argv[i] << " " << X << " " << Y << " " << L << " ratio " << ratio << " used " << t << " diff " << (diff / N) << " " << (diffv / N) << endl;
    }
    return 0;
}
