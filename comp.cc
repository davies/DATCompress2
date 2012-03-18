#include "o.h"

int main(int argc, char **argv) {
    test_huffman();

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
    FOR(i, -30, 30) {
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
            FOR(k, -1, 2) {
                FOR(l, -1, 2) {
                    if (k+l == 0 && k*l == -1) continue;
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
    
    float total = 0;
    int valid=0;
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
        //// stat for header
        //int st[20];
        //REP(j, 20) st[j] = 0;
        //REP(x, X) REP(y, Y) {
        //    int v = buffer[(x*Y+y)*L+4];
        //    st[v/1000] = st[v/1000] + 1;
        //}
        //cout << "HEADER: "<<i<<" "; REP(j, 17) cout << st[j] << "\t" ; cout << endl;

        buffer[0] = X;
        buffer[1] = Y;

        X = X/1;
        REP(k, 1){
        short *header = buffer + k * X * Y * L;
        header[0] = X; header[1] = Y; header[2] = L;
        clock_t start = clock();
        VI inp((int*)header, (int*)(header+X*Y*L+4));
        VI result = comp.compress(inp);
        VI odata = comp.decompress(result);
        double ratio = double(SZ(odata))/SZ(result);
        double t = float(clock() - start)/CLOCKS_PER_SEC;

        int diff = 0;
        int64_t diffv = 0;
        short *src = &header[3], *dst = ((short*)&odata[0]) + 3;
        int N = X*Y*L;
        cout << endl;
        REP(j, N) {
            short d = dst[j] - src[j];
//            cout << diffv << " " << dst[j] << " " << d << " ;";
            if (dst[j] < 0 || dst[j] > 16383) {
//                cout << j << " overflow " << dst[j] << endl;
            }
            if (abs(d) >50) {
//                cout << j << " " << d << " " << src[j] << " " << dst[j] << endl;
            }
            diff += d;
            diffv += d*d;
        }
        valid += 1;
        if (t < 8 && diffv < 36*N) {
            total += ratio;
        }
        cout << argv[i] << " " << X << " " << Y << " " << L << " ratio " << ratio << " used " << t << " diff " << (diff / N) << " " << (diffv/N) << endl;
        }
    }
    cout << "total " << valid << " ratio " << (total / valid) << endl;
    return 0;
}
