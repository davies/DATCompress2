import sys
import huffman

for n in sys.argv[1:]:
    print n
    rows = []
    for line in open(n):
        line = line.strip().split('\t')
        ls = [int(x) for x in line[2:]]
        rows.append(ls)
    #    x,y = line[:2]
    #    m = min(ls)
    #    d = [v-ls[i-1] for i,v in enumerate(ls)][1:]
    #    rows.append(d)
    #    ed = [i for i in d if i>=8 or i < -8]
    #    ed = [i for i in d if i>=16 or i < -16]
    #    ed2 = [i for i in d if i>=32 or i < -32]
    #    ed3 = [i for i in d if i>=64 or i < -64]
    #    if len(ed2) > 5:
    #    print x,y, min(ls), min(d), max(d), len(ed), len(ed2), len(ed3), d

    first = [row[0] for row in rows]
    print min(first), max(first)

    base = [sum(rows[j*97][i]-rows[j*97][max(i-1,0)] for j in range(100))/100 for i in range((len(rows[0])))] 
    #print base
    c = 0
    st = {}
    data = []
    for k,row in enumerate(rows):
        d = [row[j+1]-row[max(j,0)] for j in range(len(row)-1)]
        data.extend(d)
        for n in d:
            st[n] = st.get(n, 0) + 1
        ed = [i for i in d if i>=16 or i < -16]
        ed2 = [i for i in d if i>=32 or i < -32]
    #    if len(ed) < 12:
        c += len(ed) * 8.0
    #    if len(ed) > 8:
    #        print len(ed)
    #    else:
    #        c += len(ed2) * 4.0 + len(row)
    #    if len(ed2) > 18:
    #        print k, min(d), max(d), max(d)-min(d), len(ed), len(ed2)

    #for k,v in sorted(st.items()):
    #    print k,v
    enc = huffman.Encoder()
    enc.long_str = data
    print float(enc.code_length) / len(rows) / len(row)
    print (c)/len(rows)/len(row)
    #for i in range(len(rows[0])):
    #    d = [rows[j][i]-base[i] for j in range(len(rows))]
    #    print i, min(d), max(d), max(d)-min(d)
