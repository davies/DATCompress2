#!/usr/bin/env python
import sys, os, os.path
import random
from dpark import DparkContext
dpark = DparkContext()
from vector import Vector

def parseVector(line):
    num = map(int, line.strip().split('\t')[2:])
    num = [n-num[0] for n in num]
    return Vector(num[20:])
    return Vector(map(int, line.strip().split('\t')[2:]))

def closestCenter(p, centers):
    bestDist = p.squaredDist(centers[0])
    bestIndex = 0
    for i in range(1, len(centers)):
        d = p.squaredDist(centers[i])
        if d < bestDist:
            bestDist = d
            bestIndex = i
    return bestIndex

def minDist(p, centers):
    bestDist = p.squaredDist(centers[0])
    for i in range(1, len(centers)):
        d = p.squaredDist(centers[i])
        if d < bestDist:
            bestDist = d
    return bestDist

if __name__ == '__main__':
    K = 100
    IT = 50
    MIN_DIST = 0.01
    PATH = 'tab/1558dee2ecfb7a0f9f63e27376675b6c.tab'
    points = dpark.textFile(PATH, numSplits=100)[:1].map(parseVector).cache()
    print points.count()
    centers = points.take(K)

    for it in range(IT):
        print 'iteration', it
        mappedPoints = points.map(lambda p:(closestCenter(p, centers), (p, 1)))
        ncenters = mappedPoints.reduceByKey(
                lambda (s1,c1),(s2,c2): (s1+s2,c1+c2)
            ).map(
                lambda (id, (sum, count)): (id, sum/count)
            ).collectAsMap()
        
        updated = False
        for i in ncenters:
            if centers[i].dist(ncenters[i]) > MIN_DIST:
                centers[i] = ncenters[i]
                updated = True
#        if not updated:
#            break
       # print 'centers', centers
        print 'dist', sum(points.map(lambda p: minDist(p, centers)).collect())
        print 

#    print 'final', centers

