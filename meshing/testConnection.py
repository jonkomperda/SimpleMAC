import operator
import sys
import re

conlist = []
conlist.append([0,1,3,2])
conlist.append([0,1,3,2])

print 'conlist'
print conlist
print 'length conlist ' + str(len(conlist))

pList = []
pList.append([(0,0,0),(1.0,0,0),(0,1,0),(1,1,0)])
pList.append([(1,0,0),(2,0,0),(1,1,0),(2,1,0)])

print 'plist'
print pList
print 'length pList ' + str(len(pList))

def sort(val):
    out = sorted(val, key=operator.itemgetter(2,1,0))
    return out

newlist = set(pList[0]+pList[1])
newlist = sort(newlist)

print 'newlist'
print newlist

newcon = []

for k in range(len(conlist)):
    for i in conlist[k]:
        print i
        p = pList[k][i]
        newcon.append(newlist.index(p))			#searches for element p position in the new list
        
print 'newcon'
print newcon

def chopper(list,size):
    for i in xrange(0, len(list), size):
        yield list[i:i+size]

newconlist = list(chopper(newcon,4))

print 'newconlist'
print newconlist

