""" © jon komperda """

a = [0.999999,.299999,1.025,7.899999]
b = [1.0, 0.3, 1.4, 1.9999999, 4.5, 9.999999]
epsilon = 5

def checkEps(n):
    thisOut = []
    for item in n:
        if item != round(item,epsilon):
            thisOut.append(round(item,epsilon))
        else:
            thisOut.append(item)
    return thisOut

def addDelDup(item1,item2):
    temp = item1 + item2
    del_it = list(set(temp))
    return del_it

out = checkEps(a)
out2 = checkEps(b)

newList = addDelDup(out,out2)
print newList

