import sys
import re
import numpy as ar
import operator
import pyvtk

############## Begin shapes
class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self, points, old1='none', old2='none', old1c = 'none', old2c = 'none'):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        
        if( old1 == 'none' and old2 == 'none' ):
            self.old1 = 0
            self.old2 = 0
            self.old1c = 0
            self.old2c = 0
        else:
            self.old1 = old1
            self.old2 = old2
            self.old1c = old1c
            self.old2c = old2c
        
        self.connect= self.connections(self.points)
    
    # Finds the element connections (*Needs to be improved, issues with gaps)
    def connections(self,points):
        
        conlist = []
        conlist.append(self.old1c)
        conlist.append(self.old2c)
        
        pList = []
        pList.append(self.old1)
        pList.append(self.old2)
        
        newcon = []
        
        for k in range(len(conlist)):
            for i in conlist[k]:
                for j in i:
                    p = pList[k][j]
                    newcon.append(points.index(p))
        
        newcon = list(self.chopper(newcon,4))
        return newcon
    
    # Checks if the connection is supposed to exist or if it was added by mistake
    def cleanConnects(self,cons):
        connection  = ()
        for item in cons:
            point1  = self.points[item[0]]
            point2  = self.points[item[1]]
            point3  = self.points[item[2]]
            point4  = self.points[item[3]]
            cond1   = False
            cond2   = False
            exists  = False
            
            if point1 in self.old1:
                if point2 in self.old1:
                    if point3 in self.old1:
                        if point4 in self.old1:
                            cond1 = True
            if point1 in self.old2:
                if point2 in self.old2:
                    if point3 in self.old2:
                        if point4 in self.old2:
                            cond2 = True
            
            if (cond1 == True or cond2 == True):
                connection = connection + item
        return connection
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    # Overloading of addition
    def __add__(self,other):
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect)
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out

class rectangle:
    # Overload init with empty shape
    
    # Constructor of a rectangle (or possibly square)
    def __init__(self,xMin,yMin,xLeng,yLeng,npx,npy):
        print 'Shape: Creating a rectangle...'
        
        self.dx     = xLeng / (npx-1)
        self.dy     = yLeng / (npy-1)
        self.xSize  = xMin + xLeng + self.dx*0.1
        self.ySize  = yMin + yLeng + self.dy*0.1
        self.totP   = npx * npy
        self.npx    = npx
        self.npy    = npy
        
        self.x      = ar.arange(xMin,self.xSize,self.dx)
        self.y      = ar.arange(yMin,self.ySize,self.dy)
        
        self.points = [(xp,yp,zp) for yp in self.y for xp in self.x for zp in [0.0]]
        self.connect= self.connections(self.points)
        
    # Overloading of the addition operator
    def __add__(self,other):
        temp        = self.points + other.points            #add the lists
        del_points  = list(set(temp))                       #delete duplicates
        new_points  = self.sort(del_points)                 #sort the new list
        
        new_totP    = len(new_points)                       #calculate the new number of total points
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect)
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    # Calculates element connections
    def connections(self,points):
        connection = ()
        for i in range(0,len(points)-1):
            here    = points[i]
            next    = points[i+1]
            #see if the next point is the right point
            if (next[0] > here[0]) and (next[1] == here[1]):
                #print i
                for j in range(i+1,len(points)-1):
                    other   = points[j]
                    if (other[0] == here[0]):
                        #print (i,i+1,j+1,j)
                        
                        temp        = (i,i+1,j+1,j)
                        connection  = connection + temp
                        break
        conList     = list(self.chopper(connection,4))
        return conList

# A square is actually a rectangle
def square(xMin,yMin,leng,npx,npy):
        out = rectangle(xMin,yMin,leng,leng,npx,npy)
        return out


############## Global Dictionaries
# this needs to be fixed
#elemType = {    'rectangle' :   rectangle,      \
#                'square'    :   square,         \
#                'unstruct'  :   unstructShape   }
#

############## Program loop (for testing)
# put a conditional statement to run this or not
a = [0,0,0,0,0]

# we create a backward facing step
a[0] = rectangle(0.0,0.0,4.0,1.0,5,2)
a[1] = rectangle(0.0,1.0,1.0,3.0,2,4)
a[2] = rectangle(3.0,1.0,1.0,3.0,2,4)
a[3] = rectangle(1.0,3.0,2.0,1.0,3,2)


z = a[0] + a[1] + a[2] + a[3]

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( z.points, quad=z.connect))
vtk.tofile('newTest')