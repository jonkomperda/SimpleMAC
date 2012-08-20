import sys
import re
import numpy as ar
import operator
import pyvtk

############## Begin shapes
class unstructShape:
    def __init__(self,points):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        self.connect= self.connections(self.points)
    
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
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    def __add__(self,other):
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points)
        
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out

class rectangle:
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
        
        self.points = [(xp,yp,zp) for yp in self.y for xp in self.x for zp in [0]]
        self.connect= self.connections(self.points)
        
    # Overloading of the addition operator
    def __add__(self,other):
        temp        = self.points + other.points            #add the lists
        del_points  = list(set(temp))                       #delete duplicates
        new_points  = self.sort(del_points)                 #sort the new list
        
        new_totP    = len(new_points)                       #calculate the new number of total points
        
        out         = unstructShape(new_points)
        
    #    for i in range(0,len(new_points)-1):
    #        here    = new_points[i]
    #        next    = new_points[i+1]
    #        #see if the next point is the right point
    #        if (next[0] > here[0]) and (next[1] == here[1]):
    #            #print i
    #            for j in range(i+1,len(new_points)-1):
    #                other   = new_points[j]
    #                if (other[0] == here[0]):
    #                    print (i,i+1,j+1,j)
    #                    break
    #            
        
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
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

    # Old Version   
    # Create a list of connected points to create elements
    #def connections(self,points):
    #     ranger      = range(0,len(points))
    #     connection  = ()
    #     
    #     for j in range(0,self.npy-1):
    #         for i in range(0,self.npx-1):
    #             here    = i + (j*self.npx)
    #             right   = here + 1
    #             top     = here + self.npx
    #             topr    = here + self.npx + 1
    #             
    #             temp = (here, right, topr, top)
    #             connection = connection + temp
    #     
    #     conList     = list(self.chopper(connection,4))
    #    return conList

# A square is actually a rectangle
def square(xMin,yMin,leng,npx,npy):
        out = rectangle(xMin,yMin,leng,leng,npx,npy)
        return out

############## Begin data handling
class io:
    
    # Returns a stripped value as a string
    def getLineVal(self,line):
        temp = line.partition('=')[2]
        value = temp.strip().rstrip().lstrip()
        return value
    
    # Returns a stripped int value from the line
    def getLineValInt(line):
        value = getLineVal(line)
        return int(value)

    # Returns a stripped float value from the line
    def getLineValDP(line):
        value = getLineVal(line)
        return float(value)
    
    # Tells you there's an error in the file and prints the line
    def lineError(line):
        print "\n\nError in line: "+line

############## Program loop
a = [0,0,0,0]
a[0] = square(0.0,1.0,1.0,2,2)
#print a[0].x
#print a[0].y
#print a[0].points
print a[0].connect
#print a[0].points

#vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( a[0].points, quad=a[0].connect ))
#vtk.tofile('newTest')

a[1] = square(2.0,0.0,1.0,2,2)
#print a[1].x
#print a[1].y
#print a[1].points
print a[1].connect
#print a[1].points

a[2] = rectangle(1.0,0.0,3.0,2.0,4,3)

#print 'result'
z = a[0] + a[2]
print z.connect

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( z.points, quad=z.connect))
vtk.tofile('newTest')
#structure = pyvtk.UnstructuredGrid([4,3,1],z)