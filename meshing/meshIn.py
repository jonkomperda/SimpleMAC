import sys
import re
import numpy as ar
import operator
import pyvtk

############## Begin shapes
class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self,points, old1='none', old2='none'):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        
        if( old1 == 'none' and old2 == 'none' ):
            self.old1 = 0
            self.old2 = 0
        else:
            self.old1 = old1
            self.old2 = old2
        
        self.connect= self.connections(self.points)
    
    # Finds the element connections (*Needs to be improved, issues with gaps)
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
                    rightO  = points[j+1]
                    if ((other[0] == here[0]) and (rightO[0] > here[0]) and (rightO[1] > here[1])):
                        temp        = (i,i+1,j+1,j)
                        connection  = connection + temp
                        break
        #conList     = list(self.chopper(connection,4))
        
        # This condition is if we are making the shape for the first time
        if(self.old1 == 0 and self.old2 == 0):
            conList     = list(self.chopper(connection,4))
            return conList
        # This condition is if we have added two shapes and need to check for messed up connections
        else:
            conList     = list(self.chopper(connection,4))
            conList = self.cleanConnects(conList)
            conList     = list(self.chopper(conList,4))
            return conList
    
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
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points)
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
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points)
        return out
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    # Calculates element connections (*needs to be improved to better handle gaps)
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

############## Begin data handling
class readMesh:
    """
    This class reads a mesh file and stores the data
    """
#    self.elemType   =   {   'rectangle' :   self.readRectangle
#                            'square'    :   self.readSquare
#                            'unstruct'  :   self.readUnstruct
#                        }
    
    def __init__(self,theFile):
        self.checkHeader(theFile)
        self.blocks     = self.findBlocks(theFile)
        
#        for n in range(0,self.blocks):
#            status[n]  = self.elemType[]
    
    # Read the first line for a file header
    def checkHeader(self,theFile):
        line = theFile.readline()
        if "JonMesh" not in line:
            sys.exit("Error: Incorrect mesh header.")
        else:
            print 'File format: ' + 'JonMesh'
            print 'Version    : ' + str(re.findall(r'\d.\d+',line))
        return
    
    # Get number of blocks from file being read
    def findBlocks(self,theFile):
        for line in theFile:
            if not line.strip():
                continue
            elif "!" in line:
                continue
            elif "blocks" in line:
                nBlock = self.getLineValInt(line)
                print ('Blocks:     File is expected to contain '+ str(nBlock) + ' blocks...')
                return nBlock
            else:
                sys.exit("Error: Blocks not defined in file!")
    
    # Gets the shapes from the file
    def getShape(self,theFile):
        for line in theFile:
            if not line.strip():
                continue
            elif "!" in line:
                continue
            else:
                struct = self.elemType[line.strip]()
                return struct
    
    # Returns a stripped value as a string
    def getLineVal(self,line):
        temp = line.partition('=')[2]
        value = temp.strip().rstrip().lstrip()
        return value
    
    # Returns a stripped int value from the line
    def getLineValInt(self,line):
        value = self.getLineVal(line)
        return int(value)
    
    # Returns a stripped float value from the line
    def getLineValDP(self,line):
        value = self.getLineVal(line)
        return float(value)
    
    # Tells you there's an error in the file and prints the line
    def lineError(line):
        print "\n\nError in line: "+line

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
a[0] = rectangle(0.0,1.0,2.0,1.0,41,21)
a[1] = rectangle(0.0,2.0,2.0,1.0,41,21)
a[2] = rectangle(2.0,0.0,6.0,1.0,123,21)
a[3] = rectangle(2.0,1.0,6.0,2.0,123,41)

z = a[0] + a[1] + a[2] + a[3]

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( z.points, quad=z.connect))
vtk.tofile('newTest')