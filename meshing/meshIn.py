import sys
import re
import numpy as ar
import operator
import pyvtk
import math

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


class rampquad():
    """creates a quad with a ramp on the right side"""
    def __init__(self, p1, p2, p3, p4, npx, npy):
        print 'Shape: Creating a rampquad...'
        
        if(p1[0]==p3[0]):
            self.p1 = p1
            self.p2 = p2
            self.p3 = p3
            self.p4 = p4
        else:
            self.p1 = p2
            self.p2 = p1
            self.p3 = p4
            self.p4 = p3
        
        self.npx = npx
        self.npy = npy
        
        self.theta = self.calctheta()
        self.dx = self.lengths()[0]
        self.dy = self.lengths()[1]
        
        self.xSize = self.p1[0] + self.l2 + 0.1*self.dx
        self.ySize = self.p1[0] + self.h + 0.1*self.dy
        
        self.x = ar.arange(self.p1[0],self.xSize,self.dx)       # x coordinates of top points
        self.y = ar.arange(self.p1[0],self.ySize,self.dy)       # y coordinates of left points
        
        #print 'theta: ' + str(self.theta)
        #print 'dx: ' + str(self.dx) + '  dy: '+ str(self.dy)
        #print 'self.x:  ' + str(self.x)
        #print 'self.y:  ' + str(self.y)
        #print
        
        self.coordinates()
        
        l = len(self.points)
        
        self.connect = self.connections(self.points)
    
    
    def calctheta(self):
        """it calculates the angle of the ramp"""
        
        theta = math.atan((self.p4[0]-self.p2[0])/(self.p4[1]-self.p2[1]))
        
        return theta
    
    
    def lengths(self):
        """it calculates the length of the increments in x and y directions"""
        
        self.l1 = self.p2[0]-self.p1[0]
        self.l2 = self.p4[0]-self.p3[0]
        self.h = self.p3[1]-self.p1[1]
        self.l0 = self.l1/math.tan(self.theta)
        self.r0 = self.l1/math.sin(self.theta)
        self.Lv = self.h + self.l0
        
        self.p0 = (self.p1[0] , self.p1[1]-self.l0 , 0.0)
        
        dx = self.l2/(self.npx-1)
        dy = self.h/(self.npy-1)
        
        return (dx,dy)
    
    
    def coordinates(self):
        #find the x coordinates for the points at various y in the mesh and enlists them
        
        j = 0
        self.points = []
        dec_place = 7
        
        for yp in self.y:       
            i = 0                                       
            for xp in self.x:           
                L_v = self.l0 + self.dy * j                                 #length of the left side of the j-th triangle
                x_ij = self.p1[0] + L_v * i * self.dx / self.Lv         #x coordinate of top right corner of j-th triangle
                x_ij_rounded = round(x_ij, dec_place)
                y_ij = self.p1[1] + self.dy * j                         #y coordinate of top left corner of i-th triangle
                self.points.append((x_ij_rounded,y_ij,0.0))                     #prints the list of points created
                i=i+1
            j=j+1
        
        self.points = self.sort(self.points)                            #needed because points would be generated from right to left for left-ramped quad
        #print 'list of points: ' + str(self.points)        
    
    
    def chopper(self,list,size):                            # Chops a list into 'size' tuples
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    
    def connections(self,points):                           # Calculates element connections (*needs to be improved to better handle gaps)
        connection = ()
        i = 0
        j = 0
        error = 0.00001                                     #I NEED TO TAKE INTO ACCOUNT ERROR OF THE MACHINE
        for i in range(0,len(points)-1):
            here    = points[i]
            next    = points[i+1]
            #see if the next point is the right point
            if (next[0] > here[0]) and (next[1] == here[1]):
                #print i
                for j in range(i+1,len(points)-1):
                    other   = points[j]
                    tan_other = (other[0] - self.p1[0])/(other[1] - self.p0[1])
                    tan_here = (here[0] - self.p1[0])/(here[1] - self.p0[1])
                    if (tan_other < (tan_here + error)) and (tan_other > (tan_here - error)):
                        #print (i,i+1,j+1,j)
                        temp        = (i,i+1,j+1,j)
                        connection  = connection + temp
                        break
        conList     = list(self.chopper(connection,4))
        #print
        #print conList
        #print
        return conList
    
    
    def __add__(self,other):                                #overloading of addition
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect)
        return out
    
    
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    


class genshape():
    """creates a generic quad where you specifiy just the corners"""
    def __init__(self, p1, p2, p3, p4, npx, npy):
        print 'Shape: Creating a generic shape...'
        
        self.p1 = (p1[0],p1[1],0.0)
        self.p2 = (p2[0],p2[1],0.0)
        self.p3 = (p3[0],p3[1],0.0)
        self.p4 = (p4[0],p4[1],0.0)
        
        self.npx = npx
        self.npy = npy
        
        self.a = (((p1[0]-p2[0])**2)+((p1[1]-p2[1])**2))**0.5
        self.b = (((p3[0]-p4[0])**2)+((p3[1]-p4[1])**2))**0.5
        self.l1 = (((p1[0]-p3[0])**2)+((p1[1]-p3[1])**2))**0.5
        self.l2 = (((p4[0]-p2[0])**2)+((p4[1]-p2[1])**2))**0.5
        
        self.da = self.a/(npx-1)
        self.db = self.b/(npx-1)
        self.dl1 = self.l1/(npy-1)
        self.dl2 = self.l2/(npy-1)
        
        self.l1_pts = self.ver_pts(self.p1,self.p3,self.dl1,self.npy)
        self.l2_pts = self.ver_pts(self.p2,self.p4,self.dl2,self.npy)
        #self.a_pts = self.hor_pts(self.p1,self.p2,self.da,self.npx)
        #self.b_pts = self.hor_pts(self.p3,self.p4,self.db,self.npx)
        
        self.points = []
        
        for k in range(npy):
            startpt = ()
            finalpt = ()
            startpt = self.l1_pts[k]
            finalpt = self.l2_pts[k]
            
            length = (((startpt[0]-finalpt[0])**2)+((startpt[1]-finalpt[1])**2))**0.5
            dl = length/(self.npx-1)
            
            for z in range(npx):
                row = self.hor_pts(startpt,finalpt,dl,self.npx)
                self.points.append(row[z])
            
        
        self.connect = self.connections(self.points)
    
    def ver_pts(self,startpoint,finishpoint,increment,np):
        """calculates points on the vertical sides of the shape"""
        
        verpts = []
        verpts.append(startpoint)
        dec_place = 7
        i = 0
        beta = self.calcbeta(startpoint,finishpoint)
        dx = increment * math.cos(beta)
        dy = increment * math.sin(beta)
        
        for i in range (0,np-2):
            x_next = verpts[i][0] + increment * math.sin(beta)
            y_next = verpts[i][1] + increment * math.cos(beta)
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            verpts.append(nextpoint)
            
        verpts.append(finishpoint)
        return(verpts)
    
    
    def hor_pts(self,startpoint,finishpoint,increment,np):
        """calculates points on the horizontal lines of the shape"""
        
        horpts = []
        horpts.append(startpoint)
        dec_place = 7
        i = 0
        alpha = self.calcalpha(startpoint,finishpoint)
        dx = increment * math.cos(alpha)
        dy = increment * math.sin(alpha)
        
        for i in range (0,np-2):
            x_next = horpts[i][0] + dx
            y_next = horpts[i][1] + dy
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            horpts.append(nextpoint)
        
        horpts.append(finishpoint)
        return(horpts)
    
    
    def calcalpha(self,A,B):
        """it calculates the angle among two vertices with respect to x axis"""
        alpha = math.atan((B[1]-A[1])/(B[0]-A[0]))
        return alpha
    
    
    def calcbeta(self,A,B):
        """it calculates the angle among two vertices with respect to y axis"""
        beta = math.atan((B[0]-A[0])/(B[1]-A[1]))
        return beta
    
    
    def chopper(self,list,size):                            # Chops a list into 'size' tuples
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    
    def connections(self,points):                           # Calculates element connections (*needs to be improved to better handle gaps)
        conList = []
        i = 0
        j = 0
        k = -1
        for i in range(0,self.npy-1):
            here    = points[i]
            next    = points[i+1]
            k = k+1
            for j in range(0,self.npx-1):
                print str(i) + '  ' + str(k) + '  ' + str(j)
                conList.append((k,k+1,k+self.npx+1,k+self.npx))
                #connection.append(k)
                #connection.append(k+1)
                #connection.append(k+self.npx+1)
                #connection.append(k+self.npx)
                k=k+1
            """if (next[0] > here[0]):
                #print i
                for j in range(i+1,len(points)-1):
                    other   = points[j]
                    tan_other = (other[0] - self.p1[0])/(other[1] - self.p0[1])
                    tan_here = (here[0] - self.p1[0])/(here[1] - self.p0[1])
                    if (tan_other < (tan_here + error)) and (tan_other > (tan_here - error)):
                        #print (i,i+1,j+1,j)
                        temp        = (i,i+1,j+1,j)
                        connection  = connection + temp
                        break
            for j in range(0,self.npy-1):
                print i
                print j"""
        print conList
        return conList
    
    
    def __add__(self,other):                                #overloading of addition
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect)
        return out
    
    
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    


############## Global Dictionaries
# this needs to be fixed
#elemType = {    'rectangle' :   rectangle,      \
#                'square'    :   square,         \
#                'unstruct'  :   unstructShape   }
#

############## Program loop (for testing)
# put a conditional statement to run this or not
#a = [0,0,0,0,0]

# we create a backward facing step

#s1=rectangle(-4.0,0.0,4.0,4.0,17,17)
#s2=rectangle(0.0,2.0,8.0,2.0,33,9)
s1234=genshape([0.0,0.0],[2.0,0.0],[0.0,2.0],[4.0,2.0],3,5)
#s4=rampquad([0.0,-2.0],[3.0,-2.0],[0.0,0.0],[2.0,0.0],17,9)
#s5=rampquad([5.0,0.0],[8.0,0.0],[4.0,2.0],[8.0,2.0],17,9)
#s6=rampquad([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],9,9)
#s4=rectangle(0.0,8.0,8.0,4.0,5,3)


#s1234 = genshape([0.0,0.0],[2.0,1.0],[-2.0,2.0],[5.0,3.0],31,31)
#s1234 = genshape([0.0,0.0],[1.0,1.0],[-2.0,2.0],[3.0,5.0],21,21)

#s1234 = genshape([-2.0,2.0],[3.0,2.0],[-4.0,4.0],[10.0,7.0],31,31)

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s1234.points, quad=s1234.connect))
vtk.tofile('genshape')
