import sys
import re
import numpy as ar
import operator
import pyvtk
import math

class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self, points, old1='none', old2='none'):
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

class rampquad():
	"""creates a quad with a ramp on one side"""
	def __init__(self, p1, p2, p3, p4, npx, npy):
		
		print 'Shape: Creating a rampquad...'
		print
		
		self.p1 = p1
		self.p2 = p2
		self.p3 = p3
		self.p4 = p4
		self.npx = npx
		self.npy = npy
		
		self.theta = self.calctheta()
		self.dx = self.lengths()[0]
		self.dy = self.lengths()[1]
		
		self.xSize = self.p1[0] + self.l2 + 0.1*self.dx
		self.ySize = self.p1[0] + self.h + 0.1*self.dy
		
		self.x = ar.arange(self.p1[0],self.xSize,self.dx)		# x coordinates of top points
		self.y = ar.arange(self.p1[0],self.ySize,self.dy)		# y coordinates of left points
		
		print 'theta: ' + str(self.theta)
		print 'dx: ' + str(self.dx) + '  dy: '+ str(self.dy)
		print 'self.x:  ' + str(self.x)
		print 'self.y:  ' + str(self.y)
		print
		
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
		self.l0 = self.l1*math.tan(self.theta)
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
		
		for yp in self.y:		
			i = 0										
			for xp in self.x:			
				L_v = self.l0 + self.dy * j									#length of the left side of the j-th triangle
				x_ij = self.p1[0] + L_v * i * self.dx / self.Lv			#x coordinate of top right corner of j-th triangle
				y_ij = self.p1[1] + self.dy * j							#y coordinate of top left corner of i-th triangle
				self.points.append((x_ij,y_ij,0.0))						#prints the list of points created
				i=i+1
			j=j+1
		
		#print 'list of points: ' + str(self.points)		
	
	def chopper(self,list,size):							# Chops a list into 'size' tuples
		for i in xrange(0, len(list), size):
			yield list[i:i+size]
	
	def connections(self,points):							# Calculates element connections (*needs to be improved to better handle gaps)
		connection = ()
		i = 0
		j = 0
		error = 0.00001										#I NEED TO TAKE INTO ACCOUNT ERROR OF THE MACHINE
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
		print
		#print 'Connections: ' + str(conList)
		return conList


if __name__ == '__main__':
	a = rampquad([1.0,1.0,0.0],[2.0,1.0,0.0],[1.0,2.0,0.0],[3.0,2.0,0.0],3,3)
	b = rectangle(0.0,2.0,4.0,2.0,5,3)
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( a.points, quad=a.connect))
	vtk.tofile('rampq')
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( b.points, quad=b.connect))
	vtk.tofile('rect')