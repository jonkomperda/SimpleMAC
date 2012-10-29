import sys
import re
import numpy as ar
import operator
import pyvtk
import math

############## Begin shapes

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
	


class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self,points, connections, old1p='none', old2p='none', old1c='none', old2c='none'):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        self.connections= connections

        if( old1p == 'none' and old2p == 'none' ):
            self.old1p = 0
            self.old2p = 0
			self.old1c = 0
			self.old2c = 0
			
        else:
            self.old1p = old1p
            self.old2p = old2p
			self.old1c = old1c
			self.old2c = old2c
		
	"""reads the connections from the previous connection lists"""
	#conlist = []
	#conlist.append([0,1,3,2])
	#conlist.append([0,1,3,2])

	self.pList = []
	#pList.append([(0,0,0),(1,0,0),(0,1,0),(1,1,0)])
	#pList.append([(1,0,0),(2,0,0),(1,1,0),(2,1,0)])

	self.pList = set(self.old1p + self.old2p)
	self.pList = sort(self.pList)

	print 'self.pList'
	print self.pList

	self.conList = []
	self.conList = self.old1c + self.old2c
		
	newcon = []
		
	for k in range(len(self.conList)):
	    for i in self.conList[k]:
	        p = self.pList[k][i]
	        newcon.append(self.pList.index(p))

	print 'newcon'
	print newcon

	newconlist = []
	self.newconlist = list(self.chopper(newcon,4))

	print 'newconlist'
	print self.newconlist
	
    # Chops a list into 'size' tuples
    def chopper(self,list,size):
        for i in xrange(0, len(list), size):
            yield list[i:i+size]
    
    # Overloading of addition
    def __add__(self,other):
        self.totpoints = self.points + other.points
        self.del_points  = list(set(self.totpoints))
        self.new_points  = self.sort(del_points)
		
		self.totconn = []
		self.totconn = self.connections + other.connections 
        
        out         = unstructShape(self.new_points, self.totconn, old1p = self.points, old2p = other.points, old1c = self.connections, old2c = other.connections)
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



############## Global Dictionaries
# this needs to be fixed
#elemType = {    'rectangle' :   rectangle,      \
#                'square'    :   square,         \
#                'unstruct'  :   unstructShape   }
#

if __name__ == '__main__':
	
#	s1=rectangle(0.0,0.0,4.0,4.0,5,3)
#	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s1.points, quad=s1.connect))
#	vtk.tofile('s1')
	
#	s2=rectangle(-4.0,4.0,4.0,4.0,5,3)
#	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s2.points, quad=s2.connect))
#	vtk.tofile('s2')

#	s3=rampquad([0.0,4.0,0.0],[4.0,4.0,0.0],[0.0,8.0,0.0],[8.0,8.0,0.0],5,3)
#	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s3.points, quad=s3.connect))
#	vtk.tofile('s3')
	
#	s4=rectangle(0.0,8.0,8.0,4.0,5,3)
#	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s4.points, quad=s4.connect))
#	vtk.tofile('s4')

#	s124 = s1 + s2 + s4
#	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s124.points, quad=s124.connect))
#	vtk.tofile('s124')

#CASE A
	aa=rectangle(0.0,0.0,2.0,1.0,3,3)
	ba=rectangle(1.0,1.0,2.0,2.0,3,3)
	ca=aa+ba

	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( ca.points, quad=ca.connect))
	vtk.tofile('casea')

"""
#CASE B
	ab=rectangle(0.0,0.0,7.0,2.0,8,5)
	bb=rectangle(5.0,2.0,5.0,3.0,6,3)
	cb=ab+bb
	
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( cb.points, quad=cb.connect))
	vtk.tofile('caseb')
"""
