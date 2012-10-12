import math
import numpy as ar
import pyvtk

class rampquad():
    """creates a quad with a ramp on one side"""
    def __init__(self, p1, p2, p3, p4, npx, npy):
        
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
        #print 'l0: ' + str(self.l0)
        
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
    


if __name__ == '__main__':
#   a = rampquad([1.0,1.0,0.0],[2.0,1.0,0.0],[1.0,2.0,0.0],[3.0,2.0,0.0],41,41)
#   vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( a.points, quad=a.connect))
#   vtk.tofile('lalala')

#   s3=rampquad([0.0,4.0,0.0],[4.0,4.0,0.0],[0.0,8.0,0.0],[8.0,8.0,0.0],5,3)
#   vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s3.points, quad=s3.connect))
#   vtk.tofile('s3rounded')

    test=rampquad([0.0,0.0,0.0],[1.0,0.0,0.0],[0.0,1.0,0.0],[4.0,1.0,0.0],5,3)
    vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( test.points, quad=test.connect))
    vtk.tofile('ramptest')
