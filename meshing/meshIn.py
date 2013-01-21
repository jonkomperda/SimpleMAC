import sys
import re
import numpy as ar
import operator
import pyvtk
import math
import simplemesh

############## Begin shapes

class unstructShape:
    # Constructs an unstructured shape, usually the result of adding together other shapes
    def __init__(self, points, old1='none', old2='none', old1c = 'none', old2c = 'none', old1bc = 'none', old2bc = 'none', old1npx = 'none',old2npx = 'none',old1npy = 'none',old2npy = 'none',):
        print 'Shape: Creating an unstructured shape...'
        
        self.points = points
        
        if( old1 == 'none' and old2 == 'none' ):
            self.old1 = 0
            self.old2 = 0
            self.old1c = 0
            self.old2c = 0
            self.old1bc = 0
            self.old2bc = 0
            self.old1npx = 0
            self.old2npx = 0
            self.old1npy = 0
            self.old2npy = 0
            
        else:
            self.old1 = old1
            self.old2 = old2
            self.old1c = old1c
            self.old2c = old2c
            self.old1bc = old1bc
            self.old2bc = old2bc
            self.old1npx = old1npx
            self.old2npx = old2npx
            self.old1npy = old1npy
            self.old2npy = old2npy
        
        self.connect= self.connections(self.points)
        
        self.no_elements = len(self.connect)
        
        self.bc = self.boundary()
        
        
    
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
        
        temp = []
        temp = self.sort(newcon)
        conn = []
        
        #converts list of lists into list of tuples
        for i in range(len(newcon)):
            conn.append((temp[i][0],temp[i][1],temp[i][2],temp[i][3]))
        
        return conn
    
    
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
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect, old1bc = self.bc, old2bc = other.bc)
        return out
    
    
    # Sorts a list of tuples (like our grid points)
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    
    # Sorts the elements checking the face they refer to
    def sortelements(self,val):
        out     = sorted(val, key=operator.itemgetter(1,0,2))
        return  out
    
    
    def boundary(self):
        """it adds boundary conditions of different elements together, erases those with 0, updates elements numbers"""
        
        bclist = []
        bclist.append(self.old1bc)
        bclist.append(self.old2bc)
        
        cList = []
        cList.append(self.old1c)
        cList.append(self.old2c)
        
        pList = []
        pList.append(self.old1)
        pList.append(self.old2)
        
        newelbc = []
        newpoint = []
        
        for k in range(len(bclist)):
            for i in bclist[k]:
                el_i = cList[k][i[0]-1]
                p0 = self.points.index(pList[k][el_i[0]])
                p1 = self.points.index(pList[k][el_i[1]])
                p2 = self.points.index(pList[k][el_i[2]])
                p3 = self.points.index(pList[k][el_i[3]])
                newpoint.append((p0,p1,p2,p3))
        
        for i in range(len(newpoint)):
            new_el = self.connect.index(newpoint[i])
            newelbc.append((new_el+1))
        
        #it updates the boundary conditions list
        tempbc = []
        
        for j in range(len(self.old1bc)):
            tempbc.append(self.old1bc[j])
        for j in range(len(self.old2bc)):
            tempbc.append(self.old2bc[j])
        
        newbc = []
        
        for i in range(len(tempbc)):
            newbc.append((newelbc[i],tempbc[i][1],tempbc[i][2]))
        
        newbc = self.sortelements(newbc)
        
        bc = []
        
        for i in range(len(newbc)):
            if newbc[i][2] != 0:
                bc.append(newbc[i])
        return bc
    
    


class rectangle:
    # Overload init with empty shape
    
    # Constructor of a rectangle (or possibly square)
    def __init__(self,xMin,yMin,xLeng,yLeng,npx,npy,(f1,f4,f2,f6),xfirst_leng,xlast_leng,yfirst_leng,ylast_leng):
        print 'Shape: Creating a rectangle...'
        
        self.dx     = xLeng / (npx-1)
        self.dy     = yLeng / (npy-1)
        self.xSize  = xMin + xLeng + self.dx*0.1
        self.ySize  = yMin + yLeng + self.dy*0.1
        self.totP   = npx * npy
        self.xLeng  = xLeng
        self.yLeng  = yLeng
        self.npx    = npx
        self.npy    = npy
        self.xfirst_leng = xfirst_leng
        self.yfirst_leng = yfirst_leng
        self.xlast_leng = xlast_leng
        self.ylast_leng = ylast_leng
        
        self.face1 = f1
        self.face4 = f4
        self.face2 = f2
        self.face6 = f6
        
        
        if(self.xfirst_leng==0 and self.xlast_leng==0):
            self.x      = ar.arange(xMin,self.xSize,self.dx)
        elif(self.xfirst_leng!=0 and self.xlast_leng!=0):
            print 'Error: just specify one length along the x axis'
        elif(xfirst_leng > 0):
            self.x = self.first_power(self.xLeng,self.xfirst_leng,self.npx,xMin)
        elif(xlast_leng > 0):
            self.x = self.last_power(self.xLeng,self.xlast_leng,self.npx,xMin)
        
        
        if(self.yfirst_leng==0 and self.ylast_leng==0):
            self.y      = ar.arange(yMin,self.ySize,self.dy)
        elif(self.yfirst_leng!=0 and self.ylast_leng!=0):
            print 'Error: just specify one length along the x axis'
        elif(yfirst_leng > 0):
            self.y = self.first_power(self.yLeng,self.yfirst_leng,self.npy,yMin)
        elif(ylast_leng > 0):
            self.y = self.last_power(self.yLeng,self.ylast_leng,self.npy,yMin)
            
        
        self.points = [(xp,yp,zp) for yp in self.y for xp in self.x for zp in [0.0]]
        self.connect= self.connections(self.points)
        
        self.no_elements = len(self.connect)
        
        self.bc_sides()
        
    
    
    # Overloading of the addition operator
    def __add__(self,other):
        temp        = self.points + other.points            #add the lists
        del_points  = list(set(temp))                       #delete duplicates
        new_points  = self.sort(del_points)                 #sort the new list
        
        new_totP    = len(new_points)                       #calculate the new number of total points
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect, old1bc = self.bc, old2bc = other.bc)
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
    
    
    def bc_sides(self):
        """it calculates boundary conditions for the shape - ELEMENTS ALREADY START FROM 1, NOT FROM 0"""
        
        self.bc = []
        
        #bottom side (1)
        for i in range(self.npx-1):
            self.bc.append((i+1,1,self.face1))
            
        #right side (4)
        for i in range(self.npy-1):
            self.bc.append(((self.npx-1)*(i+1),4,self.face4))
            
        #top side (2)
        for i in range(self.npx-1):
            self.bc.append(((self.npy-2)*(self.npx-1)+i+1,2,self.face2))
            
        #left side (6)
        for i in range(self.npy-1):
            self.bc.append(((i+1)+i*(self.npx-2),6,self.face6))
    
    
    def first_power(self,leng,first,np,min):
        """it calculates a non-uniform grid using the power distribution - specified FIRST LENGTH"""
        
        pow = math.log((first)/leng)
        b = 1.0/((np-1)*1.0)
        pow = pow/math.log(b)
        
        coord = []
        dec_place = 7
        
        for i in range(0,np):
            f = i*1.0/(np-1)*1.0
            g = f**pow
            pos = g*leng + min
            pos_rounded = round(pos, dec_place)
            coord.append(pos_rounded)
            #print str(i) + '  ' +str(pos_rounded)
            
        return coord
    
    
    def last_power(self,leng,last,np,min):
        """it calculates a non-uniform grid using the power distribution - specified LAST LENGTH"""
        
        pow = math.log((last)/leng)
        b = 1.0/((np-1)*1.0)
        pow = pow/math.log(b)
        
        coord = []
        dec_place = 7
        
        for i in range(0,np):
            f = i*1.0/(np-1)*1.0
            g = f**pow
            pos = (1-g)*leng + min
            pos_rounded = round(pos, dec_place)
            coord.append(pos_rounded)
            #print str(i) + '  ' +str(pos_rounded)
        
        temp = []
        for i in range(len(coord)):
            temp.append(coord[len(coord)-(i+1)])
        
        coord = temp
        
        return coord
    
    
    def first_cosine(self,leng,first,np,min):
        """it calculates a non-uniform grid using the cosine distribution - specified FIRST LENGTH"""
        
        pow = math.log((last)/leng)
        b = 1.0/((np-1)*1.0)
        pow = pow/math.log(b)
        
        coord = []
        dec_place = 7
        
        for i in range(0,np):
            f = i*1.0/(np-1)*1.0
            g = f**pow
            pos = (1-g)*leng + min
            pos_rounded = round(pos, dec_place)
            coord.append(pos_rounded)
            #print str(i) + '  ' +str(pos_rounded)
        
        temp = []
        for i in range(len(coord)):
            temp.append(coord[len(coord)-(i+1)])
        
        coord = temp
        
        return coord
        
        
# A square is actually a rectangle
def square(xMin,yMin,leng,npx,npy,(f1,f4,f2,f6)):
        out = rectangle(xMin,yMin,leng,leng,npx,npy,(f1,f4,f2,f6))
        return out


class rampquad():
    """creates a quad with a ramp on the right side"""
    def __init__(self, p1, p2, p3, p4, npx, npy, (f1,f4,f2,f6)):
        print 'Shape: Creating a rampquad...'
        
        if(p1[0]==p3[0]):
            self.p1 = p1
            self.p2 = p2
            self.p3 = p3
            self.p4 = p4
            self.face1 = f1
            self.face4 = f4
            self.face2 = f2
            self.face6 = f6
        else:
            self.p1 = p2
            self.p2 = p1
            self.p3 = p4
            self.p4 = p3
            self.face1 = f2
            self.face2 = f1
            self.face3 = f4
            self.face4 = f3
        
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
        
        self.no_elements = len(self.connect)
        
        self.bc_sides()
    
    
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
    
    
    def bc_sides(self):
        """it calculates boundary conditions for the shape - ELEMENTS ALREADY START FROM 1, NOT FROM 0"""
        
        self.bc = []
        
        #bottom side (1)
        for i in range(self.npx-1):
            self.bc.append((i+1,1,self.face1))
            
        #right side (4)
        for i in range(self.npy-1):
            self.bc.append(((self.npx-1)*(i+1),4,self.face4))
            
        #top side (2)
        for i in range(self.npx-1):
            self.bc.append(((self.npy-2)*(self.npx-1)+i+1,2,self.face2))
        
        #left side (6)
        for i in range(self.npy-1):
            self.bc.append(((i+1)+i*(self.npx-2),6,self.face6))
    
    
    def __add__(self,other):                                #overloading of addition
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect, old1bc = self.bc, old2bc = other.bc)
        return out
    
    
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    


class genshape():
    """creates a generic quad where you specifiy just the corners"""
    def __init__(self, p1, p2, p3, p4, npx, npy, (f1,f4,f2,f6),x1first_leng,x1last_leng,y1first_leng,y1last_leng,x2first_leng,x2last_leng,y2first_leng,y2last_leng):
        print 'Shape: Creating a generic shape...'
        
        self.p1 = (p1[0],p1[1],0.0)
        self.p2 = (p2[0],p2[1],0.0)
        self.p3 = (p3[0],p3[1],0.0)
        self.p4 = (p4[0],p4[1],0.0)
        
        self.npx = npx
        self.npy = npy
        
        self.face1 = f1
        self.face4 = f4
        self.face2 = f2
        self.face6 = f6
        
        self.x1first_leng = x1first_leng
        self.y1first_leng = y1first_leng
        self.x1last_leng = x1last_leng
        self.y1last_leng = y1last_leng
        self.x2first_leng = x2first_leng
        self.y2first_leng = y2first_leng
        self.x2last_leng = x2last_leng
        self.y2last_leng = y2last_leng
                
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
        
        self.no_elements = len(self.connect)
        
        self.bc_sides()
    
    
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
        return conList
    
    
    def bc_sides(self):
        """it calculates boundary conditions for the shape - ELEMENTS ALREADY START FROM 1, NOT FROM 0"""
        
        self.bc = []
        
        #bottom side (1)
        for i in range(self.npx-1):
            self.bc.append((i+1,1,self.face1))
        
        #right side (4)
        for i in range(self.npy-1):
            self.bc.append(((self.npx-1)*(i+1),4,self.face4))
        
        #top side (2)
        for i in range(self.npx-1):
            self.bc.append(((self.npy-2)*(self.npx-1)+i+1,2,self.face2))
        
        #left side (6)
        for i in range(self.npy-1):
            self.bc.append(((i+1)+i*(self.npx-2),6,self.face6))    
    
    
    def __add__(self,other):                                #overloading of addition
        temp        = self.points + other.points
        del_points  = list(set(temp))
        new_points  = self.sort(del_points)
        
        out         = unstructShape(new_points, old1 = self.points, old2 = other.points, old1c = self.connect, old2c = other.connect, old1bc = self.bc, old2bc = other.bc)
        return out
    
    
    def sort(self,val):
        out     = sorted(val, key=operator.itemgetter(2,1,0))
        return  out
    
    


class gsnotunif():
    """it creates a generic shape with not uniform distribution of points"""
    
    def __init__(self,p1,p2,p3,p4,npx,npy,(f1,f4,f2,f6),x1first,x1last,y1first,y1last,x2first,x2last,y2first,y2last):
        
        self.p1 = (p1[0],p1[1],0.0)
        self.p2 = (p2[0],p2[1],0.0)
        self.p3 = (p3[0],p3[1],0.0)
        self.p4 = (p4[0],p4[1],0.0)
        
        self.npx = npx
        self.npy = npy
        
        self.face1 = f1
        self.face4 = f4
        self.face2 = f2
        self.face6 = f6
        
        self.x1first = x1first
        self.x1last = x1last
        self.y1first = y1first
        self.y1last = y1last
        self.x2first = x2first
        self.x2last = x2last
        self.y2first = y2first
        self.y2last = y2last
        
        #self.y1pts = self.vpts_last(self.p1,self.p3,self.y1last,self.npy)
        #self.y2pts = self.vpts_adapt(self.p2,self.p4,self.y1pts,self.npy)
        self.x1pts = self.hpts_last(self.p1,self.p2,self.x1first,self.npx)
        #self.x2pts = self.hpts_adapt(self.p3,self.p4,self.x1pts,self.npx)
        #print self.y1pts
        #print self.y2pts
        print self.x1pts
        #print self.x2pts
    
    
    def vpts_first(self,startpoint,finishpoint,yfirst,npy):
        """calculates the coordinates of the points on the two vertical sides (y sides)"""
        
        ylength = finishpoint[1]-startpoint[1]
        
        vpts = []
        vpts.append(startpoint)
        dec_place = 7
        beta = self.calcbeta(startpoint,finishpoint)
        
        ycoord = self.first_power(ylength,yfirst,npy,startpoint[1])
        
        for i in range(1,npy-1):
            x_next = startpoint[0]+(ycoord[i]-startpoint[1])*math.tan(beta)
            y_next = ycoord[i]
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            vpts.append(nextpoint)
        
        vpts.append(finishpoint)
        
        return(vpts)
    
    
    def vpts_last(self,startpoint,finishpoint,ylast,npy):
        """calculates the coordinates of the points on the two vertical sides (y sides) - last coordinates specified"""
        
        ylength = finishpoint[1]-startpoint[1]
        
        vpts = []
        vpts.append(startpoint)
        dec_place = 7
        beta = self.calcbeta(startpoint,finishpoint)
        
        ycoord = self.last_power(ylength,ylast,npy,startpoint[1])
        
        for i in range(1,npy-1):
            x_next = startpoint[0]+(ycoord[i]-startpoint[1])*math.tan(beta)
            y_next = ycoord[i]
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            vpts.append(nextpoint)
        
        vpts.append(finishpoint)
        
        return(vpts)
    
    
    def vpts_adapt(self,startpoint,finishpoint,ypts,npy):
        """it calculates the points on a vertical side without any coordinate distribution preferred"""
        
        ylength = finishpoint[1]-startpoint[1]
        
        vpts = []
        vpts.append(startpoint)
        dec_place = 7
        beta = self.calcbeta(startpoint,finishpoint)
        
        for i in range(1,npy-1):
            ydistribution = (ypts[i][1]-ypts[0][1])/(ypts[npy-1][1]-ypts[0][1])
            y_next = startpoint[1] + ydistribution*(finishpoint[1]-startpoint[1])
            x_next = startpoint[0] + y_next*math.tan(beta)
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            vpts.append(nextpoint)
        
        vpts.append(finishpoint)
        
        return(vpts)
    
    
    def hpts_first(self,startpoint,finishpoint,xfirst,npx):
        """calculates the coordinates of the points on the horizontal sides"""
        
        xlength = finishpoint[0]-startpoint[0]
        
        hpts = []
        hpts.append(startpoint)
        dec_place = 7
        alpha = self.calcalpha(startpoint,finishpoint)
        
        xcoord = self.first_power(xlength,xfirst,npx,startpoint[0])
        
        for i in range(1,npx-1):
            x_next = xcoord[i]
            y_next = startpoint[1]+(xcoord[i]-startpoint[1])*math.tan(alpha)
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            hpts.append(nextpoint)
        
        hpts.append(finishpoint)
        
        return(hpts)
        
    
    
    def hpts_last(self,startpoint,finishpoint,xlast,npx):
        """calculates the coordinates of the points on the two horizontal sides (x sides) - last coordinates specified"""
        
        xlength = finishpoint[0]-startpoint[0]
        
        hpts = []
        hpts.append(startpoint)
        dec_place = 7
        alpha = self.calcalpha(startpoint,finishpoint)
        
        xcoord = self.last_power(xlength,xlast,npx,startpoint[0])
        
        for i in range(1,npx-1):
            x_next = xcoord[i]
            y_next = startpoint[1]+(xcoord[i]-startpoint[0])*math.tan(alpha)
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            hpts.append(nextpoint)
        
        hpts.append(finishpoint)
        
        return(hpts)
    
    
    def hpts_adapt(self,startpoint,finishpoint,xpts,npx):
        """it calculates the points on a horizontal side without any coordinate distribution preferred"""
        
        xlength = finishpoint[0]-startpoint[0]
        
        hpts = []
        hpts.append(startpoint)
        dec_place = 7
        alpha = self.calcalpha(startpoint,finishpoint)
        
        for i in range(1,npx-1):
            xdistribution = (xpts[i][0]-xpts[0][0])/(xpts[npx-1][0]-xpts[0][0])
            x_next = startpoint[0] + xdistribution*(finishpoint[0]-startpoint[0])
            y_next = startpoint[1] + x_next*math.tan(alpha)
            nextpoint = (round(x_next , dec_place) , round(y_next , dec_place), 0.0)
            hpts.append(nextpoint)
        
        hpts.append(finishpoint)
        
        return(hpts)
    
    
    def calcalpha(self,A,B):
        """it calculates the angle among two vertices with respect to x axis"""
        alpha = math.atan((B[1]-A[1])/(B[0]-A[0]))
        return alpha
    
    
    def calcbeta(self,A,B):
        """it calculates the angle among two vertices with respect to y axis"""
        beta = math.atan((B[0]-A[0])/(B[1]-A[1]))
        return beta
    
    
    def first_power(self,leng,first,np,min):
        """it calculates a non-uniform grid using the power distribution - specified FIRST dx or dy"""
        
        pow = math.log((first)/leng)
        b = 1.0/((np-1)*1.0)
        pow = pow/math.log(b)
        
        coord = []
        dec_place = 7
        
        for i in range(0,np):
            f = i*1.0/(np-1)*1.0
            g = f**pow
            pos = g*leng + min
            pos_rounded = round(pos, dec_place)
            coord.append(pos_rounded)
            #print str(i) + '  ' +str(pos_rounded)
        
        return coord
        
    
    
    def last_power(self,leng,last,np,min):
        """it calculates a non-uniform grid using the power distribution - specified LAST dx or dy"""
        
        pow = math.log((last)/leng)
        b = 1.0/((np-1)*1.0)
        pow = pow/math.log(b)
        
        coord = []
        dec_place = 7
        
        for i in range(0,np):
            f = i*1.0/(np-1)*1.0
            g = f**pow
            pos = (1-g)*leng + min
            pos_rounded = round(pos, dec_place)
            coord.append(pos_rounded)
            #print str(i) + '  ' +str(pos_rounded)
            
        temp = []
        for i in range(len(coord)):
            temp.append(coord[len(coord)-(i+1)])
            
        coord = temp
        
        return coord
        
        


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


if __name__ == '__main__':
    
    #s1 = rectangle(-5.0,1.0,5.0,10.76,9,7,(3,0,2,1),0.0,0.5,0.2,0.0)
    #s2 = rectangle(0.0,1.0,17.92,10.76,18,7,(0,2,2,0),0.5,0.0,0.2,0.0)
    #s3 = rectangle(0.0,0.0,17.92,1.0,18,7,(3,2,0,3),0.5,0.0,0.0,0.2)
    s1 = rectangle(0.0,0.0,4.0,4.0,5,5,(3,2,0,3),0.5,0.0,0.0,0.5)
    
    #s = s1 + s2 + s3
    
    s = gsnotunif((0.0,0.0),(4.0,0.0),(-2.0,4.0),(6.0,4.0),5,5,(2,2,2,2),0.5,0.5,0.5,0.5,0.0,0.0,0.0,0.0)
    
    
    #vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s.points, quad=s.connect))
    #vtk.tofile('test')
    

