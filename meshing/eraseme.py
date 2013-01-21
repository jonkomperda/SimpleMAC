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