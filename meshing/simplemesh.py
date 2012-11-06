import pyvtk
import meshIn


class boundary():
    """it calculates boundary conditions for the shapes given as input - ELEMENTS ALREADY START FROM 1, NOT FROM 0"""
    def __init__(self, points, connect, npx, npy, bottom=0, right=0, top=0, left=0):
        
        self.points = points
        self.connect = connect
        self.npx = npx
        self.npy = npy
        
        self.face1 = bottom
        self.face2 = right
        self.face3 = top
        self.face4 = left
        
        self.no_elements = len(self.connect)
        
        self.bc_sides()
    
    
    def bc_sides(self):
        """it calculates the bc on the bottom face"""
        
        self.bc = []
        
        #bottom side (1)
        for i in range(self.npx-1):
            self.bc.append((i+1,1,self.face1))
        
        #right side (2)
        for i in range(self.npy-1):
            self.bc.append(((self.npx-1)*(i+1),2,self.face2))
        
        #top side (3)
        for i in range(self.npx-1):
            self.bc.append(((self.npy-2)*(self.npx-1)+i+1,3,self.face3))
        
        #left side (4)
        for i in range(self.npy-1):
            self.bc.append(((i+1)+i*(self.npx-2),4,self.face4))
    
    


class depth():
    """it repeats the intial mesh along the z axis, lz=total depth, npz=no. of points along z direction"""
    def __init__(self, points, connect, boundary, lz, npz, front=0, back=0):
        print 'Extruding the shape...'
        
        self.points = points
        self.connect = connect
        self.boundary = boundary
        self.lz = lz
        self.npz = npz
        
        self.face1 = front
        self.face2 = back
        
        self.coordinates()
        self.connections()
        self.bc_faces()
        
        #frontback function is called only for three dimensional cases
        if self.npz > 1:
            self.bc_frontback()
        
    
    
    def coordinates(self):
        """calculates all the new points (repeats the same points for different z coordinates)"""
        
        self.dz = self.lz/(self.npz-1)
        self.z_pos = self.points[0][2]
        
        self.no_points = len(self.points)
        
        for i in range(self.npz-1):
            self.z_pos = self.z_pos + self.dz
            for j in range(self.no_points):
                #print str(i) + '   ' + str(j)
                self.points.append((self.points[j][0],self.points[j][1],self.z_pos))
    
    
    def connections(self):
        """calculates the new connection list adding all the new points"""
        
        self.no_connections = len(self.connect)
        
        layer = -1
        self.tempcon = []
        
        #calculates the connections for the other layers and stores them in tempcon (simply adds the number of points in order to find the new points)
        for i in range(self.npz):
            layer = layer + 1
            for j in range(self.no_connections):
                p0 = self.connect[j][0]+layer*self.no_points
                p1 = self.connect[j][1]+layer*self.no_points
                p2 = self.connect[j][2]+layer*self.no_points
                p3 = self.connect[j][3]+layer*self.no_points
                self.tempcon.append((p0,p1,p2,p3))
                        
        self.newcon = []
        
        #calculates the new tuples, constituted by 8 elements each instead of 4
        for i in range(layer):
            for j in range(self.no_connections):
                #print str(i) + '   ' + str(j)
                p0 = self.tempcon[j][0]+i*self.no_points
                p1 = self.tempcon[j][1]+i*self.no_points
                p2 = self.tempcon[j][2]+i*self.no_points
                p3 = self.tempcon[j][3]+i*self.no_points
                p4 = self.tempcon[j+self.no_connections][0]+i*self.no_points
                p5 = self.tempcon[j+self.no_connections][1]+i*self.no_points
                p6 = self.tempcon[j+self.no_connections][2]+i*self.no_points
                p7 = self.tempcon[j+self.no_connections][3]+i*self.no_points
                
                self.newcon.append((p0,p1,p2,p3,p4,p5,p6,p7))
        
        #write everything back into self.connect, in order to give the same input to simplemesh
        self.connect = []
        
        for i in range(len(self.newcon)):
            p0 = self.newcon[i][0]
            p1 = self.newcon[i][1]
            p2 = self.newcon[i][2]
            p3 = self.newcon[i][3]
            p4 = self.newcon[i][4]
            p5 = self.newcon[i][5]
            p6 = self.newcon[i][6]
            p7 = self.newcon[i][7]
            
            self.connect.append((p0,p1,p2,p3,p4,p5,p6,p7))
    
    
    def bc_faces(self):
        """it calculates the boundary conditions along the faces parallel to the z direction"""
        
        self.no_bc = len(self.boundary)
        self.no_elements = self.no_connections
        
        for i in range(self.no_bc):
            self.boundary.append((self.boundary[i][0]+self.no_elements,self.boundary[i][1],self.boundary[i][2]))
    
    
    def bc_frontback(self):
        """it calculates the boundary conditions on front and back faces"""
        
        self.temp_bc = []
        for i in range(len(self.boundary)):
            self.temp_bc.append((self.boundary[i][0],self.boundary[i][1]+2,self.boundary[i][2]))
        
        self.boundary = []
        self.boundary = self.temp_bc
        
        for i in range(self.no_elements):
            self.boundary.append((i+1,1,self.face1))
        
        for i in range(self.no_elements):
            self.boundary.append(((i+1)+(self.npz-2)*self.no_elements,2,self.face2))
    
    


class simplemesh():
    """it creates the file .mesh; px,py,pz are the polinomial orders"""
    def __init__(self, points, connect, boundary, polx=6, poly=6, polz=None, bc=None):
        
        self.points = points
        self.connect = connect
        self.boundary = boundary
        self.polx = polx
        self.poly = poly
        self.polz = polz
        
        
        self.f = open('squaretest.mesh','w')
        
        self.f.write('simple mesh format version = 1.1')
        
        self.nodes()
        self.elements()
        self.bc()
        
    def nodes(self):
        """it prints out the nodes in the right format"""
        
        self.f.write('\nnodes')
        
        k=0
        
        for i in self.points:
            k = k + 1
            p_x = str('%.14f'%i[0])
            p_y = str('%.14f'%i[1])
            p_z = str('%.14f'%i[2])
            self.f.write('\n\t' + str(k) + '\t' + str(p_x) +'\t' + str(p_y) +'\t' + str(p_z))
        
        self.f.write('\nend nodes')
    
    def elements(self):
        """it prints out the connections in the right format"""        
                
        self.f.write('\nelements')
        
        k=0
        self.px = 16
        self.py = 16
        
        if len(self.connect[0])==4:
            print ' -- 2D Case -- '
            for j in self.connect:
                k=k+1
                p0 = str(j[0]+1)
                p1 = str(j[1]+1)
                p2 = str(j[2]+1)
                p3 = str(j[3]+1)
                self.f.write('\n\t' + str(k) + '\tquad ' + str(len(self.connect[0])) + ' 0 '+ str(self.polx) + ' ' + str(self.poly) + ' 0')
                self.f.write('\n\t\t' + str(p0) + '\t\t' + str(p1) + '\t\t'+ str(p2) + '\t\t'+ str(p3))
            self.f.write('\nend elements')
        
        if len(self.connect[0])==8:
            print ' -- 3D Case -- '
            for j in self.connect:
                k=k+1
                p0 = str(j[0]+1)
                p1 = str(j[1]+1)
                p2 = str(j[2]+1)
                p3 = str(j[3]+1)
                p4 = str(j[4]+1)
                p5 = str(j[5]+1)
                p6 = str(j[6]+1)
                p7 = str(j[7]+1)
                self.f.write('\n\t' + str(k) + '\thex ' + str(len(self.connect[0])) + ' 0 '+ str(self.polx) + ' ' + str(self.poly) + ' ' + str(self.polz) + ' 0')
                self.f.write('\n\t\t' + str(p0) + '\t\t' + str(p1) + '\t\t'+ str(p2) + '\t\t'+ str(p3) + '\t\t' + str(p4) + '\t\t' + str(p5) + '\t\t'+ str(p6) + '\t\t'+ str(p7))
            self.f.write('\nend elements')
    
    
    def bc(self):
        """it prints out the boundary conditions in the right format"""
        
        self.f.write('\nboundary')
        
        for i in range(len(self.boundary)):
            self.f.write('\n\t\t' + str(self.boundary[i][0]) + '\t\t' + str(self.boundary[i][1]) + '\t\t' + str(self.boundary[i][2]))
        
        self.f.write('\nend boundary')



if __name__ == '__main__':
    
    """s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
    s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)
    
    s = s1 + s2
    sfinal = depth(s.points,s.connect,2.0,2)"""
    
    s = meshIn.rectangle(0.0,0.0,2.0,2.0,3,3)
    sint = boundary(s.points,s.connect,s.npx,s.npy,1,2,3,4)
    
    #s=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
    sfinal = depth(sint.points,sint.connect,sint.bc,2.0,3,1,2)
    
    #vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( sfinal.points, hexahedron=sfinal.newcon))
    #vtk.tofile('testthreed')
    
    simplemesh(sfinal.points,sfinal.connect,sfinal.boundary,6,6,6)
    