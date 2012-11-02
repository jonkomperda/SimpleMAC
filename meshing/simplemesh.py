import pyvtk
import meshIn

class simplemesh():
    """it creates the file .mesh"""
    def __init__(self, points, connect, bc=None,):
        
        self.points = points
        self.connect = connect
        self.bc = bc
        
        self.f = open('squaretest.mesh','w')
        
        self.f.write('simple mesh format version = 1.1')
        
        self.nodes()
        self.connections()
        
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
    
    def connections(self):
        """it prints out the connections in the right format"""        
        
        #print self.connect
        
        self.f.write('\nelements')
        
        k=0
        
        for j in self.connect:
            k=k+1
            p0 = str(j[0])
            p1 = str(j[1])
            p2 = str(j[2])
            p3 = str(j[3])
            #print p0 + '  ' + p1 + '  ' + p2 + '  ' + p3
            self.f.write('\n\t' + str(k) + '\t' + str(p0) + '\t' + str(p1) +'\t' + str(p2) +'\t' + str(p3))
        
    
    


class depth():
    """it repeats the intial mesh along the z axis, lz=total depth, npz=no. of points along z direction"""
    def __init__(self, points, connect, lz, npz):
        
        self.points = points
        self.connect = connect
        self.lz = lz
        self.npz = npz
        
        threeDimcase = False
        
        if self.npz > 1:
            threeDimcase = True
            self.coordinates()
            self.connections()
            
        

    
    
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
        
        


if __name__ == '__main__':
    
    """s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
    s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)
    
    s = s1 + s2
    sfinal = depth(s.points,s.connect,2.0,2)"""
    
    s = meshIn.rectangle(0.0,0.0,2.0,2.0,3,3)
    sfinal = depth(s.points,s.connect,2.0,3)
    
    simplemesh(sfinal.points,sfinal.connect)
    