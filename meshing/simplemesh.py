import pyvtk
import meshIn

class simplemesh():
    """it creates the file .mesh"""
    def __init__(self, points, connect, bc=None):
        
        self.points = points
        self.connect = connect
        self.bc = bc
        
        
        for i in self.points:
            p_x = str(i[0])
            p_y = str(i[1])
            p_z = str(i[2])
            #print p_x + '  ' + p_y + '  ' + p_z
        
        """for j in self.connect:
            p0 = str(j[0])
            p1 = str(j[1])
            p2 = str(j[2])
            p3 = str(j[3])
            #print p1 + '  ' + p2 + '  ' + p3 + '  ' + p4"""

class depth():
    """it repeats the intial mesh along the z axis"""
    def __init__(self, points, connect, lz, npz):
        
        self.points = points
        self.connect = connect
        self.lz = lz
        self.npz = npz
        
        self.dz = self.lz/(self.npz-1)
        self.z_pos = 0
                
        self.points.append((self.points[0][0],self.points[0][1],self.dz))
        
        for i in range(npz):
            self.z_pos = self.z_pos + self.dz
            for j in range(len(self.points)):
                self.points.append((self.points[j][0],self.points[j][1],self.dz))
                
            print str(i) + '  ' + str(j)
        
        print self.points



if __name__ == '__main__':
    
    s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
    s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)
    
    s = s1 + s2
    sfinal = depth(s.points,s.connect,1.0,2)
    
    #simplemesh(s.points,s.connect)
    