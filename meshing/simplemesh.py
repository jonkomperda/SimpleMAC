import meshIn
import pyvtk

class simplemesh(object):
    """docstring for simplemesh"""
    def __init__(self, arg):
        super(simplemesh, self).__init__()
        self.arg = arg
        
    


if __name__ == '__main__':
    
    s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
    s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)
    
    s = s1 + s2
    vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s.points, quad=s.connect))
    vtk.tofile('test2')