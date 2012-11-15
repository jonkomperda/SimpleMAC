import meshIn
import simplemesh
import pyvtk

s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],17,17,(3,3,0,0))
s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],17,17,(3,0,0,0))
s3=meshIn.rampquad([5.0,0.0],[8.0,0.0],[4.0,2.0],[8.0,2.0],17,17,(3,2,0,0))
s4=meshIn.rectangle(-4.0,0.0,2.0,4.0,9,17,(3,0,3,1))
s5=meshIn.rectangle(-2.0,0.0,4.0,4.0,17,17,(0,0,3,0))
s6=meshIn.genshape([2.0,0.0],[4.0,2.0],[2.0,4.0],[4.0,4.0],17,17,(0,0,3,0))
s7=meshIn.rectangle(4.0,2.0,4.0,2.0,17,17,(0,2,3,0))


sint = s1 + s2 + s3 + s4 + s5 + s6 + s7


s = simplemesh.depth(sint.points,sint.connect,sint.bc,2.0,3,(2,2))

simplemesh.simplemesh('duct',s.points,s.connect,s.bc,6,6,6)

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s.points, hexahedron=s.connect))
vtk.tofile('duct')