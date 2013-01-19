"""s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
#s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)

s = sint1
sfinal = depth(s.points,s.connect,s.bc,2.0,2)"""

#eightelementcube
#sint = meshIn.square(0.0,0.0,2.0,3,3,(2,2,2,2))

#rectangle
#sint = meshIn.rectangle(0.0,0.0,10.0,4.0,21,9,(3,2,2,2))
#s2 = meshIn.rampquad([2.0,0.0],[4.0,0.0],[2.0,2.0],[6.0,2.0],3,3,(3,2,0,0))
#s2 = meshIn.square(2.0,0.0,2.0,3,3,(3,2,0,0))

#pipewithhole
"""s1 = meshIn.rectangle(0.0,0.0,0.875,0.85,36,18,(2,0,0,2))
s2 = meshIn.rectangle(0.875,0.0,0.25,0.85,11,18,(2,0,3,0))
s3 = meshIn.rectangle(1.125,0.0,0.875,0.85,36,18,(2,2,0,0))
s4 = meshIn.rectangle(1.125,0.85,0.875,0.3,36,7,(0,2,0,3))
s5 = meshIn.rectangle(1.125,1.15,0.875,0.85,36,18,(0,2,2,0))
s6 = meshIn.rectangle(0.875,1.15,0.25,0.85,11,18,(3,0,2,0))
s7 = meshIn.rectangle(0.0,1.15,0.875,0.85,36,18,(0,0,2,2))
s8 = meshIn.rectangle(0.0,0.85,0.875,0.3,36,7,(0,3,0,2))
#testshape with just 8 elements
s1 = meshIn.rectangle(0.0,0.0,0.875,0.85,2,2,(2,0,0,2))
s2 = meshIn.rectangle(0.875,0.0,0.25,0.85,2,2,(2,0,3,0))
s3 = meshIn.rectangle(1.125,0.0,0.875,0.85,2,2,(2,2,0,0))
s4 = meshIn.rectangle(1.125,0.85,0.875,0.3,2,2,(0,2,0,3))
s5 = meshIn.rectangle(1.125,1.15,0.875,0.85,2,2,(0,2,2,0))
s6 = meshIn.rectangle(0.875,1.15,0.25,0.85,2,2,(3,0,2,0))
s7 = meshIn.rectangle(0.0,1.15,0.875,0.85,2,2,(0,0,2,2))
s8 = meshIn.rectangle(0.0,0.85,0.875,0.3,2,2,(0,3,0,2))"""

#backwardstep
"""s1 = meshIn.rectangle(0.0,1.0,5.0,2.0,6,3,(3,0,3,1))
s2 = meshIn.rectangle(5.0,1.0,10.0,2.0,11,3,(0,2,3,0))
s3 = meshIn.rectangle(5.0,0.0,10.0,1.0,11,2,(3,2,0,3))"""

#backwardstepreverse
"""s3 = meshIn.rectangle(10.0,1.0,5.0,2.0,6,3,(3,2,3,0))
s2 = meshIn.rectangle(0.0,1.0,10.0,2.0,11,3,(0,0,3,1))
s1 = meshIn.rectangle(0.0,0.0,10.0,1.0,11,2,(3,3,0,1))"""

#cavity
"""s1 = meshIn.square(5.0,0.0,5.0,51,51,(3,3,0,3))
s2 = meshIn.square(0.0,5.0,5.0,41,51,(3,0,2,2))
s3 = meshIn.square(5.0,5.0,5.0,51,51,(0,0,2,0))
s4 = meshIn.square(10.0,5.0,5.0,41,51,(3,2,2,0))"""
"""s1 = meshIn.square(6.0,0.0,4.0,5,5,(3,3,0,3))
s2 = meshIn.rectangle(0.0,4.0,6.0,4.0,7,5,(3,0,3,1))
s3 = meshIn.square(6.0,4.0,4.0,5,5,(0,0,3,0))
s4 = meshIn.rectangle(10.0,4.0,6.0,4.0,7,5,(3,2,3,0))"""
s1 = meshIn.square(6.0,0.0,4.0,11,11,(3,3,0,3))
s2 = meshIn.rectangle(0.0,4.0,6.0,4.0,19,11,(3,0,3,1))
s3 = meshIn.square(6.0,4.0,4.0,11,11,(0,0,3,0))
s4 = meshIn.rectangle(10.0,4.0,6.0,4.0,19,11,(3,2,3,0))

#channel
#sint = meshIn.rectangle(0.0,0.0,10.0,2.0,11,3,(3,2,3,1))
#sint = meshIn.rectangle(0.0,0.0,10.0,2.0,21,11,(3,2,3,1))

#channel with obstacle
"""s1 = meshIn.rectangle(0.0,0.0,2.0,1.0,3,2,(3,3,0,1))
s2 = meshIn.rectangle(0.0,1.0,2.0,1.0,3,2,(0,0,3,1))
s3 = meshIn.rectangle(2.0,1.0,1.0,1.0,2,2,(3,0,3,0))
s4 = meshIn.rectangle(3.0,0.0,2.0,1.0,3,2,(3,2,0,3))
s5 = meshIn.rectangle(3.0,1.0,2.0,1.0,3,2,(0,2,3,0))"""

sint = s1 + s2 + s3 + s4

s = depth(sint.points,sint.connect,sint.bc,0.05,2,(2,2))

simplemesh('cavity',s.points,s.connect,s.bc,6,6,6)

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s.points, hexahedron=s.connect))
vtk.tofile('cavity')
