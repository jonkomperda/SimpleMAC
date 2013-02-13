s1=meshIn.rampquad([-2.0,-2.0],[3.0,-2.0],[-2.0,0.0],[2.0,0.0],3,3)
s2=meshIn.genshape([3.0,-2.0],[5.0,0.0],[2.0,0.0],[4.0,2.0],3,3)

s = sint1
sfinal = depth(s.points,s.connect,s.bc,2.0,2)

#eightelementcube
sint = meshIn.square(0.0,0.0,2.0,3,3,(2,2,2,2))

#rectangle
sint = meshIn.rectangle(0.0,0.0,10.0,4.0,21,9,(3,2,2,2))
s2 = meshIn.rampquad([2.0,0.0],[4.0,0.0],[2.0,2.0],[6.0,2.0],3,3,(3,2,0,0))
s2 = meshIn.square(2.0,0.0,2.0,3,3,(3,2,0,0))

#pipewithhole
s1 = meshIn.rectangle(0.0,0.0,0.875,0.85,36,18,(2,0,0,2))
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
s8 = meshIn.rectangle(0.0,0.85,0.875,0.3,2,2,(0,3,0,2))

#backwardstep
s1 = meshIn.rectangle(0.0,1.0,5.0,2.0,6,3,(3,0,3,1))
s2 = meshIn.rectangle(5.0,1.0,10.0,2.0,11,3,(0,2,3,0))
s3 = meshIn.rectangle(5.0,0.0,10.0,1.0,11,2,(3,2,0,3))

#backwardstepreverse
s3 = meshIn.rectangle(10.0,1.0,5.0,2.0,6,3,(3,2,3,0))
s2 = meshIn.rectangle(0.0,1.0,10.0,2.0,11,3,(0,0,3,1))
s1 = meshIn.rectangle(0.0,0.0,10.0,1.0,11,2,(3,3,0,1))

#cavity
s1 = meshIn.square(5.0,0.0,5.0,51,51,(3,3,0,3))
s2 = meshIn.square(0.0,5.0,5.0,41,51,(3,0,2,2))
s3 = meshIn.square(5.0,5.0,5.0,51,51,(0,0,2,0))
s4 = meshIn.square(10.0,5.0,5.0,41,51,(3,2,2,0))

s1 = meshIn.square(6.0,0.0,4.0,5,5,(3,3,0,3))
s2 = meshIn.rectangle(0.0,4.0,6.0,4.0,7,5,(3,0,3,1))
s3 = meshIn.square(6.0,4.0,4.0,5,5,(0,0,3,0))
s4 = meshIn.rectangle(10.0,4.0,6.0,4.0,7,5,(3,2,3,0))

s1 = meshIn.square(6.0,0.0,4.0,11,11,(3,3,0,3))
s2 = meshIn.rectangle(0.0,4.0,6.0,4.0,19,11,(3,0,3,1))
s3 = meshIn.square(6.0,4.0,4.0,11,11,(0,0,3,0))
s4 = meshIn.rectangle(10.0,4.0,6.0,4.0,19,11,(3,2,3,0))

#channel
sint = meshIn.rectangle(0.0,0.0,10.0,2.0,11,3,(3,2,3,1))
sint = meshIn.rectangle(0.0,0.0,10.0,2.0,21,11,(3,2,3,1))

#channel with obstacle
s1 = meshIn.rectangle(0.0,0.0,2.0,1.0,3,2,(3,3,0,1))
s2 = meshIn.rectangle(0.0,1.0,2.0,1.0,3,2,(0,0,3,1))
s3 = meshIn.rectangle(2.0,1.0,1.0,1.0,2,2,(3,0,3,0))
s4 = meshIn.rectangle(3.0,0.0,2.0,1.0,3,2,(3,2,0,3))
s5 = meshIn.rectangle(3.0,1.0,2.0,1.0,3,2,(0,2,3,0))


s1 = meshIn.rectangle(-5.0,1.0,5.0,10.76,9,10,(3,0,2,1),0.0,0.3,0.1,0.0)
s2 = meshIn.rectangle(0.0,1.0,17.92,10.76,18,10,(0,2,2,0),0.3,0.0,0.1,0.0)
s3 = meshIn.rectangle(0.0,0.0,17.92,1.0,18,7,(3,2,0,3),0.3,0.0,0.0,0.1)

#bfs Jaobs
s1 = meshIn.rectangle(-5.0,1.0,5.0,10.76,9,11,(3,0,3,1),0.0,0.3,0.2,0.2)
s2 = meshIn.rectangle(0.0,1.0,17.92,10.76,31,11,(0,2,3,0),0.2,0.0,0.2,0.2)
s3 = meshIn.rectangle(0.0,0.0,17.92,1.0,31,7,(3,2,0,3),0.2,0.0,0.1,0.1)

#bfs shorter
s1 = meshIn.rectangle(-5.0,1.0,5.0,2.0,9,7,(3,0,3,1),0.0,0.3,0.15,0.15)
s2 = meshIn.rectangle(0.0,1.0,20.0,2.0,29,7,(0,2,3,0),0.2,0.0,0.15,0.15)
s3 = meshIn.rectangle(0.0,0.0,20.0,1.0,29,7,(3,2,0,3),0.2,0.0,0.1,0.1)

s1 = meshIn.rectangle(-5.0,1.0,5.0,10.76,9,11,(3,0,3,1),0.0,0.3,0.1,0.2)
s2 = meshIn.rectangle(0.0,1.0,5.92,10.76,21,11,(0,0,3,0),0.2,0.2,0.1,0.2)
s3 = meshIn.rectangle(0.0,0.0,5.92,1.0,21,7,(3,0,0,3),0.2,0.2,0.1,0.1)
s4 = meshIn.gsnotunif((5.92,0.0),(7.92,0.0),(5.92,1.0),(8.92,1.0),9,7,(3,3,0,0),0.0,0.0,0.1,0.1,0.2,0.0,0.0,0.0)
s5 = meshIn.rectangle(5.92,1.0,3.0,10.76,9,11,(0,0,3,0),0.2,0.0,0.1,0.2)
s6 = meshIn.rectangle(8.92,1.0,13.0,10.76,17,11,(3,2,3,0),0.2,0.0,0.1,0.2)

#duct
s1 = meshIn.rectangle(-5.0,1.0,5.0,2.0,9,7,(3,0,3,1),0.0,0.3,0.1,0.2)
s2 = meshIn.rectangle(0.0,1.0,3.0,2.0,9,7,(0,0,3,0),0.3,0.0,0.1,0.2)
s3 = meshIn.gsnotunif((3.0,1.0),(5.0,2.0),(3.0,3.0),(5.0,3.0),9,7,(0,0,3,0),0.3,0.0,0.1,0.2,0.0,0.0,0.0,0.0)
s4 = meshIn.rectangle(5.0,2.0,10.0,1.0,13,7,(0,2,3,0),0.3,0.0,0.05,0.1)
s5 = meshIn.gsnotunif((0.0,0.0),(4.0,0.0),(0.0,1.0),(3.0,1.0),9,7,(3,0,0,3),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)
s6 = meshIn.gsnotunif((4.0,0.0),(6.0,1.0),(3.0,1.0),(5.0,2.0),9,7,(3,0,0,0),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)
s7 = meshIn.gsnotunif((6.0,1.0),(15.0,1.0),(5.0,2.0),(15.0,2.0),13,7,(3,2,0,0),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)

s2 = meshIn.rectangle(0.0,1.0,4.92,10.76,17,11,(0,0,3,0),0.2,0.2,0.2,0.2)
s3 = meshIn.rectangle(0.0,0.0,4.92,1.0,17,7,(3,3,0,3),0.2,0.2,0.1,0.1)
s4 = meshIn.rectangle(4.92,1.0,13.0,10.76,17,11,(3,2,3,0),0.2,0.0,0.2,0.2)

#duct
s1 = meshIn.rectangle(-5.0,1.0,5.0,2.0,9,7,(3,0,3,1),0.0,0.3,0.1,0.2)
s2 = meshIn.rectangle(0.0,1.0,3.0,2.0,9,7,(0,0,3,0),0.3,0.0,0.1,0.2)
s3 = meshIn.gsnotunif((3.0,1.0),(5.0,1.5),(3.0,3.0),(5.0,3.0),9,7,(0,0,3,0),0.3,0.0,0.1,0.2,0.0,0.0,0.0,0.0)
s4 = meshIn.rectangle(5.0,2.0,10.0,1.0,13,7,(0,2,3,0),0.3,0.0,0.05,0.1)
s5 = meshIn.gsnotunif((0.0,0.0),(4.0,0.0),(0.0,1.0),(3.0,1.0),9,7,(3,0,0,1),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)
s6 = meshIn.gsnotunif((4.0,0.0),(6.0,1.0),(3.0,1.0),(5.0,2.0),9,7,(3,0,0,0),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)
s7 = meshIn.gsnotunif((6.0,1.0),(15.0,1.0),(5.0,2.0),(15.0,2.0),13,7,(3,2,3,0),0.0,0.0,0.1,0.1,0.3,0.0,0.0,0.0)
s8 = meshIn.gsnotunif((0.0,1.0),(3.0,1.0),(0.0,2.0),(5.0,2.0),9,9,(0,0,3,1),0.3,0.0,0.15,0.0,0.0,0.0,0.0,0.0)
s2 = meshIn.rectangle(0.0,1.0,4.92,10.76,17,11,(0,0,3,0),0.2,0.2,0.2,0.2)
s3 = meshIn.rectangle(0.0,0.0,4.92,1.0,17,7,(3,3,0,3),0.2,0.2,0.1,0.1)
s4 = meshIn.rectangle(4.92,1.0,13.0,10.76,17,11,(3,2,3,0),0.2,0.0,0.2,0.2)

#skewed cube
s1 = meshIn.rectangle(0.0,0.0,4.0,4.0,4,4,(2,2,2,2),1.0,0.0,1.0,0.0)

#bfs shorter
s1 = meshIn.rectangle(-5.0,1.0,5.0,2.0,9,7,(3,0,3,1),0.0,0.3,0.15,0.15)
s2 = meshIn.rectangle(0.0,1.0,20.0,2.0,29,7,(0,2,3,0),0.2,0.0,0.15,0.15)
s3 = meshIn.rectangle(0.0,0.0,20.0,1.0,29,7,(3,2,0,3),0.2,0.0,0.1,0.1)

sint = meshIn.gsnotunif((0.0,0.0),(4.0,0.0),(0.0,1.0),(4.0,1.0),2,2,(3,3,3,3),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
sint = meshIn.rectangle(0.0,0.0,10.0,1.0,11,2,(3,2,3,1),0.0,0.0,0.0,0.0)

s1 = meshIn.gsnotunif((0.0,0.0),(6.0,0.0),(0.0,1.0),(4.0,1.0),6,5,(3,0,3,1),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
s2 = meshIn.gsnotunif((6.0,0.0),(10.0,0.0),(4.0,1.0),(10.0,1.0),6,5,(3,2,3,0),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

#ramp 1 el
s1 = meshIn.rectangle(0.0,0.0,4.0,1.0,5,2,(3,0,3,1),0.0,0.0,0.0,0.0)
s2 = meshIn.gsnotunif((4.0,0.0),(5.0,0.2),(4.0,1.0),(5.0,1.0),2,2,(3,0,3,0),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
s3 = meshIn.rectangle(5.0,0.2,5.0,0.8,6,2,(3,2,3,0),0.0,0.0,0.0,0.0)

#ramp 6 el
s1 = meshIn.rectangle(0.0,0.0,4.0,1.0,9,7,(3,0,3,1),0.0,0.3,0.1,0.1)
s2 = meshIn.gsnotunif((4.0,0.0),(5.0,0.2),(4.0,1.0),(5.0,1.0),5,7,(3,0,3,0),0.0,0.0,0.1,0.1,0.0,0.0,0.0,0.0)
s3 = meshIn.rectangle(5.0,0.2,5.0,0.8,11,7,(3,2,3,0),0.3,0.0,0.08,0.08)

#bfs+ramp
s1 = meshIn.rectangle(-5.0,1.0,5.0,2.0,9,7,(3,0,3,1),0.0,0.3,0.15,0.15)
s2 = meshIn.rectangle(0.0,1.0,5.0,2.0,15,7,(0,0,3,0),0.2,0.0,0.15,0.15)
s3 = meshIn.rectangle(0.0,0.0,5.0,1.0,15,7,(3,0,0,3),0.2,0.0,0.1,0.1)
s4 = meshIn.rectangle(5.0,1.0,5.0,2.0,2,2,(0,0,3,1),0.0,0.0,0.0,0.0)
s5 = meshIn.gsnotunif((5.0,0.0),(8.0,0.0),(5.0,1.0),(10.0,1.0),2,2,(3,3,0,1),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
s6 = meshIn.rectangle(10.0,1.0,10.0,2.0,2,2,(3,2,3,0),0.0,0.0,0.0,0.0)

#ramp 3 pieces
s1 = meshIn.rectangle(0.0,0.0,4.0,1.0,9,5,(3,0,0,1),0.0,0.0,0.0,0.0)
s2 = meshIn.gsnotunif((4.0,0.0),(5.0,1.0),(4.0,1.0),(5.0,2.0),5,5,(3,0,0,0),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
s3 = meshIn.rectangle(5.0,1.0,5.0,1.0,11,5,(3,2,3,0),0.0,0.0,0.00,0.00)
s4 = meshIn.gsnotunif((0.0,1.0),(4.0,1.0),(0.0,2.0),(5.0,2.0),9,5,(0,0,3,1),0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

#duct
ha = 9
hb = 15
hc = 9
hd = 17
vu = 9
vd = 7
s5 = meshIn.rectangle(-5.0,1.0,5.0,2.0,ha,vu,(3,0,3,1),0.0,0.3,0.1,0.1)
s6 = meshIn.rectangle(0.0,1.0,5.0,2.0,hb,vu,(0,0,3,0),0.2,0.2,0.1,0.1)
s7 = meshIn.gsnotunif((5.0,1.0),(7.0,1.5),(5.0,3.0),(7.0,3.0),hc,vu,(0,0,3,0),0.1,0.1,0.1,0.1,0.0,0.0,0.0,0.0)
#s8 = meshIn.gsnotunif((10.0,1.5),(17.0,1.5),(10.0,3.0),(17.0,3.0),2,vu,(0,2,3,0),0.0,0.0,0.075,0.075,0.0,0.0,0.0,0.0)
s9 = meshIn.gsnotunif((7.0,1.5),(20.0,1.5),(7.0,3.0),(20.0,3.0),hd,vu,(0,2,3,0),0.3,0.0,0.075,0.075,0.0,0.0,0.0,0.0)
s1 = meshIn.rectangle(0.0,0.0,5.0,1.0,hb,vd,(3,0,0,3),0.2,0.2,0.1,0.1)
s2 = meshIn.gsnotunif((5.0,0.0),(7.0,1.0),(5.0,1.0),(7.0,1.5),hc,vd,(3,0,0,0),0.1,0.1,0.1,0.1,0.0,0.0,0.0,0.0)
#s3 = meshIn.gsnotunif((10.0,1.0),(17.0,1.0),(10.0,1.5),(17.0,1.5),2,vd,(3,2,0,0),0.0,0.0,0.05,0.05,0.0,0.0,0.0,0.0)
s4 = meshIn.gsnotunif((7.0,1.0),(20.0,1.0),(7.0,1.5),(20.0,1.5),hd,vd,(3,2,0,0),0.3,0.0,0.05,0.05,0.0,0.0,0.0,0.0)

a = (ha+hb+hc+hd-4)*(vu-1) + (hb+hc+hd-3)*(vd-1)
p = ((ha+hb+hc+hd-3)*vu + (hb+hc+hd-2)*(vd-1))*2

sint = s1 + s2 + s4 + s5 + s6 + s7 + s9



sint = s1 + s2 + s3 + s4

s = depth(sint.points,sint.connect,sint.bc,0.05,2,(2,2))

simplemesh('cavity',s.points,s.connect,s.bc,6,6,6)

vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s.points, hexahedron=s.connect))
vtk.tofile('cavity')
