if __name__ == '__main__':
	
	s1=rectangle(0.0,0.0,4.0,4.0,5,3)
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s1.points, quad=s1.connect))
	vtk.tofile('s1')
	
	s2=rectangle(-4.0,4.0,4.0,4.0,5,3)
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s2.points, quad=s2.connect))
	vtk.tofile('s2')

	s3=rampquad([0.0,4.0,0.0],[4.0,4.0,0.0],[0.0,8.0,0.0],[8.0,8.0,0.0],5,3)
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s3.points, quad=s3.connect))
	vtk.tofile('s3')
	
	s4=rectangle(0.0,8.0,8.0,4.0,5,3)
	vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid( s4.points, quad=s4.connect))
	vtk.tofile('s4')
