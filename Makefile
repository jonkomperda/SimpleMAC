f90		=	gfortran

flags		=	-O3					\
			-fopenmp
				
libs		=

objects	=	src/size.f90			\
			src/carrier_initBC.f90		\
			src/carrier_velCalc.f90		\
			src/carrier_timeStep.f90	\
			src/carrier_poisson.f90		\
			src/carrier_vtk.f90		\
			src/driver.f90
				
main: $(objects)
	rm -rf run_SimpleMAC
	$(f90) $(flags) $(objects) $(libs) -o run_SimpleMAC
	rm -rf *.mod *.o

run:
	rm -rf data
	mkdir data
	time ./run_SimpleMAC
