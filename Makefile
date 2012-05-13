f90		=	gfortran

flags		=	-O3					\
			-fopenmp
				
libs		=

objects	=	src/size.f90			\
			src/driver.f90
				
main: $(objects)
	rm -rf run_SimpleMAC
	$(f90) $(flags) $(objects) $(libs) -o run_SimpleMAC
	rm -rf *.mod *.o

run:
	rm -rf data
	mkdir data
	time ./run_SimpleMAC
