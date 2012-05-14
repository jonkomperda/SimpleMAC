module size
	integer, parameter             :: xSize      = 240
	integer, parameter             :: ySize      = 240
	double precision, parameter    :: Lx	   = 1.0d0
	double precision, parameter    :: Ly         = 1.0d0
	double precision, parameter    :: Lz         = 0.10d0
	
	!Time Step sizes
	integer, parameter          :: maxSteps      = 10000
	double precision, parameter :: r             = 0.11d0
	integer, parameter          :: itmax         = 1000
	
	!Fluid domain parameters
	double precision, parameter :: re            = 100.0d0
	double precision, parameter :: conv          = 1.0d-5
	integer, parameter          :: pInterval     = 1000
	
	!Calculated values
	double precision, parameter :: dx            = (1/(dble(xSize)-2.0d0))
	double precision, parameter :: dy            = (1/(dble(ySize)-2.0d0))
	double precision            :: dt       
end module size
