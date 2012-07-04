!> This module contains all size variables for the domain.
!! @param xSize Number of cells in x direction
!! @param ySize Number of cells in y direction
!! @param Lx Length in x direction
!! @param Ly Length in y direction
!! @param Lz Length in z direction
!! @param maxSteps Maximum number of timesteps to be taken for the code
!! @param r Convergence criteria
!! @param itmax Maximum number of steps for the Poisson solver to take before it gives up (it will be less than this if it converges)
!! @param re Reynolds number of the flow
!! @param conv Convergence criteria for Poisson solver
!! @param pInterval Print interval
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
