!> This module contains all size variables for the domain.
module size
	integer, parameter             :: xSize     = 240       !< Number of cells in x-direction
	integer, parameter             :: ySize     = 240       !< Number of cells in y-direction
	double precision, parameter    :: Lx        = 1.0d0     !< Length in x-direction
	double precision, parameter    :: Ly        = 1.0d0     !< Length in y-direction
	double precision, parameter    :: Lz        = 0.10d0    !< Length in z-direction
	
	!Time Step sizes
	integer, parameter          :: maxSteps     = 10000     !< Maximum number of timesteps taken by the code
	double precision, parameter :: r            = 0.11d0    !< Convergence criteria
	integer, parameter          :: itmax        = 1000      !< Maximum number of steps used by the Poisson solver before it gives up (less if converged)
	
	!Fluid domain parameters
	double precision, parameter :: re           = 100.0d0   !< Reynolds number defined as \f$ Re = \frac{U L}{\nu} \f$
	double precision, parameter :: conv         = 1.0d-5    !< Convergence criteria for poisson solver
	integer, parameter          :: pInterval    = 1000      !< Print interval
	
	!Calculated values
	double precision, parameter :: dx           = (1/(dble(xSize)-2.0d0))   !< Calculated \f$ \Delta x \f$
	double precision, parameter :: dy           = (1/(dble(ySize)-2.0d0))   !< Calculated \f$ \Delta y \f$
	double precision            :: dt                                       !< Timestep size \f$ \Delta t \f$
end module size
