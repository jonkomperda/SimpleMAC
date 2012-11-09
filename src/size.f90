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
    integer, parameter             :: xSize         = 160
    integer, parameter             :: ySize         = 160
    integer, parameter             :: xSizeSol      = xSize - 2!size of solution part of grid
    integer, parameter             :: ySizeSol      = ySize - 2!size of solution part of grid
    integer, parameter             :: sideSize      = xSize
    integer, parameter             :: sides         = 6
    integer, parameter             :: boundSideBig  = xSize 
    integer, parameter             :: boundSideSmall= xSize/2 
    integer, parameter             :: solSideBig    = xSizeSol 
    integer, parameter             :: solSideSmall  = boundSideSmall - 2 
    integer, parameter             :: sizeSol = (solSideSmall*boundSideSmall) + (solSideSmall*solSideBig)
    double precision, parameter    :: Lx            = 1.0d0
    double precision, parameter    :: Ly            = 1.0d0
    double precision, parameter    :: Lz            = 0.10d0
    
    !Time Step sizes
    integer, parameter          :: maxSteps         = 160000
    double precision, parameter :: r                = 0.11d0
    integer, parameter          :: itmax            = 1000
    
    !Fluid domain parameters
    double precision, parameter :: re               = 6000.0d0
    double precision, parameter :: conv             = 1.0d-5
    integer, parameter          :: pInterval        = 500
    
    !Calculated values
    double precision, parameter :: dx               = (1/(dble(xSize)-2.0d0))
    double precision, parameter :: dy               = (1/(dble(ySize)-2.0d0))
    double precision            :: dt       
end module size
