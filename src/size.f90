!> This module contains all size variables for the domain.
module size
    integer, parameter             :: xSize         = 62 !<Fixed integer used to calculate dx
    integer, parameter             :: ySize         = 62 !<Fixed integer used to calculate dy
    integer                        :: numPoints     = 0
    integer                        :: numCells      = 0
    integer                        :: numCellConnections = 0
    double precision, parameter    :: Lx            = 1.0d0 !<Lx Length in x direction
    double precision, parameter    :: Ly            = 1.0d0 !<Ly Length in y direction
    double precision, parameter    :: Lz            = 0.10d0 !<Lz Length in z direction
    
    
    !Time Step sizes
    integer, parameter          :: maxSteps         = 30000 !<maxSteps Maximum number of timesteps to be taken for the code.  Will be 30,000
    double precision, parameter :: r                = 0.11d0 !<Convergence criteria
    integer, parameter          :: itmax            = 1000 !<Maximum number of steps for the Poisson solver to take before it gives up (it will be less than this if it converges)
    
    !Fluid domain parameters
    double precision, parameter :: re               = 5000.0d0 !<Reynolds number of the flow
    double precision, parameter :: conv             = 1.0d-5 !<Convergence criteria for Poisson solver
    integer, parameter          :: pInterval        = 1000 !< Print interval
    
    !Calculated values
    double precision, parameter :: dx               = (1/(dble(xSize)-2.0d0))
    double precision, parameter :: dy               = (1/(dble(ySize)-2.0d0))
    double precision            :: dt       

    ! Particle Parameters
    integer, parameter          :: numParticles     = 3721  ! Number of Monte Carlo Particles to be simulated.  Initially, one per cell in the grid.  Will be 3721
    integer, parameter          :: PartPrintInt     = 100   ! Particle Print Interval

end module size