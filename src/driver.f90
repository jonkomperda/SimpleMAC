!>This is the main program. It calls all the necessary methods and contains the main calculation loop. 
program simpleMAC
    use omp_lib
    use size
    use domain 
    implicit none
    
    integer                                             :: timestep, t
    type(element), allocatable, Target, dimension(:)    :: d!<solution domain. contains every element point on the grid. 
    integer, allocatable, Target, dimension(:,:)        :: c!<cell connectivity data from input.vtk file

    !> Read in the input.vtk file and store data in d and c. 
    call readHeader() !Read header of input vtk file
    allocate(  d(numPoints) ) !allocate array d using information obtained from readHeader()
    call readPoints(d) !Read in the point data from input.vtk. This also reads in numCells.
    allocate( c(numCells,5)) !allocate array c using numCells read in from readPoints.
    call readCells(c) !Read in cell location data from input.vtk.
    call assignConnectivitiesUsingCells(d,c) !Using the cellular location data stored into c assign the neighboring point connectivities of the elements in array d. 
    call readBoundary(d) !Reads in boundary data from the input.vtk file. This information read in indicates what elements in d lie on the boundary of the grid. 

    !> establish initial conditions on the host
    call initialConditions(d)

    !> Our main computational loop
    do t=1,maxSteps
        !> Calculate our timestep
        call calcTStep(d,t,dt)
        
        !> Print the timestep to screen / gotta know what's going on
        write(*,*) 'Timestep: ',t,'dt: ',dt
        
        !> First boundary condition
        call ghostCondition(d)
        
        !> calculate Fn and Gn
        call calcFnGn(d)
         
        !> calculate Qn
        call calcQn(d)
        
        !> calculate the pressure field
        call poisson(d,t)
        
        !> calculate u-vel and v-vel
        call calcVel(d)
        
        !> Moving lid boundary
        call lidCondition(d)
        !> Plot to file
        if(mod(t,pInterval)==0) then 
            call writeVTK(d,c,t)
        end if
    end do

    !> free up our memory
    deallocate(  d )
    deallocate(  c )

end program simpleMAC
    
