program simpleMAC
    use omp_lib
    use size
    use domain 
    implicit none
    
    integer                                             :: timestep, t
    type(element), allocatable, Target, dimension(:)    :: d!<solution domain
    integer, allocatable, Target, dimension(:,:)        :: c!<cells

    !> establish initial conditions on the host
    call readHeader()
    allocate(  d(numPoints) )
    call readPoints(d)
    allocate( c(numCells,5))
    call readCells(c)
    call assignConnectivitiesUsingCells(d,c)
    call readBoundary(d)
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
    
