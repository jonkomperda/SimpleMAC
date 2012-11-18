program simpleMAC
    use omp_lib
    use size
    use domain 
    implicit none
    
    integer                                             :: timestep, t
    type(element), allocatable, Target, dimension(:)    :: d!<solution domain
    type(element), allocatable, Target, dimension(:,:)  :: b!<boundary domains
    integer, allocatable, Target, dimension(:,:)        :: c!<cells
    !allocate our host memory
    !allocate(  b(sides,boundSideBig) )

    !> establish initial conditions on the host
    call readHeader()
    allocate(  d(numPoints) )
    call readPoints(d)
    allocate( c(numCells,5))
    call readCells(c)
    call assignConnectivities(d)
    call initialConditionsMeshReader(d)
    !call debugSolPointConnections(d)
    !go to 59
    !> Our main computational loop
    do t=1,maxSteps
        !> Calculate our timestep
        call calcTStepMeshReader(d,t,dt)
        
        !> Print the timestep to screen / gotta know what's going on
        write(*,*) 'Timestep: ',t,'dt: ',dt
        
        !> First boundary condition
        call ghostConditionMeshReader(d)
        
        !> calculate Fn and Gn
        call calcFnGnMeshReader(d)
        
        !> calculate Qn
        call calcQnMeshReader(d)
        
        !> calculate the pressure field
        call poissonMeshReader(d,t)
        
        !> calculate u-vel and v-vel
        call calcVelMeshReader(d)
        
        !> Check NaN if case blew up (>.<)
        !if( isnan(u(int(xSize/2),int(ySize/2))) ) then
            !write(*,*)'Case Blew UP!!!!!!!!!'
            !stop
        !end if
        
        !> Moving lid boundary
        call lidConditionMeshReader(d)
        !> Plot to file
        if(mod(t,pInterval)==0) then 
            call writeVTKMeshReader(d,c,t)
        end if

    end do
    !59 continue
    !> free up our memory
    deallocate(  d )
    deallocate(  c )
    !deallocate(  b )
end program simpleMAC
    
