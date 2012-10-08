program simpleMAC
	use omp_lib
	use size
	use domain 
	implicit none
	
	integer						      :: timestep, t
	type(element), allocatable, Target, dimension(:):: d!<solution domain
	type(element), allocatable, Target, dimension(:,:):: b!<boundary domains
	!allocate our host memory
	allocate(  d(xSizeSol*ySizeSol) )!>multiply xSizeSol by ySizeSol to establish a one dimensional list able to hold all positions on the two dimensional grid
	allocate(  b(sides,sideSize) )!>Counter Clockwise starting with South.
	!allocate(  d(sizeSol) )
	!allocate(  b(sides,sideSize) )

	!> establish initial conditions on the host
	call initialConditionsForElement(d,b) 
	!call initialConditionsComplexGeometry(d,b)

	!> Our main computational loop
	do t=1,maxSteps
		!go to 52
		!> Calculate our timestep
		call calcTStepForElement(d,b,t,dt)
		
		!> Print the timestep to screen / gotta know what's going on
		write(*,*) 'Timestep: ',t,'dt: ',dt
		
		!> First boundary condition
		call ghostConditionForElement(b)
		
		!> calculate Fn and Gn
		call calcFnGnForElement(d)
		
		!> calculate Qn
		call calcQnForElement(d)
		
		!> calculate the pressure field
		call poissonForElement(d,b,t)
		
		!> calculate u-vel and v-vel
		call calcVelForElement(d)
		
		!> Check NaN if case blew up (>.<)
		!if( isnan(u(int(xSize/2),int(ySize/2))) ) then
			!write(*,*)'Case Blew UP!!!!!!!!!'
			!stop
		!end if
		
		!> Moving lid boundary
		call lidConditionForElement(b)
		!52 continue
		!> Plot to file
		if(mod(t,pInterval)==0) then 
			call writeVTKForElement(d,t)
		end if
	end do

	!> free up our memory
	deallocate(  d )
	deallocate(  b )
end program simpleMAC
	
