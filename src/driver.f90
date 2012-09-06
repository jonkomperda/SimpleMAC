program simpleMAC
	use omp_lib
	use size
	use domain 
	implicit none!test comment for ubuntu git setup
	
	integer						      :: timestep, t
	type(element), allocatable, Target, dimension(:):: d!<solution domain
	type(element), allocatable, Target, dimension(:,:):: b!<boundary domains
	!allocate our host memory
	allocate(  d(xSizeSol*ySizeSol) )!>multiply xSizeSol by ySizeSol to establish a one dimensional list able to hold all positions on the two dimensional grid
	allocate(  b(sides,sideSize) )!>1=S,2=E,3=N,4=W 
	
	!> establish initial conditions on the host
	call initialConditionsForElement(d,b) 
	
	!> Our main computational loop
	do t=1,maxSteps
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
		
		!> Plot to file
		if(mod(t,pInterval)==0) then 
			call writeVTKForElement(d,t)
		end if
	end do
	
	!> free up our memory
	deallocate(  d )
	deallocate(  b )
end program simpleMAC
	
