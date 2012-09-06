program simpleMAC
	use omp_lib
	use size
	use domain 
	implicit none!test comment for ubuntu git setup
	
	integer						      :: timestep, t
	double precision, allocatable, dimension(:,:)	:: u, v, p, Fn, Gn, Q
	type(element), allocatable, Target, dimension(:):: d!<solution domain
	type(element), allocatable, Target, dimension(:,:):: b!<boundary domains
	!allocate our host memory
	allocate(  d(xSizeSol*ySizeSol) )!>multiply xSizeSol by ySizeSol to establish a one dimensional list able to hold all positions on the two dimensional grid
	allocate(  b(sides,sideSize) )!>1=S,2=E,3=N,4=W 
	allocate(  u(xSize,ySize) )
	allocate(  v(xSize,ySize) )
	allocate(  p(xSize,ySize) )
	allocate( Fn(xSize,ySize) )
	allocate( Gn(xSize,ySize) )
	allocate(  Q(xSize,ySize) )
	
	!> establish initial conditions on the host
	!call initialConditions(u,v,p,Fn,Gn,Q)
	call initialConditionsForElement(d,b) 
	
	!> Our main computational loop
	do t=1,maxSteps
		!> Calculate our timestep
		!call calcTStep(u,v,t,dt)
		call calcTStepForElement(d,b,t,dt)
		
		!> Print the timestep to screen / gotta know what's going on
		write(*,*) 'Timestep: ',t,'dt: ',dt
		
		!> First boundary condition
		!call ghostCondition(u,v)
		call ghostConditionForElement(b)
		
		!> calculate Fn and Gn
		!call calcFnGn(u,v,Fn,Gn)
		call calcFnGnForElement(d)
		
		!> calculate Qn
		!call calcQn(Fn,Gn,Q)
		call calcQnForElement(d)
		
		!> calculate the pressure field
		!call poisson(Q,u,v,t,p)    !Serial Poisson solver (for testing)
		!call parPoisson(Q,u,v,t,p)
		call poissonForElement(d,b,t)
		
		!> calculate u-vel and v-vel
		!call calcVel(Fn,Gn,p,u,v)
		call calcVelForElement(d)
		
		!> Check NaN if case blew up (>.<)
		!if( isnan(u(int(xSize/2),int(ySize/2))) ) then
			!write(*,*)'Case Blew UP!!!!!!!!!'
			!stop
		!end if
		
		!> Moving lid boundary
		!call lidCondition(u,v)
		call lidConditionForElement(b)
		
		!> Plot to file
		if(mod(t,pInterval)==0) then 
			!call writeVTK(u,v,p,t)
			call writeVTKForElement(d,t)
		end if
	end do
	
	!> free up our memory
	deallocate(  d )
	deallocate(  b )
	deallocate(  u )
	deallocate(  v )
	deallocate(  p )
	deallocate( Fn )
	deallocate( Gn )
	deallocate(  Q )
end program simpleMAC
	
