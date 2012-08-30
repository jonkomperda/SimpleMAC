program simpleMAC
	use omp_lib
	use size
	use domain 
	implicit none
	
	integer						      :: timestep, t
	double precision, allocatable, dimension(:,:)	:: u, v, p, Fn, Gn, Q
	type(element), allocatable, dimension(:):: d
	!allocate our host memory
	allocate(  d(xSize*ySize) )!>multiply xSize by ySize to establish a one dimensional list able to hold all positions on the two dimensional grid
	allocate(  u(xSize,ySize) )
	allocate(  v(xSize,ySize) )
	allocate(  p(xSize,ySize) )
	allocate( Fn(xSize,ySize) )
	allocate( Gn(xSize,ySize) )
	allocate(  Q(xSize,ySize) )
	
	!> establish initial conditions on the host
	call initialConditions(u,v,p,Fn,Gn,Q)
	call initialConditionsForElement(d) 
	!> Our main computational loop
	do t=1,maxSteps
		!> Calculate our timestep
		call calcTStep(u,v,t,dt)
		call calcTStepForElement(d,t,dt)
		
		!> Print the timestep to screen / gotta know what's going on
		write(*,*) 'Timestep: ',t,'dt: ',dt
		
		!> First boundary condition
		call ghostCondition(u,v)
		
		!> calculate Fn and Gn
		call calcFnGn(u,v,Fn,Gn)
		
		!> calculate Qn
		call calcQn(Fn,Gn,Q)
	
		!> calculate the pressure field
		!call poisson(Q,u,v,t,p)    !Serial Poisson solver (for testing)
		call parPoisson(Q,u,v,t,p)
		
		!> calculate u-vel and v-vel
		call calcVel(Fn,Gn,p,u,v)
		
		!> Check NaN if case blew up (>.<)
		if( isnan(u(int(xSize/2),int(ySize/2))) ) then
			write(*,*)'Case Blew UP!!!!!!!!!'
			stop
		end if
		
		!> Moving lid boundary
		call lidCondition(u,v)
		
		!> Plot to file
		if(mod(t,pInterval)==0) then 
			call writeVTK(u,v,p,t)
		end if
	end do
	
	!> free up our memory
	deallocate(  u )
	deallocate(  v )
	deallocate(  p )
	deallocate( Fn )
	deallocate( Gn )
	deallocate(  Q )
end program simpleMAC
	
