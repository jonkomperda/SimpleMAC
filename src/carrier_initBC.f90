!>This subroutine populates the computational domain [0,0] to [1,1] with zeros in parallel
subroutine initialConditions(u,v,p,Fn,Gn,Q)
	use omp_lib
	use size
	implicit none
	double precision, dimension(xSize,ySize), intent(inout) :: u, v, p, Fn, Gn, Q
	integer							:: i, j
		
	!populate the domain with zeros
	!$omp parallel do shared(u,v,p) private(i,j)
	do j=1,ySize
		do i=1,xSize
			u(i,j)  = 0.0d0
			v(i,j)  = 0.0d0
			p(i,j)  = 0.0d0
			Fn(i,j) = 0.0d0
			Gn(i,j) = 0.0d0
			Q(i,j)  = 0.0d0
		end do
	end do
	!$end omp parallel do
end subroutine initialConditions

!>This subroutine populates the computational domain [0,0] to [1,1] with zeros in parallel
subroutine initialConditionsForElement(d,b)
	use omp_lib
	use size
	use domain
	implicit none
	double precision:: firstX
	integer							:: n,y,x,i,j!<n,i and j are counter variables. x and y keep track of position
	type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
	type(element), dimension(sides, sideSize), intent(inout) :: b
	type(element) :: test
	y = 0
	d(1)%W => test!TESTING TO SEE IF POINTERS TO DERIVED TYPE EVEN WORKS
	!populate the domain with zeros
	!$omp parallel do shared(d) private(n)
	
	!loop through solution domain
	do n=1,ySizeSol*xSizeSol
		x = (mod(n-1,xSizeSol)+1.0)
		d(n)%xLoc(1) = x
	    d(n)%X(1) =  x * dx
	    if(d(1)%X(1)==d(n)%X(1)) then!<if we see the same x value again we have entered a new row, so update y value
	        y = y + 1
	    end if
	    d(n)%xLoc(2) = y
	    d(n)%X(2) = y * dy 
	    d(n)%u  = 0.0d0
	    d(n)%v  = 0.0d0
	    d(n)%p  = 0.0d0
	    d(n)%Fn = 0.0d0
	    d(n)%Gn = 0.0d0
	    d(n)%Q  = 0.0d0
	    
	    !Set N, S, E, & W Neighboring positions
	    !if(x==1) then!At west boundary
	    	!d(n)%W=>b(4,y+1)
	    	!d(n)%E=>d(n+1)
	    !else if (x==xSizeSol) then!At East boundary
	    	!d(n)%E=>b(2,y+1)
	    	!d(n)%W=>d(n-1)
	    !else
	    	!d(n)%E=>d(n+1)
	    	!d(n)%W=>d(n-1)
	    !end if
	    
	    !if(y==1) then!At South boundary
	    	!d(n)%S=>b(1,x+1)
	    	!d(n)%N=>d(n+xSizeSol)
	    !else if (y==ySizeSol) then
	    	!d(n)%N=>b(3,x+1)
	    	!d(n)%S=>d(n-xSizeSol)
	    !else
	    	!d(n)%N=>d(n+xSizeSol)
	    	!d(n)%S=>d(n-xSizeSol)
	    !end if	 
	    
	end do
	
	!loop though boundary domain
	do i=1,sides
		do j=1, sideSize
			if(i==1) then!south
				b(i,j)%X(1)=j*dx
				b(i,j)%X(2)=0
				b(i,j)%xLoc(1)=j
				b(i,j)%xLoc(2)=0
				
				!if(j==1) then
					!b(i,j)%N => b(4,j+1)
					!b(i,j)%E => b(i,j+1)
				!else if (j==sideSize) then
					!b(i,j)%N => b(2,2)
					!b(i,j)%W => b(i,j-1)
				!else
					!b(i,j)%N => d(j-1)
					!b(i,j)%E => b(i,j+1)
					!b(i,j)%W => b(i,j-1)
				!end if
				
			else if(i==2) then!east
				b(i,j)%X(1)=xSize*dx
				b(i,j)%X(2)=j*dy
				b(i,j)%xLoc(1)=xSize
				b(i,j)%xLoc(2)=j
				
				!if(j==1) then
					!b(i,j)%N=>b(i,j+1)
					!b(i,j)%W=>b(1,sideSize-1)
				!else if (j==sideSize) then
					!b(i,j)%S=>b(i,j-1)
					!b(i,j)%W=>b(3,sideSize-1)
				!else
					!b(i,j)%N=>b(i,j+1)
					!b(i,j)%S=>b(i,j-1)
					!b(i,j)%W=>d(xSizeSol*(j-1))
				!end if
				
			else if(i==3) then!North
				b(i,j)%X(1)=j*dx
				b(i,j)%X(2)=ySize*dy
				b(i,j)%xLoc(1)=j
				b(i,j)%xLoc(2)=ySize
				
				!if(j==1) then
					!b(i,j)%S => b(4,sideSize-1)
					!b(i,j)%E => b(i,j+1)
				!else if(j==sideSize) then
					!b(i,j)%S => b(2,sideSize-1)
					!b(i,j)%W => b(i,j-1)
				!else
					!b(i,j)%W => b(i,j-1)
					!b(i,j)%E => b(i,j+1)
					!b(i,j)%S => d( (xSizeSol*(ySizeSol-1)) + (j-1) )
				!end if
				
			else if(i==4) then!West
				b(i,j)%X(1)=0
				b(i,j)%X(2)=j*dy
			    b(i,j)%xLoc(1)=0
				b(i,j)%xLoc(2)=j
				
				!if(j==1) then
					!b(i,j)%N=>b(i,j+1)
					!b(i,j)%E=>b(1,2)
				!else if (j==sideSize) then
					!b(i,j)%S => b(i,j-1)
					!b(i,j)%E => b(3,2)
				!else 
					!b(i,j)%N => b(i,j+1)
					!b(i,j)%S => b(i,j-1)
					!b(i,j)%E => d(1 + ( (j-2)*xSizeSol ) )
				!end if
				
			end if
			
			b(i,j)%u  = 0.0d0
	    	b(i,j)%v  = 0.0d0
	    	b(i,j)%p  = 0.0d0
	    	b(i,j)%Fn = 0.0d0
	    	b(i,j)%Gn = 0.0d0
	    	b(i,j)%Q  = 0.0d0
		end do
	end do
	
	!$end omp parallel do
end subroutine initialConditionsForElement

!>This subroutine applies the ghost cell boundary condition
subroutine ghostCondition(u,v)
	use omp_lib
	use size
	implicit none
	double precision, dimension(xSize,ySize), intent(inout) :: u, v
	integer							:: i,j
		
	!X Boundary condition
	!$omp parallel do shared(u) private(i) schedule(dynamic)
	do j=1,ySize
		u(1,j)     = 0.0d0
		u(xSize,j) = 0.0d0
	end do
	!$omp end parallel do
		
	!Y Boundary condition
	!$omp parallel do shared(v)
	do i=1,xSize
		v(i,1)     = 0.0d0
		v(i,ySize) = 0.0d0
	end do
	!$omp end parallel do
end subroutine ghostCondition

!>This subroutine applies the ghost cell boundary condition
subroutine ghostConditionForElement(b)
	use omp_lib
	use size
	use domain
	implicit none
	integer							:: i,j
	type(element), dimension(sides,sideSize), intent(inout) :: b
	!$omp parallel do shared(b) private(i,j)
	do i=1,sides
		do j=1,sideSize
			if(i==1 .or. i==3) then!North south
				b(i,j)%v = 0.0d0
			else if(i==2 .or. i==4) then
				b(i,j)%u = 0.0d0
			end if
		end do
	end do
	!$end omp parallel do
end subroutine ghostConditionForElement

!>our moving lid condition
subroutine lidCondition(u,v)
	use omp_lib
	use size
	implicit none
	double precision, dimension(xSize,ySize), intent(inout)	:: u, v
	integer							:: i, j
		
	!U velocity condition
	!$omp parallel do private(i) shared(u) schedule(dynamic)
	do i=2,xSize-1
		u(i,1)	= 		- u(i,2)
		u(i,ySize)	= 2.0d0	- u(i,ySize-1)
	end do
	!$omp end parallel do
		
	!V Velocity condition
	!$omp parallel do private(j) shared(v) schedule(dynamic)
	do j=2,ySize-1
		v(1,j)	=       - v(2,j)
		v(xSize,j)	=       - v(xSize-1,j)
	end do
	!$omp end parallel do
end subroutine lidCondition

!>our moving lid condition
subroutine lidConditionForElement(b)
	use omp_lib
	use size
	use domain
	implicit none
	type(element), dimension(sides,sideSize), intent(inout) :: b
	integer							:: i,j
	!U and V velocity condition
	!$omp parallel do private(i,j) shared(b) schedule(dynamic)
	do i=1,sides
		do j=2, sideSize-1
			if(i==1) then!South
				b(i,j)%u = -b(i,j)%N%u
			else if (i==2) then!East
				b(i,j)%v = -b(i,j)%W%v
			else if (i==3) then!North
				b(i,j)%u = 2.0d0 - b(i,j)%S%u 
			else if (i==4) then
				b(i,j)%v = -b(i,j)%E%v			
			end if
		end do
	end do
	!$omp end parallel do
end subroutine lidConditionForElement


