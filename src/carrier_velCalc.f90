!> Calculates Fn and Gn used for calculating velocity
	subroutine calcFnGn(u,v,Fn,Gn)
		use size
		use omp_lib
		implicit none
		double precision, dimension(xSize,ySize), intent(in)    :: u, v
		double precision, dimension(xSize,ySize), intent(out)   :: Fn, Gn
		integer											:: i, j, err
		
		!$omp parallel do private(i,j) schedule(dynamic)
		do i=2,xSize-1
			do j=2,ySize-1
				Fn(i,j) = u(i,j)+dt*(((u(i+1,j)-2*u(i,j)+u(i-1,j))/(re*dx*dx))+((u(i,j-1)               &
				          -2.0d0*u(i,j)+u(i,j+1))/(re*dy*dy))-(((u(i,j)+u(i+1,j))*(u(i,j)+u(i+1,j))         &
				          -(u(i-1,j)+u(i,j))*(u(i-1,j)+u(i,j)))/(4.0d0*dx))-((((u(i,j)+u(i,j+1))*(v(i+1,j)  &
				          +v(i,j)))-((u(i,j-1)+u(i,j))*(v(i+1,j-1)+v(i,j-1))))/(4.0d0*dy)))
				Gn(i,j) = v(i,j)+dt*(((v(i+1,j)-2.0d0*v(i,j)+v(i-1,j))/(re*dx*dx))+((v(i,j-1)				&
				          -2.0d0*v(i,j)+v(i,j+1))/(re*dy*dy))-(((v(i,j+1)+v(i,j))*(v(i,j+1)+v(i,j))			&
				          -(v(i,j-1)+v(i,j))*(v(i,j-1)+v(i,j)))/(4.0d0*dy))-((((u(i,j)+u(i,j+1))*(v(i+1,j)  &
				          +v(i,j)))-((u(i-1,j)+u(i-1,j+1))*(v(i,j)+v(i-1,j))))/(4.0d0*dx)))
			end do
		end do
		!$omp end parallel do
	end subroutine calcFnGn
	
	!> Calculates Fn and Gn used for calculating velocity
	subroutine calcFnGnForElement(d)
		use size
		use domain
		use omp_lib
		implicit none
		double precision:: uMiddle,uWest,uNorth,uEast,uSouth,uNorthWest
		double precision:: vMiddle,vWest,vNorth,vEast,vSouth,vSouthEast
		integer											:: n, err
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1,xSizeSol * ySizeSol
			uMiddle = d(n)%u
			uSouth = d(n)%S%u
			uEast = d(n)%E%u
			uNorth = d(n)%N%u
			uWest = d(n)%W%u
			vMiddle = d(n)%v
			vSouth = d(n)%S%v
			vEast = d(n)%E%v
			vWest = d(n)%W%v
			vNorth = d(n)%n%v
			vSouthEast = d(n)%E%S%v
			uNorthWest = d(n)%W%N%u
			
			d(n)%Fn = uMiddle+dt*(((uWest-2*uMiddle+uEast)/(re*dx*dx))+((uSouth               &
				        -2.0d0*uMiddle+uNorth)/(re*dy*dy))-(((uMiddle+uWest)*(uMiddle+uWest)         &
				        -(uEast+uMiddle)*(uEast+uMiddle))/(4.0d0*dx))-((((uMiddle+uNorth)*(vEast  &
				        +vMiddle))-((uSouth+uMiddle)*(vSouthEast+vSouth)))/(4.0d0*dy)))
				        
			d(n)%Gn = vMiddle+dt*(((vEast-2.0d0*vMiddle+vWest)/(re*dx*dx))+((vSouth				&
				        -2.0d0*vMiddle+vNorth)/(re*dy*dy))-(((vNorth+vMiddle)*(vNorth+vMiddle)			&
				         -(vSouth+vMiddle)*(vSouth+vMiddle))/(4.0d0*dy))-((((uMiddle+uNorth)*(vEast  &
				        +vMiddle))-((uWest+uNorthWest)*(vMiddle+vWest)))/(4.0d0*dx)))	          
		end do
		!$omp end parallel do
		
	end subroutine calcFnGnForElement

!> Calculates Qn, used for calculating pressure
	subroutine calcQn(Fn,Gn,Q)
		use size
		use omp_lib
		double precision, dimension(xSize,ySize), intent(in)    :: Fn, Gn
		double precision, dimension(xSize,ySize), intent(out)   :: Q
		integer											:: i, j, err
		
		!$omp parallel do private(i,j) schedule(dynamic)
		do i=2,xSize-1
			do j=2,ySize-1
				Q(i,j) =((Fn(i,j)-Fn(i-1,j)+Gn(i,j)-Gn(i,j-1))/(dt*dx))
			end do
		end do
		!$omp end parallel do
		
	end subroutine calcQn
	
	!> Calculates Qn, used for calculating pressure
	subroutine calcQnForElement(d)
		use size
		use omp_lib
		use domain
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		integer											:: n, err
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1,xSizeSol*ySizeSol
			d(n)%Q =((d(n)%Fn-d(n)%W%Fn+d(n)%Gn-d(n)%S%Gn)/(dt*dx))
		end do
		!$omp end parallel do
		
	end subroutine calcQnForElement

!> Calculates velocity using Fn and pressure
	subroutine calcVel(Fn,Gn,p,u,v)
		use size
		use omp_lib
		double precision, dimension(xSize,ySize), intent(in)	:: Fn, Gn, p
		double precision, dimension(xSize,ySize), intent(out)	:: u, v
		integer											:: i, j, err
		
		!$omp parallel do private(i,j) schedule(dynamic)
		do i=2,xSize-2
			do j=2,ySize-1
				u(i,j) = Fn(i,j) - ( p(i+1,j) - p(i,j) ) * (dt/dx)
			end do
		end do
		!$omp end parallel do
		
		!$omp parallel do private(i,j) schedule(dynamic)
		do i=2,xSize-1
			do j=2,ySize-2
				v(i,j) = Gn(i,j) - ( p(i,j+1) - p(i,j) ) * (dt/dy)
			end do
		end do
		!$omp end parallel do
		
	end subroutine calcVel
	
	!> Calculates velocity using Fn and pressure
	subroutine calcVelForElement(d)
		use size
		use omp_lib
		use domain
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		integer											:: n, err
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1, xSizeSol*ySizeSol
			if(d(n)%xLoc(1)<xSizeSol-1) then
				d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
			end if
				
			if(d(n)%xLoc(2)<ySizeSol-1) then
				d(n)%v = d(n)%Gn - ( d(n)%N%p - d(n)%p ) * (dt/dy)
			end if
		end do
		!$omp end parallel do
		
	
		
	end subroutine calcVelForElement
