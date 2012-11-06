		!> Calculates Fn and Gn used for calculating velocity
	subroutine calcFnGnComplexGeometry(d)
		use size
		use domain
		use omp_lib
		implicit none
		double precision:: uMiddle,uWest,uNorth,uEast,uSouth,uNorthWest
		double precision:: vMiddle,vWest,vNorth,vEast,vSouth,vSouthEast
		integer											:: n, err
		type(element), Target, dimension(sizeSol), intent(inout) :: d
		
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1,sizeSol
			uMiddle = d(n)%u
			uSouth = d(n)%S%u
			uEast = d(n)%E%u
			uNorth = d(n)%N%u
			uWest = d(n)%W%u
			vMiddle = d(n)%v
			vSouth = d(n)%S%v
			vEast = d(n)%E%v
			vWest = d(n)%W%v
			vNorth = d(n)%N%v
			vSouthEast = d(n)%E%S%v
			uNorthWest = d(n)%W%N%u
			
				d(n)%Fn = uMIddle+dt*(((uEast-2*uMIddle+uWest)/(re*dx*dx))+((uSouth               &
				          -2.0d0*uMIddle+uNorth)/(re*dy*dy))-(((uMIddle+uEast)*(uMIddle+uEast)         &
				          -(uWest+uMIddle)*(uWest+uMIddle))/(4.0d0*dx))-((((uMIddle+uNorth)*(vEast  &
				          +vMIddle))-((uSouth+uMIddle)*(vSouthEast+vSouth)))/(4.0d0*dy)))
				          
				d(N)%Gn = vMIddle+dt*(((vEast-2.0d0*vMIddle+vWest)/(re*dx*dx))+((vSouth				&
				          -2.0d0*vMIddle+vNorth)/(re*dy*dy))-(((vNorth+vMIddle)*(vNorth+vMIddle)			&
				          -(vSouth+vMIddle)*(vSouth+vMIddle))/(4.0d0*dy))-((((uMIddle+uNorth)*(vEast  &
				          +vMIddle))-((uWest+uNorthWest)*(vMIddle+vWest)))/(4.0d0*dx)))          
		end do
		!$omp end parallel do
		
	end subroutine calcFnGnComplexGeometry

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
			vNorth = d(n)%N%v
			vSouthEast = d(n)%E%S%v
			uNorthWest = d(n)%W%N%u
			
				d(n)%Fn = uMIddle+dt*(((uEast-2*uMIddle+uWest)/(re*dx*dx))+((uSouth               &
				          -2.0d0*uMIddle+uNorth)/(re*dy*dy))-(((uMIddle+uEast)*(uMIddle+uEast)         &
				          -(uWest+uMIddle)*(uWest+uMIddle))/(4.0d0*dx))-((((uMIddle+uNorth)*(vEast  &
				          +vMIddle))-((uSouth+uMIddle)*(vSouthEast+vSouth)))/(4.0d0*dy)))
				          
				d(N)%Gn = vMIddle+dt*(((vEast-2.0d0*vMIddle+vWest)/(re*dx*dx))+((vSouth				&
				          -2.0d0*vMIddle+vNorth)/(re*dy*dy))-(((vNorth+vMIddle)*(vNorth+vMIddle)			&
				          -(vSouth+vMIddle)*(vSouth+vMIddle))/(4.0d0*dy))-((((uMIddle+uNorth)*(vEast  &
				          +vMIddle))-((uWest+uNorthWest)*(vMIddle+vWest)))/(4.0d0*dx)))          
		end do
		!$omp end parallel do
		
	end subroutine calcFnGnForElement
	
!> Calculates Qn, used for calculating pressure
	subroutine calcQnComplexGeometry(d)
		use size
		use omp_lib
		use domain
		type(element), Target, dimension(sizeSol), intent(inout) :: d
		integer											:: n, err
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1,sizeSol
			d(n)%Q =((d(n)%Fn-d(n)%W%Fn+d(n)%Gn-d(n)%S%Gn)/(dt*dx))
		end do
		!$omp end parallel do
		
	end subroutine calcQnComplexGeometry

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
	subroutine calcVelComplexGeometry(d)
		use size
		use omp_lib
		use domain
		type(element), Target, dimension(sizeSol), intent(inout) :: d
		integer											:: n, err
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1, sizeSol
			if(d(n)%xLoc(1)<solSideSmall .and. d(n)%xLoc(2)<=boundSideSmall) then
				d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
			else if(d(n)%xLoc(1)<solSideBig .and. d(n)%xLoc(2)>boundSideSmall) then
				d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
			end if

			if(d(n)%xLoc(2)<solSideBig) then
				d(n)%v = d(n)%Gn - ( d(n)%N%p - d(n)%p ) * (dt/dy)
			end if
			
		end do
		!$omp end parallel do

	end subroutine calcVelComplexGeometry

	!> Calculates velocity using Fn and pressure
	subroutine calcVelForElement(d)
		use size
		use omp_lib
		use domain
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		integer											:: n, err
		
		!$omp parallel do private(n) schedule(dynamic)
		do n=1, xSizeSol*ySizeSol
			if(d(n)%xLoc(1)<xSizeSol) then
				d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
			end if
				
			if(d(n)%xLoc(2)<ySizeSol) then
				d(n)%v = d(n)%Gn - ( d(n)%N%p - d(n)%p ) * (dt/dy)
			end if
			
		end do
		!$omp end parallel do

	end subroutine calcVelForElement

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
