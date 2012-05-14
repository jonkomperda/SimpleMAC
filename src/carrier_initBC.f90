!This subroutine populates the computational domain [0,0] to [1,1] with zeros in parallel
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

!This subroutine applies the ghost cell boundary condition
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

!our moving lid condition
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
