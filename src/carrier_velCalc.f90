!Calculates Fn and Gn used for calculating velocity
	subroutine calcFnGn(u,v,Fn,Gn)
		use size
		use omp_lib
		implicit none
		double precision, dimension(:,:), intent(in)    :: u, v
		double precision, dimension(:,:), intent(out)   :: Fn, Gn
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

!Calculates Qn, used for calculating pressure
	subroutine calcQn(Fn,Gn,Q)
		use size
		use omp_lib
		double precision, dimension(:,:), intent(in)    :: Fn, Gn
		double precision, dimension(:,:), intent(out)   :: Q
		integer											:: i, j, err
		
		!$omp parallel do private(i,j) schedule(dynamic)
		do i=2,xSize-1
			do j=2,ySize-1
				Q(i,j) =((Fn(i,j)-Fn(i-1,j)+Gn(i,j)-Gn(i,j-1))/(dt*dx))
			end do
		end do
		!$omp end parallel do
		
	end subroutine calcQn

!Calculates velocity using Fn and pressure
	subroutine calcVel(Fn,Gn,p,u,v)
		use size
		use omp_lib
		double precision, dimension(:,:), intent(in)	:: Fn, Gn, p
		double precision, dimension(:,:), intent(out)	:: u, v
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
