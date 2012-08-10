!> @brief Calculates Fn and Gn used for calculating velocity
!! 
!! @todo fill out the rest of this commentary
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

!> @brief Calculates Qn, used for calculating pressure
!!
!! @todo fill out the rest of this commentary
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

!> @brief Calculates velocity using Fn and pressure
!!
!! U-velocity is calculated from:
!! \f$  u_{n_1}^{i+1/2,j} = F^n_{i+1/2,j} - \frac{\Delta t}{\Delta x} (P^{n+1}_{i+1,j} - P^{n+1}_{i,j})  \f$
!! V-Velocity is calculated from:
!! \f$  v_{n_1}^{i,j+1/2} = G^n_{i,j+1/2} - \frac{\Delta t}{\Delta x} (P^{n+1}_{i,j+1} - P^{n+1}_{i,j})  \f$
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
