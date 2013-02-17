module parts
	use omp_lib
	implicit none

	type particle
		double precision 	:: Xp(2)	! Position of particle in 2D.
		double precision	:: Vp(2)	! Velocity of particle in 2D.
		double precision	:: Vfp(2)	! Velocity of fluid at particles location in 2D.
	end type particle

end module parts	



! This subroutine initializes the particle into the computational domain.
subroutine InitiateParticles(p,d)
	use omp_lib
	use parts
	use size 
	use domain
	use randomGenerationLibrary
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer 													:: i
	integer														:: j=0
	double precision 											:: ur1,ur2

	do i=1,numPoints
		if (( associated(d(i)%E ) .eqv. .true.) .and. (associated(d(i)%N) .eqv. .true.)) then
			j=j+1

123			continue			
			ur1=uniformRandom()
			if ((ur1 .gt. 0) .and. (ur1 .lt. 1)) then
				p(j)%Xp(1)=ur1*dx+d(i)%X(1)
			else
				goto 123
			end if 

124			continue
			ur2=uniformRandom()
			if ((ur2 .gt. 0) .and. (ur2 .lt. 1)) then
				p(j)%Xp(2)=ur2*dy+d(i)%X(2)
			else
				goto 124
			end if 
!			write(*,*) 'i=',i,'j=',j,'Xp(1)=',p(j)%Xp(1),'Xp(2)=',p(j)%Xp(2)
		end if 
	end do 

end subroutine InitiateParticles



	
! This subroutine determines which element the particle belongs to.
! If a particle falls on a boundary of an element, it will belong to 
! 	the element to the North or East of the boundary.
subroutine ParticleMotion(p,d)
	use omp_lib
	use parts
	use size
	use domain
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer														:: i,j,k,n


	do i=1,maxSteps

		do j=1,numParticles

			do k=1,numPoints
				if (( associated(d(k)%E ) .eqv. .true.) .and. (associated(d(k)%N) .eqv. .true.)) then
					if((p(j)%Xp(1) .ge. d(k)%X(1)) .and. (p(j)%Xp(2) .ge. d(k)%X(2))) then
						if((p(j)%Xp(1) .lt. d(k)%E%N%X(1)) .and. (p(j)%Xp(2) .lt. d(k)%E%N%X(2))) then
							n = k
						end if
					end if
				end if
			end do
			
			

		end do

	end do
	
end subroutine ParticleMotion