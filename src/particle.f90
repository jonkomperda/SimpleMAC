! Written by:  Paul Vesely, BS in Mechanical Engineering Student
! ME 392 Undergraduate Research
! Spring 2013 Semester
! University of Illinois at Chicago
! Advisors: Professor Farzad Mashayek, PhD & Jon Komperda, PhD student


module dispersed
	use omp_lib
	implicit none

	type particle
		double precision 	:: Xp(2)		! Position of particle in 2D.
		double precision	:: Vp1(2)		! Velocity of particle in 2D.
		double precision	:: Vp2(2)	! Velocity of particle from last iteration.
	end type particle

end module dispersed


! This subroutine uses Bilinear Interpolation to determine the fluid velocity at the particles position.
subroutine FluidVelocity(p,d)
	use omp_lib
	use dispersed
	use size
	use domain
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer														:: i,j
	double precision 											:: u00,u10,u01,u11
	double precision 											:: v00,v10,v01,v11
	double precision 											:: x,y

	do i=1,numPoints	! Run through each element one at a time
		if (( associated(d(i)%E ) .eqv. .true.) .and. (associated(d(i)%N) .eqv. .true.)) then

			! Saving the fluid u & v velocities at the four corners of the element
			u00 = d(i)%u
			u10 = d(i)%E%u
			u01 = d(i)%N%u
			u11 = d(i)%N%E%u
			v00 = d(i)%v
			v10 = d(i)%E%v
			v01 = d(i)%N%v
			v11 = d(i)%N%E%v

			do j=1,numParticles	! Search all particles to see which are in the current element
				if ((p(j)%Xp(1) .gt. d(i)%X(1)) .and. (p(j)%Xp(2) .gt. d(i)%X(2))) then
					if ((p(j)%Xp(1) .le. d(i)%E%N%X(1)) .and. (p(j)%Xp(2) .le. d(i)%E%N%X(2))) then

						! Map the location of the particle within the unitized Element
						x = (p(j)%Xp(1) - d(i)%X(1)) / dx	! Position in the x-direction
						y = (p(j)%Xp(2) - d(i)%X(2)) / dx	! Position in the y-direction

						! Using Bilinear Interpolation for a Unit Square to calculate the particles velocity
						p(j)%Vp1(1) = u00*(1-x)*(1-y) + u10*x*(1-y) + u01*(1-x)*y + u11*x*y 		! Velocity in the x-direction
						p(j)%Vp1(2) = v00*(1-x)*(1-y) + v10*x*(1-y) + v01*(1-x)*y + v11*x*y 		! Velocity in the y-direction

					end if
				end if
			end do
		end if
	end do


end subroutine FluidVelocity



! This subroutine initializes the particle into the computational domain by giving them an initial position.
subroutine InitiateParticles(p,d)
	use omp_lib
	use dispersed
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
		end if 
	end do 

end subroutine InitiateParticles



! This subroutine is used for the 1st time step to calculate particle position using
! 	Euler's Method for integration.
subroutine PartMotionEuler(p,d)
	use omp_lib
	use dispersed
	use size
	use domain
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer														:: n


	do n=1,numParticles

		! Integrate to find the particles new location using Euler's Method
		p(n)%Xp(1) = p(n)%Xp(1) + (p(n)%Vp1(1) * dt)
		p(n)%Xp(2) = p(n)%Xp(2) + (p(n)%Vp1(2) * dt)

		! Updating Vp2 for the next iteration.
		p(n)%Vp2(1) = p(n)%Vp1(1)
		p(n)%Vp2(2) = p(n)%Vp1(2)

	end do
	
end subroutine PartMotionEuler



! This subroutine is used for the remaining time steps to calculate particle position.
! 	Adams-Bashforth 2nd Order Method for integration is used.
subroutine PartMotionAdamBash(p,d)
	use omp_lib
	use dispersed
	use size
	use domain
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer														:: n


	do n=1,numParticles

		! Integrate to find the particles new location using Adam-Bashforth 2nd Order Method.
		p(n)%Xp(1) = p(n)%Xp(1) + dt*(3/2*p(n)%Vp1(1) - 1/2*p(n)%Vp2(1))
		p(n)%Xp(2) = p(n)%Xp(2) + dt*(3/2*p(n)%Vp1(2) - 1/2*p(n)%Vp2(2))

		! Updating Vp2 for the iteration.
		p(n)%Vp2(1) = p(n)%Vp1(1)
		p(n)%Vp2(2) = p(n)%Vp1(2)

	end do
	
end subroutine PartMotionAdamBash


subroutine RogueParticles(p,d)
	use omp_lib
	use dispersed
	use size
	use domain
	implicit none

	type(particle), dimension(numParticles), intent(inout)		:: p
	type(element), target, dimension(numPoints), intent(in)		:: d
	integer														:: i

	do i=1,numParticles
		if (p(i)%Xp(1) .ge. d(62)%X(1)) then
			p(i)%Xp(1) = d(62)%X(1) - dx/10
			p(i)%Vp1(1) = 0
			p(i)%Vp2(1) = 0
		end if
	end do


end subroutine RogueParticles