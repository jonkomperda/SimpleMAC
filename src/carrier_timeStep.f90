	!Calculates our timestep for variable timestepping algorithm
	subroutine calcTStep(u,v,t,dt)
		use omp_lib
		use size
		implicit none
		double precision, dimension(:,:), intent(in)	:: u,v
		integer, intent(in)								:: t
		double precision, intent(out)					:: dt
		double precision								:: umax1(2),umax2,vmax1(2),vmax2,dtC,dtR
		
		!We use different step sizes depending on how long we've been running
		if(t .gt. 10) then
			!Calculate first stability condition
			dtR = r*dx*dy*re
			
			!get the maximum value of u and v using worksharing
			!$omp workshare
				umax1 = maxval(u)
				umax2 = maxval(umax1)
				vmax1 = maxval(v)
				vmax2 = maxval(vmax1)
			!$omp end workshare
			
			!Calculate second stability condition
			dtC = (1.0d0 / r) / (re * (abs(umax2) + abs(vmax2))**2 )
			
			!use the smaller of the two
			dt = min(dtR,dtC)
		else
			!If first few steps then take a very small step
			dt = (r/20.0d0)*dx*dy*re
		end if
	end subroutine calcTStep
