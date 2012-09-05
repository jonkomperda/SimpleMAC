	!> Calculates our timestep for variable timestepping algorithm
	subroutine calcTStep(u,v,t,dtNew)
		use omp_lib
		use size
		implicit none
		double precision, dimension(xSize,ySize), intent(in)	:: u,v
		integer, intent(in)								:: t
		double precision, intent(out)					:: dtNew
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
			dtNew = min(dtR,dtC)
		else
			!If first few steps then take a very small step
			dtNew = (r/25.0d0)*dx*dy*re
		end if
		write(*,*)'umax1:', umax1, ' umax2:', umax2, ' vmax1:', vmax1, ' vmax2:', vmax2, ' dtc', dtC, ' dtnew:', dtNew
	end subroutine calcTStep
	
	
		!> Calculates our timestep for variable timestepping algorithm
	subroutine calcTStepForElement(d,b,t,dtNew)
		use omp_lib
		use size
		use domain
		implicit none
		double precision, intent(out)					:: dtNew
		double precision								:: umax1(2),umax2,vmax1(2),vmax2,dtC,dtR, maxValU, maxValV
		integer, intent(in)								:: t
		integer                                         :: n,i,j
		double precision, dimension(xSize,ySize)  :: unew,vnew
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		type(element), dimension(sides, sideSize), intent(inout) :: b
		
		!We use different step sizes depending on how long we've been running
		if(t .gt. 10) then
			!Calculate first stability condition
			dtR = r*dx*dy*re
			
			!get the maximum value of u and v using worksharing
			!!@todo add parallel processing. it was removed due to do loop replacing maxval(u) and maxval(v) 
			
			    !Find Maximum value of u and v in array of elements. 
			    !maxValU = d(1)%u
			    !maxValV = d(1)%v
			    
			    !do n=2,ySizeSol*xSizeSol
			       !if(maxValU .lt. d(n)%u) maxValU = d(n)%u
			       !if(maxValV .lt. d(n)%v) maxValV = d(n)%v
			    !end do
			    
			    !do i=1,sides
			    	!do j=1,sideSize
			    		!if(maxValU .lt. b(i,j)%u) maxValU = b(i,j)%u
			    		!if(maxValV .lt. b(i,j)%v) maxValV = b(i,j)%v
			    	!end do
			    !end do
			    
			    !Calculate using max values determined by above do loop
				!umax1 = maxvalU 
				!umax2 = maxval(umax1)
				!vmax1 = maxvalV
				!vmax2 = maxval(vmax1)
				
				!Copied to 2D Array. THIS IS CHEATING, but it proved my theory that the bug is in this method
				!I need to learn more about the fortran maxval() function in order to fix this method properly
				!for element 
				do n=1,ySizeSol*xSizeSol
					unew( (d(n)%xLoc(1) + 1) , (d(n)%xLoc(2) +1) ) = d(n)%u
					vnew( (d(n)%xLoc(1)+1) , (d(n)%xLoc(2)+1)   ) = d(n)%v
				end do
				
				do i=1,sides
			    	do j=1,sideSize
			    		unew( (b(i,j)%xLoc(1)+1) , (b(i,j)%xLoc(2)+1) ) = b(i,j)%u
						vnew(( b(i,j)%xLoc(1)+1) , (b(i,j)%xLoc(2)+1) ) = b(i,j)%v
			    	end do
			    end do
				
				umax1 = maxval(unew)
				umax2 = maxval(umax1)
				vmax1 = maxval(vnew)
				vmax2 = maxval(vmax1)
			
			!Calculate second stability condition
			dtC = (1.0d0 / r) / (re * (abs(umax2) + abs(vmax2))**2 )
			
			!use the smaller of the two
			dtNew = min(dtR,dtC)
		else
			!If first few steps then take a very small step
			dtNew = (r/25.0d0)*dx*dy*re
		end if
		goto 111
		write(*,*) ' '
		write(*,*)' FOR ELEMENT'
		write(*,*)'umax1:', umax1, ' umax2:', umax2, ' vmax1:', vmax1, ' vmax2:', vmax2, ' dtc', dtC, ' dtnew:', dtNew
		111 continue
	end subroutine calcTStepForElement
