		!> Poisson solver for the pressure field
	subroutine poissonComplexGeometry(d,b,tstep)
		use size
		use domain
		implicit none
		type(element), Target, dimension(sizeSol), intent(inout) :: d
		type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
		double precision, parameter						:: rf = 1.60d0
		double precision  								:: change, pold, po, ch
		integer           								:: iter, im, jm, k
		integer           								:: n,i,j,tstep, boundSize

		pold = 0
		iter = 0
		change = 1.0d0
		do while((change .ge. conv))
			iter = iter + 1
			change = 0.0d0
			do n=1,sizeSol
				pold=d(n)%p
				d(n)%p = 0.250d0*((d(n)%W%p+d(n)%S%p+d(n)%E%p+d(n)%N%p)-(d(n)%q*dx*dx))
				d(n)%p = pold + rf*(d(n)%p-pold);
				
				!Calculates change
				if(pold .ne. 0) ch = abs((d(n)%p-pold)/pold)
				
				if(ch .gt. change) then
						change = ch
						im = i
						jm = j
						po = pold
				end if
			end do
			
			!update boundaries on poisson solver
			do i=1,sides
				if (i == 1 .or. i == 4) then
					boundSize = boundSideSmall
				else if (i == 2 .or. i == 3) then
					boundSize = boundSideSmall + 1
				else if (i==5 .or. i==6) then
					boundSize = boundSideBig
				end if
				do j=1,boundSize
					if(i==1 .or. i==3) then!Southern wall
						b(i,j)%p= b(i,j)%N%p - ((2.0*b(i,j)%N%v) / (re*dx))!South
					else if (i==5) then!Northen wall
						b(i,j)%p = b(i,j)%S%p + ((2.0*b(i,j)%S%S%v) / (re*dx))
					else if (i==2 .or. i==4) then!Eastern walls
						b(i,j)%p = b(i,j)%W%p + ((2.0*b(i,j)%W%W%u) / (re*dx))
					else if (i==6) then!Western Wall
						b(i,j)%p = b(i,j)%E%p - ((2.0*b(i,j)%E%u) / (re*dx))
					end if
				end do
			end do
			
			if(iter .gt. itmax) then
				write(*,*)'No convergence for pressure at timestep:',tstep
				goto 62
			end if
		end do
        62  continue		
	end subroutine poissonComplexGeometry

	!> Poisson solver for the pressure field
	subroutine poissonForElement(d,b,tstep)
		use size
		use domain
		implicit none
		type(element), dimension(xSizeSol * ySizeSol), intent(inout) :: d
		type(element), dimension(sides, sideSize), intent(inout) :: b
		double precision, parameter						:: rf = 1.60d0
		double precision  								:: change, pold, po, ch
		integer           								:: iter, im, jm, k
		integer           								:: n,i,j,tstep

		pold = 0
		iter = 0
		change = 1.0d0
		do while((change .ge. conv))
			iter = iter + 1
			change = 0.0d0
			do n=1,xSizeSol*ySizeSol
				pold=d(n)%p
				d(n)%p = 0.250d0*((d(n)%W%p+d(n)%S%p+d(n)%E%p+d(n)%N%p)-(d(n)%q*dx*dx))
				d(n)%p = pold + rf*(d(n)%p-pold);
				
				!Calculates change
				if(pold .ne. 0) ch = abs((d(n)%p-pold)/pold)
				
				if(ch .gt. change) then
						change = ch
						im = i
						jm = j
						po = pold
				end if
			end do
			
			!update boundaries on poisson solver
			do i=1,sides
				do j=1,sideSize
					if(i==1) then!South
						b(i,j)%p= b(i,j)%N%p - ((2.0*b(i,j)%N%v) / (re*dx))!South
					else if (i==3) then!North
						b(i,j)%p = b(i,j)%S%p + ((2.0*b(i,j)%S%S%v) / (re*dx))
					else if (i==2) then!East
						b(i,j)%p = b(i,j)%W%p + ((2.0*b(i,j)%W%W%u) / (re*dx))
					else if (i==4) then!West
						b(i,j)%p = b(i,j)%E%p - ((2.0*b(i,j)%E%u) / (re*dx))
					end if
				end do
			end do
			
			if(iter .gt. itmax) then
				write(*,*)'No convergence for pressure at timestep:',tstep
				goto 55
			end if
		end do
        55  continue		
	end subroutine poissonForElement

!> Poisson solver for the pressure field
	subroutine poisson(q,u,v,tstep,p)
		use size
		implicit none
		double precision, dimension(xSize,ySize), intent(out)   :: p
		double precision, dimension(xSize,ySize), intent(in)    :: q ,u, v
		double precision, parameter						:: rf = 1.60d0
		double precision  								:: change, pold, po, ch
		integer           								:: iter, im, jm, k
		integer           								:: i,j,tstep

		iter = 0
		change = 1.0d0
		do while((change .ge. conv))
			iter = iter + 1
			change = 0.0d0
			
			do i=2,xSize-1
				do j=2,ySize-1
					pold=p(i,j)
					p(i,j) = 0.250d0*((p(i-1,j)+p(i,j-1)+p(i+1,j)+p(i,j+1))-(q(i,j)*dx*dx))
					p(i,j) = pold + rf*(p(i,j)-pold);
					
					!Calculates change
					if(pold .ne. 0) ch = abs((p(i,j)-pold)/pold)
					
					!Stores greatest change value
					if(ch .gt. change) then
						change = ch
						im = i
						jm = j
						po = pold
					end if
				end do
			end do
			
			!update boundaries on poisson solver
			do k=1,xSize
				p(k,1)     = p(k,2)       - ((2.0 * v(k,2))       / (re*dx))!South
                p(k,ySize) = p(k,ySize-1) + ((2.0 * v(k,ySize-2)) / (re*dx))!North
                p(1,k)     = p(2,k)       - ((2.0 * u(2,k))       / (re*dx))!West
                p(xSize,k) = p(xSize-1,k) + ((2.0 * u(xSize-2,k)) / (re*dx))!East
			end do
			
			if(iter .gt. itmax) then
				write(*,*)'No convergence for pressure at timestep:',tstep
				goto 100
			end if
		end do
100     continue		
	end subroutine poisson

!> Parallel poisson solver for the pressure field
	subroutine parPoisson(q,u,v,tstep,p)
		use omp_lib
		use size
		implicit none
		
		double precision, dimension(xSize,ySize), intent(out)   :: p
		double precision, dimension(xSize,ySize), intent(in)    :: q ,u, v
		double precision, parameter								:: rf = 1.60d0
		double precision  								:: pold, po, ch
		integer           								:: iter, im, jm, k
		integer           								:: i,j,tstep,t
		integer											:: rowPer, colPer, numThreads, istart, iend, myid, kstart, kend
		double precision								:: change, changeThread

		iter = 0
		change = 1.0d0
		
		!For OpenMP
		numThreads = 4
		!call omp_set_num_threads(numThreads)
		
		!calculate sizes of blocks
		rowPer = int(ySize/numThreads)
		colPer = int(xSize/numThreads)
		
		!Fork the threads
		!$omp parallel private(myid, istart, iend, i, j, k,t, iter, pold, changeThread) shared(change)
		!myid = omp_get_thread_num()
		
		istart = myid*rowPer + 1
		if(myid == 0) istart = istart + 1
		iend   = (myid*rowPer) + rowPer
		if(myid == numThreads-1) iend = iend - 1
		
		!Main iterative loop
		do while(iter .lt. itmax)
			iter = iter + 1

			!$omp single
			change = 0.0d0
			!$omp end single
			!$omp barrier
			!$omp flush

			changeThread = 0.0d0
			do j=istart,iend
				do i=2,xSize-1
					pold=p(i,j)
					p(i,j) = 0.250d0*((p(i-1,j)+p(i,j-1)+p(i+1,j)+p(i,j+1))-(q(i,j)*dx*dx))
					p(i,j) = pold + rf*(p(i,j)-pold);

					!Calculates change
					if(pold .ne. 0) changeThread = max(changeThread,abs((p(i,j)-pold)/pold))
				end do
			end do

			!calculates change
			!$omp critical
			change = max(change,changeThread)
			!$omp end critical
			!$omp barrier
			
			if(change .lt. conv) exit
			
			!$omp barrier
			
			!update boundaries on poisson solver
			!$omp master
			do k=1,xSize
				p(k,1)     = p(k,2)       - ((2.0d0 * v(k,2))       / (re*dx))
                p(k,ySize) = p(k,ySize-1) + ((2.0d0 * v(k,ySize-2)) / (re*dx))
                p(1,k)     = p(2,k)       - ((2.0d0 * u(2,k))       / (re*dx))
                p(xSize,k) = p(xSize-1,k) + ((2.0d0 * u(xSize-2,k)) / (re*dx))
			end do
			!$omp end master
			
		end do
100		continue
		!$omp barrier
		!$omp end parallel	

	end subroutine parPoisson
