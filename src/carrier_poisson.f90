	subroutine poisson(q,u,v,tstep,p)
		use size
		implicit none
		double precision, dimension(:,:), intent(out)   :: p
		double precision, dimension(:,:), intent(in)    :: q ,u, v
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
				p(k,1)     = p(k,2)       - ((2.0 * v(k,2))       / (re*dx))
                p(k,ySize) = p(k,ySize-1) + ((2.0 * v(k,ySize-2)) / (re*dx))
                p(1,k)     = p(2,k)       - ((2.0 * u(2,k))       / (re*dx))
                p(xSize,k) = p(xSize-1,k) + ((2.0 * u(xSize-2,k)) / (re*dx))
			end do
			
			if(iter .gt. itmax) then
				write(*,*)'No convergence for pressure at timestep:',tstep
				goto 100
			end if
		end do
100     continue		
	end subroutine poisson
