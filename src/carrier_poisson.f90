        !> Poisson solver for the pressure field
    subroutine poisson(d,tstep)
        use size
        use domain
        implicit none
        type(element), Target, dimension(numPoints), intent(inout) :: d
        double precision, parameter                     :: rf = 1.60d0
        double precision                                :: change, pold, po, ch
        integer                                         :: iter, im, jm, k
        integer                                         :: n, tstep

        pold = 0
        iter = 0
        change = 1.0d0
        do while((change .ge. conv))
            iter = iter + 1
            change = 0.0d0
            do n=1,numPoints
                if(d(n)%b == 0) then
                    pold=d(n)%p
                    d(n)%p = 0.250d0*((d(n)%W%p+d(n)%S%p+d(n)%E%p+d(n)%N%p)-(d(n)%q*dx*dx))
                    d(n)%p = pold + rf*(d(n)%p-pold);
                
                    !Calculates change
                    if(pold .ne. 0) ch = abs((d(n)%p-pold)/pold)
                
                    if(ch .gt. change) then
                        change = ch
                        im = d(n)%xLoc(1)
                        jm = d(n)%xLoc(2)
                        po = pold
                    end if
                end if
    
            end do
            
            !update boundaries on poisson solver
            do n=1,numPoints
                if(d(n)%b > 0) then
                    if(associated(d(n)%S) .eqv. .false.) then!Southern wall
                        d(n)%p= d(n)%N%p - ((2.0*d(n)%N%v) / (re*dx))!South
                    else if (associated(d(n)%N) .eqv. .false.) then!Northen wall
                        d(n)%p = d(n)%S%p + ((2.0*d(n)%S%S%v) / (re*dx))
                    else if (associated(d(n)%E) .eqv. .false.) then!Eastern walls
                        d(n)%p = d(n)%W%p + ((2.0*d(n)%W%W%u) / (re*dx))
                    else if (associated(d(n)%W) .eqv. .false.) then!Western Wall
                        d(n)%p = d(n)%E%p - ((2.0*d(n)%E%u) / (re*dx))
                    end if
                end if
            end do
            
            if(iter .gt. itmax) then
                write(*,*)'No convergence for pressure at timestep:',tstep
                goto 57
            end if
        end do
        57  continue        
    end subroutine poisson