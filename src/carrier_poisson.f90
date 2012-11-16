        !> Poisson solver for the pressure field
    subroutine poissonMeshReader(d,tstep)
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
                if(d(n)%isBoundary .eqv. .false.) then
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
                if(d(n)%isBoundary .eqv. .true.) then
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
    end subroutine poissonMeshReader       


        !> Poisson solver for the pressure field
    subroutine poissonComplexGeometry(d,b,tstep)
        use size
        use domain
        implicit none
        type(element), Target, dimension(sizeSol), intent(inout) :: d
        type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
        double precision, parameter                     :: rf = 1.60d0
        double precision                                :: change, pold, po, ch
        integer                                         :: iter, im, jm, k
        integer                                         :: n,i,j,tstep, boundSize

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
