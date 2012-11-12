        !> Calculates our timestep for variable timestepping algorithm
    subroutine calcTStepComplexGeometry(d,b,t,dtNew)
        use omp_lib
        use size
        use domain
        implicit none
        double precision, intent(out)                   :: dtNew
        double precision                                :: umax1(2),umax2,vmax1(2),vmax2,dtC,dtR, maxValU, maxValV
        integer, intent(in)                             :: t
        integer                                         :: n,i,j,boundSize
        type(element), Target, dimension(sizeSol), intent(inout) :: d
        type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
        
        !We use different step sizes depending on how long we've been running
        if(t .gt. 10) then
            !Calculate first stability condition
            dtR = r*dx*dy*re
            
            !get the maximum value of u and v using worksharing
            !!@todo add parallel processing. it was removed due to do loop replacing maxval(u) and maxval(v) 
            
                !Find Maximum value of u and v in array of elements. 
                maxValU = d(1)%u
                maxValV = d(1)%v
                
                do n=2,sizeSol
                   if(maxValU .lt. d(n)%u) maxValU = d(n)%u
                   if(maxValV .lt. d(n)%v) maxValV = d(n)%v
                end do
                
                do i=1,sides
                    if (i == 1 .or. i == 4) then
                        boundSize = boundSideSmall
                    else if (i == 2 .or. i == 3) then
                        boundSize = boundSideSmall + 1
                    else if (i==5 .or. i==6) then
                        boundSize = boundSideBig
                    end if
                    do j=1,boundSize
                        if(maxValU .lt. b(i,j)%u) maxValU = b(i,j)%u
                        if(maxValV .lt. b(i,j)%v) maxValV = b(i,j)%v
                    end do
                end do
                
                !Calculate using max values determined by above do loop
                umax1 = maxvalU 
                umax2 = maxval(umax1)
                vmax1 = maxvalV
                vmax2 = maxval(vmax1)

            !Calculate second stability condition
            dtC = (1.0d0 / r) / (re * (abs(umax2) + abs(vmax2))**2 )
            
            !use the smaller of the two
            dtNew = min(dtR,dtC)
        else
            !If first few steps then take a very small step
            dtNew = (r/25.0d0)*dx*dy*re
        end if
    end subroutine calcTStepComplexGeometry