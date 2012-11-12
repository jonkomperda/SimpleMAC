subroutine debugPointLocations(d,b)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(sizeSol), intent(inout) :: d
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
    integer             :: n,i,j,boundSize

    do n=1,sizeSol
        write(*,*) 'sol ',n, ' X:', d(n)%xLoc(1), ' Y:', d(n)%xLoc(2)
    end do

    do i=1,sides
        if (i == 1 .or. i == 4) then
            boundSize = boundSideSmall
        else if (i == 2 .or. i == 3) then
            boundSize = boundSideSmall + 1
        else if (i==5 .or. i==6) then
            boundSize = boundSideBig
        end if
            write(*,*)' '
            write(*,*) 'Bound ', i
        do j=1, boundSize
            write(*,*) 'bound ', j, ' X:', b(i,j)%xLoc(1), ' Y:', b(i,j)%xLoc(2)
        end do
    end do

end subroutine debugPointLocations

subroutine debugSolPointConnections(d,b)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(sizeSol), intent(inout) :: d
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
    integer             :: n,i,j

    do n=1,sizeSol

        if(d(n)%xLoc(1) + 1 /= d(n)%E%xLoc(1) .or.  d(n)%xLoc(1)-1 /= d(n)%W%xLoc(1)) then
            write(*,*) 'East or West has wrong X cord at: ', n
        end if

        if(d(n)%xLoc(2) /= d(n)%E%xLoc(2) .or.  d(n)%xLoc(2) /= d(n)%W%xLoc(2)) then
            write(*,*) 'East or West has wrong Y cord at: ', n
        end if

        if(d(n)%xLoc(1) /= d(n)%N%xLoc(1) .or.  d(n)%xLoc(1) /= d(n)%S%xLoc(1)) then
            write(*,*) 'North or South has wrong X cord at: ', n
        end if

        if(d(n)%xLoc(2)+1 /= d(n)%N%xLoc(2) .or.  d(n)%xLoc(2)-1 /= d(n)%S%xLoc(2)) then
            write(*,*) 'North or South has wrong Y cord at: ', n
        end if

    end do

end subroutine debugSolPointConnections

subroutine debugBoundPointConnections(d,b)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(sizeSol), intent(inout) :: d
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
    integer             :: n,i,j,boundSize

    do i=1,sides
        if (i == 1 .or. i == 4) then
            boundSize = boundSideSmall
        else if (i == 2 .or. i == 3) then
            boundSize = boundSideSmall + 1
        else if (i==5 .or. i==6) then
            boundSize = boundSideBig
        end if

        do j=1, boundSize
            if(b(i,j)%xLoc(1) /= 0) then
                if( b(i,j)%xLoc(1)-1 /= b(i,j)%W%xLoc(1) .or. b(i,j)%xLoc(2) /= b(i,j)%W%xLoc(2)) then
                    write(*,*) 'West has wrong cord at: ', i,j
                end if
            end if

            if(b(i,j)%xLoc(2) /= 0 .and. (b(i,j)%xLoc(2) /= boundSideSmall .and. b(i,j)%xLoc(1) < solSideSmall + 1)) then
                if( b(i,j)%xLoc(1) /= b(i,j)%S%xLoc(1) .or. b(i,j)%xLoc(2)-1 /= b(i,j)%S%xLoc(2)) then
                    write(*,*) 'South has wrong cord at: ', i,j
                end if
            end if

            if(b(i,j)%xLoc(2) /= boundSideBig - 1) then
                if(b(i,j)%xLoc(1) /= b(i,j)%N%xLoc(1) .or. b(i,j)%xLoc(2) + 1 /= b(i,j)%N%xLoc(2)) then
                    write(*,*) 'North has wrong cord at: ', i,j 
                end if
            end if

            if((b(i,j)%xLoc(1)/=boundSideSmall-1 .and. b(i,j)%xLoc(2)<boundSideSmall+1)  & 
                .and. (b(i,j)%xLoc(2)>boundSideSmall .and. b(i,j)%xLoc(1) /= boundSideBig-1) ) then
                if(b(i,j)%xLoc(2) /= b(i,j)%E%xLoc(2) .or. b(i,j)%xLoc(1)+1 /= b(i,j)%E%xLoc(1)) then
                    write(*,*) 'East has wrong cord at: ', i,j      
                end if
            end if
        end do


    end do
end subroutine debugBoundPointConnections
