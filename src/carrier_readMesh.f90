subroutine readHeader()
    use size
    use domain
    implicit none
    character (len=40) :: inputText
    logical :: readError
    readError = .false.
    open(unit=51,file='input.vtk')


    read(51,'(a)') inputText
    if(.not.(inputText .EQ. '# vtk DataFile Version 2.0')) readError = .true.

    read(51,*) inputText
    if(.not.(inputText .EQ. 'Data')) readError = .true.

    read(51,*) inputText
    if(.not.(inputText .EQ. 'ASCII')) readError = .true.

    read(51,'(a)') inputText
    if(.not.(inputText .EQ. 'DATASET UNSTRUCTURED_GRID')) readError = .true.

    read(51,*) inputText, numPoints
    if( (.not.(inputText .EQ. 'POINTS')) .or. (numPoints < 0)) readError = .true.

    if(readError .eqv. .true.) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if
end subroutine readHeader

subroutine readPoints(d)
    use size
    use domain
    implicit none
    type(element), Target, intent(inout), dimension(numPoints)    :: d!<solution domain
    integer :: n
    character (len=40) :: inputText

    do n=1,numPoints
        read(51,*) d(n)%xLoc(1), d(n)%xLoc(2)
        d(n)%X(1) = d(n)%xLoc(1)*dx
        d(n)%X(2) = d(n)%xLoc(2)*dy
        d(n)%N => null()
        d(n)%S => null()
        d(n)%E => null()
        d(n)%W => null()
        d(n)%isBoundary = .false.
    end do
    read(51,*) inputText, numCells, numCellConnections
end subroutine readPoints

subroutine readCells(c)
    use size
    use domain
    implicit none
    integer, Target, intent(inout), dimension(numCells,5)        :: c
    integer :: n
    do n=1,numCells
        read(51,*) c(n,1),c(n,2),c(n,3),c(n,4),c(n,5)
    end do
end subroutine readCells

subroutine assignConnectivities(d)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n,m
    do n=1,numPoints
        write(*,*)(n)
        do m=1,numPoints
            if((d(m)%xLoc(1) == d(n)%xLoc(1) + 1) .and. d(m)%xLoc(2) == d(n)%xLoc(2)) then
                d(n)%E => d(m)
            else if((d(m)%xLoc(1) == d(n)%xLoc(1) - 1) .and. d(m)%xLoc(2) == d(n)%xLoc(2)) then
                d(n)%W => d(m)
            else if((d(m)%xLoc(1) == d(n)%xLoc(1)) .and. d(m)%xLoc(2) == d(n)%xLoc(2) + 1) then
                d(n)%N => d(m)
            else if((d(m)%xLoc(1) == d(n)%xLoc(1)) .and. d(m)%xLoc(2) == d(n)%xLoc(2) - 1) then
                d(n)%S => d(m)
            end if
        end do

        if(associated(d(n)%N) .eqv. .false.)  then
            d(n)%isBoundary = .true.
        else if (associated(d(n)%S) .eqv. .false.) then
            d(n)%isBoundary = .true.
        else if (associated(d(n)%E) .eqv. .false.) then
            d(n)%isBoundary = .true.
        else if (associated(d(n)%W) .eqv. .false.) then
            d(n)%isBoundary = .true.
        end if
        
    end do
end subroutine assignConnectivities