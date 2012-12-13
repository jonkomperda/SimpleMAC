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
    end do
    read(51,*) inputText, numCells, numCellConnections
end subroutine readPoints

subroutine readCells(c)
    use size
    use domain
    implicit none
    integer, Target, intent(inout), dimension(numCells,5)        :: c
    integer :: n, cell
    character (len=40) :: inputText
    do n=1,numCells
        read(51,*) c(n,1),c(n,2),c(n,3),c(n,4),c(n,5)
    end do

    read(51,*) inputText, cell
    if(numCells /= cell .or. .not.(inputText .EQ. 'CELL_TYPES')) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if
    do n=1,numCells
        read(51,*) cell
        if(cell /= 9) then
            write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
            STOP
        end if
    end do
end subroutine readCells

subroutine assignConnectivitiesUsingCells(d,c)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer, Target, intent(inout), dimension(numCells,5)        :: c
    integer :: n,currPoint,currPointNorth,currPointEast,currPointNorthEast
    do n=1,numCells
        currPoint = c(n,2) + 1
        currPointEast = c(n,3) + 1
        currPointNorthEast = c(n,4) + 1
        currPointNorth = c(n,5) + 1

        if(associated(d(currPoint)%E) .eqv. .false.) then
            d(currPoint)%E => d(currPointEast)
        end if
        
        if(associated(d(currPoint)%N) .eqv. .false.) then
            d(currPoint)%N => d(currPointNorth)
        end if

        if(associated(d(currPointEast)%W) .eqv. .false.) then
            d(currPointEast)%W => d(currPoint)
        end if

        if(associated(d(currPointEast)%N) .eqv. .false.) then
            d(currPointEast)%N=> d(currPointNorthEast)
        end if
        
        if(associated(d(currPointNorth)%S) .eqv. .false.) then
            d(currPointNorth)%S => d(currPoint)
        end if

        if(associated(d(currPointNorth)%E) .eqv. .false.) then
            d(currPointNorth)%E => d(currPointNorthEast)
        end if

        if(associated(d(currPointNorthEast)%W) .eqv. .false.) then
            d(currPointNorthEast)%W => d(currPointNorth)
        end if

        if(associated(d(currPointNorthEast)%S) .eqv. .false.) then
            d(currPointNorthEast)%S => d(currPointEast)
        end if


    end do
end subroutine assignConnectivitiesUsingCells


subroutine readBoundary(d)
    use omp_lib
    use size
    use domain
    implicit none
    logical :: readError
    character (len=40) :: inputText
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n, points

    readError = .false.
    read(51,*) inputText, points
    if(.not.(inputText .EQ. 'POINT_DATA')) then
        readError = .true.
    end if
    if(numPoints /= points) readError = .true.

    read(51,'(a)') inputText
    if(.not.(inputText .EQ. 'SCALARS boundary float')) readError = .true.

    read(51,'(a)') inputText
    if(.not.(inputText .EQ. 'LOOKUP_TABLE default')) readError = .true.

    if(readError .eqv. .true.) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if

    do n=1,numPoints
        read(51,*) d(n)%b
    end do
end subroutine readBoundary


