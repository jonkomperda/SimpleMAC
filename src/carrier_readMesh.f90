subroutine readInMesh(d,c)
    use omp_lib
    use size
    use domain
    implicit none
    character (len=12) :: inputText
    logical :: readError
    integer :: n
    type(element), allocatable, Target, intent(inout), dimension(:)    :: d!<solution domain
    integer, allocatable, Target, intent(inout), dimension(:,:)        :: c
    readError = .false.
    open(10,file='inputMesh.vtk')


    read(10,'(a)') inputText
    if(.not.(inputText .EQ. '# vtk DataFile Version 2.0')) readError = .true.

    read(10,*) inputText
    if(.not.(inputText .EQ. 'Data')) readError = .true.

    read(10,*) inputText
    if(.not.(inputText .EQ. 'ASCII')) readError = .true.

    read(10,'(a)') inputText
    if(.not.(inputText .EQ. 'DATASET UNSTRUCTURED_GRID')) readError = .true.

    read(10,*) inputText, numPoints
    if( (.not.(inputText .EQ. 'POINTS')) .or. (numPoints < 0)) readError = .true.

    if(readError .EQV. .FALSE.) then
        allocate(  d(numPoints) )
        do n=1,numPoints
            read(10,*) d(n)%xLoc(1), d(n)%xLoc(2)
            d(n)%X(1) = d(n)%xLoc(1)*dx
            d(n)%X(2) = d(n)%xLoc(2)*dy
            d(n)%N => null()
            d(n)%S => null()
            d(n)%E => null()
            d(n)%W => null()
            d(n)%isBoundary = .false.
        end do
    else
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER OR POINTS HEADER'
    end if


    read(10,*) inputText, numCells, numCellConnections
    if( (.not.(inputText .EQ. 'Cells')) .or. (numCells < 0) .or. (numCellConnections<0)) readError = .true.
    if(readError .EQV. .FALSE.) then
        allocate( c(numCells,5))
        do n=1,numCells
            read(10,*) c(n,1),c(n,2),c(n,3),c(n,4),c(n,5)
        end do
    else
         write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR CELLS'
    end if

    close(10)
end subroutine readInMesh

subroutine assignConnectivities(d)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n,m

    do n=1,numPoints
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