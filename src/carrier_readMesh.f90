 !> Reads the header portion of input.vtk file. The purpose is to verify that it is structured correctly and to determine the number of points in the grid, so that we can initialize the solution domain array.
subroutine readHeader()
    use size
    use domain
    implicit none
    character (len=40) :: inputText
    logical :: readError
    readError = .false.
    open(unit=51,file='input.vtk')

    !Verify first line is formatted correctly
    read(51,'(a)') inputText
    if(.not.(inputText .EQ. '# vtk DataFile Version 2.0')) readError = .true.

    !Verify second line is formatted correctly
    read(51,*) inputText
    if(.not.(inputText .EQ. 'Data')) readError = .true.

    !Verify thrid line is formatted correctly
    read(51,*) inputText
    if(.not.(inputText .EQ. 'ASCII')) readError = .true.

    !Verify fourth line is formatted correctly
    read(51,'(a)') inputText
    if(.not.(inputText .EQ. 'DATASET UNSTRUCTURED_GRID')) readError = .true.

    !Verify fifth line is formatted correctly and read in the number of total points
    read(51,*) inputText, numPoints
    if( (.not.(inputText .EQ. 'POINTS')) .or. (numPoints < 0)) readError = .true.

    !If any of the lines read in are not formatted correctly the input data is assumed to be bad and the program is halted.
    if(readError .eqv. .true.) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if
end subroutine readHeader

!>Reads in all of the point data from input.vtk to properly intialize the elements in domain array d. Must be called after readHeader() and and after allocating array d. 
subroutine readPoints(d)
    use size
    use domain
    implicit none
    type(element), Target, intent(inout), dimension(numPoints)    :: d!<solution domain
    integer :: n
    character (len=40) :: inputText

    do n=1,numPoints
        read(51,*) d(n)%xLoc(1), d(n)%xLoc(2) !Read in unscaled location data
        d(n)%X(1) = d(n)%xLoc(1)*dx !Calculate Scaled x location data
        d(n)%X(2) = d(n)%xLoc(2)*dy !Calculate scaled y location data

        !Initialize all pointers to neighboring elements to null for now. We will later use the cell data in the input.vtk to properly set them. 
        d(n)%N => null()
        d(n)%S => null()
        d(n)%E => null()
        d(n)%W => null()
    end do

    !Read data in from the 1st line of the cell data connection part of the input.vtk file, so that we can initialize the cells array. 
    read(51,*) inputText, numCells, numCellConnections
end subroutine readPoints

!>Reads in all of the cell data from input.vtk to properly intialize the elements in cell array c. Must be called after readPoints(d) and after allocating array c.
subroutine readCells(c)
    use size
    use domain
    implicit none
    integer, Target, intent(inout), dimension(numCells,5)        :: c
    integer :: n, cell
    character (len=40) :: inputText

    !Read in all cell connection data
    do n=1,numCells
        read(51,*) c(n,1),c(n,2),c(n,3),c(n,4),c(n,5)
    end do

    !Read in the header portion of the cell type data and verify it is formatted correctly. If not assume input data is bad and halt program. 
    read(51,*) inputText, cell
    if(numCells /= cell .or. .not.(inputText .EQ. 'CELL_TYPES')) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if

    !Read in cell type data and verify all cell types are squares (Only cell type supported for now). If not assume input data is bad and halt program. 
    do n=1,numCells
        read(51,*) cell
        if(cell /= 9) then !The number 9 means cell type is a square. If input does not equal not it is not a square.
            write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
            STOP
        end if
    end do
end subroutine readCells

!>Using the information obtained from the cell data of the input.vtk set the North, South, East, and West neighbor pointer values of the elements in array d. Must be called after readCells(c).
subroutine assignConnectivitiesUsingCells(d,c)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer, Target, intent(inout), dimension(numCells,5)        :: c
    integer :: n,currPoint,currPointNorth,currPointEast,currPointNorthEast

    !Using the formating rules of the cells portion of the unstructured grid vtk file assign the connectivities of the elements.
    do n=1,numCells
        !Every line read in defines a square shaped cell. The first number indicates the number of points in the cell, in this case 4. 
        currPoint = c(n,2) + 1 !The second number is the bottom left point of the square.
        currPointEast = c(n,3) + 1 !The third number is the bottom right point of the square.
        currPointNorthEast = c(n,4) + 1 !The fourth number is the top right point of the square.
        currPointNorth = c(n,5) + 1 !The fifth number is the top left point of the square. 

        if(associated(d(currPoint)%E) .eqv. .false.) then!This can also read as "if the east point is null".
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

!>Determines which points in d are boundaries and which ones are not based upon the data in the input.vtk. Must be run after assignConnectivitiesUsingCells(d,c).
subroutine readBoundary(d)
    use omp_lib
    use size
    use domain
    implicit none
    logical :: readError
    character (len=40) :: inputText
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n, points

    !Ensure data file is formatted correctly. 
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

    !If data was not formatted correctly assume input is bad and halt program. 
    if(readError .eqv. .true.) then
        write(*,*) 'READ ERROR: FILE NOT FORMATTED CORRECTLY FOR MAIN HEADER'
        STOP
    end if

    !Read in the boundary data for each point. This is defined in further detail in size.f90 in the comments for integer b. 
    do n=1,numPoints
        read(51,*) d(n)%b
    end do
end subroutine readBoundary


