      subroutine writeVTK(d,c,timestep)
            use size
            use domain
            implicit none
            
!     ------Input definitions
            type(element), dimension(numPoints), intent(inout) :: d
            integer, dimension(numCells,5), intent(inout) :: c
            integer, intent(in)               :: timestep
            
!     ------Variable definitions
            character(len=32)                 :: filename
            character(len=5)                  :: filePath = 'data/'
            integer                           :: i, ios, j, n
            double precision                  :: x, y
            integer                           :: cellCount

!     ------Format definitions
            character(len=*), parameter     :: textLine = "(A)"
            character(len=*), parameter     :: textInt  = "(A,I7)"
            character(len=*), parameter     :: textIntText = "(A,I7,A)"
            character(len=*), parameter     :: textIIText = "(A,I7,I7,A)"

!     ------Filename generation
            write(filename, fmt='(a4,i7.7,a4)') 'data',timestep,'.vtk'
            open(unit=51, file=filePath//filename, iostat=ios, status="new")
            
!     ------now write the header portion of the VTK filel
            write(unit=51, fmt=textLine) '# vtk DataFile Version 2.0'
            write(unit=51, fmt=textLine) 'Data'
            write(unit=51, fmt=textLine) 'ASCII'
            write(unit=51, fmt=textLine) 'DATASET UNSTRUCTURED_GRID'
            write(unit=51, fmt=*)
            
!     ------Write out Points
            write(unit=51, fmt=textIntText) 'POINTS',numPoints,' float'

            do n=1,numPoints
                  write(unit=51, fmt=*) d(n)%xLoc(1),  d(n)%xLoc(2), '0'
            end do
            
!     ------Write out Cells
            write(unit=51, fmt=*) 'Cells',numCells, numCellConnections
            do n=1,numCells
                  write(unit=51, fmt=*) c(n,1),c(n,2),c(n,3),c(n,4),c(n,5)
            end do
            
!     ------Write out Cell Types
            write(unit=51, fmt=*) 'CELL_TYPES',numCells
            do n=1, numCells
                  write(unit=51, fmt=*) 9
            end do
!     ------write particle data out
            write(unit=51, fmt=textInt) 'POINT_DATA ',numPoints

!       -----begin with scalars
            write(unit=51, fmt=textLine) 'SCALARS u float'
            write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
            do n = 1, numPoints
                  write(unit=51, fmt=*) d(n)%u
            end do
            write(unit=51, fmt=*)

            write(unit=51, fmt=textLine) 'SCALARS v float'
            write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
            do n=1, numPoints
                  write(unit=51, fmt=*) d(n)%v
            end do
            write(unit=51, fmt=*)  
            write(unit=51, fmt=textLine) 'SCALARS p float'
            write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
            do n=1, numPoints
                  write(unit=51, fmt=*) d(n)%p
            end do

                  write(unit=51, fmt=*)
                  close(51)
            
      end subroutine writeVTK