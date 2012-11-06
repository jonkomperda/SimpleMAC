      subroutine writeVTKComplexGeometry(d,timestep)
            use size
            use domain
            implicit none
            
!     ------Input definitions
           type(element), dimension(sizeSol), intent(inout) :: d
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
                  write(unit=51, fmt=textIntText) 'POINTS',sizeSol,' float'
            do n=1,sizeSol
                  write(unit=51, fmt=*) d(n)%X(1),  d(n)%X(2), '0'
            end do
            
!     ------Write out Cells
            cellCount = (boundSideSmall*(solSideSmall-1))+((solSideBig-1)*(solSideSmall-1))
            write(unit=51, fmt=*) 'Cells',cellCount, cellCount*5
            do n=1,sizeSol
                  if(d(n)%xLoc(2)<= boundSideSmall) then
                        if(d(n)%xLoc(1)<solSideSmall) then
                              write(unit=51, fmt=*) 4,n-1,n,n-1+solSideSmall+1,n-1+solSideSmall
                        end if
                  else
                        if(d(n)%xLoc(1)<solSideBig .and. d(n)%xLoc(2)<solSideBig) then
                              write(unit=51, fmt=*) 4,n-1,n,n-1+solSideBig+1,n-1+solSideBig
                        end if
                  end if
            end do
            
!     ------Write out Cell Types
            write(unit=51, fmt=*) 'CELL_TYPES',cellCount
            do n=1, cellCount
                        write(unit=51, fmt=*) 9
            end do
            !go to 84
!     ------write particle data out
            write(unit=51, fmt=textInt) 'POINT_DATA ',sizeSol

!       -----begin with scalars
            write(unit=51, fmt=textLine) 'SCALARS u float'
            write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
                  do n = 1, sizeSol
                  write(unit=51, fmt=*) d(n)%u
                  end do
            write(unit=51, fmt=*)

                  write(unit=51, fmt=textLine) 'SCALARS v float'
                  write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
                  do n=1, sizeSol
                        write(unit=51, fmt=*) d(n)%v
                  end do
                  write(unit=51, fmt=*)
            
                  write(unit=51, fmt=textLine) 'SCALARS p float'
                  write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
                  do n=1, sizeSol
                        write(unit=51, fmt=*) d(n)%p
                  end do
                  !84 continue
                  write(unit=51, fmt=*)
                  close(51)
            
      end subroutine writeVTKComplexGeometry


      

