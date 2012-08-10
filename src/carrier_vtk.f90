!> Subroutine that plots out the data in a VTK format for plotting in VisIt Visualization tool.
!! This format is also compatible with Visualization Toolkit as well as paraview, although it has
!! not been tested.
      subroutine writeVTK(u,v,p,timestep)
            use size
            implicit none
            
!     ------Input definitions
            double precision, intent(in)      :: u(ySize,xSize),v(ySize,xSize),p(ySize,xSize)
            integer, intent(in)               :: timestep
            
!     ------Variable definitions
            character(len=32)                 :: filename
            character(len=5)                  :: filePath = 'data/'
            integer                           :: i, ios, j
			double precision                  :: x, y

!     ------Format definitions
            character(len=*), parameter     :: textLine = "(A)"
            character(len=*), parameter     :: textInt  = "(A,I7)"
            character(len=*), parameter     :: textIntText = "(A,I7,A)"
            character(len=*), parameter     :: textIIText = "(A,I7,I7,A)"

!     ------Filename generation
            write(filename, fmt='(a4,i7.7,a4)') 'data',timestep,'.vtk'
            open(unit=51, file=filePath//filename, iostat=ios, status="new")
            
!     ------now write the header portion of the VTK file
            write(unit=51, fmt=textLine) '# vtk DataFile Version 3.0'
            write(unit=51, fmt=textLine) 'Data'
            write(unit=51, fmt=textLine) 'ASCII'
            write(unit=51, fmt=textLine) 'DATASET RECTILINEAR_GRID'
            write(unit=51, fmt=*)
            
!     ------Write out mesh information
            write(unit=51, fmt=textIIText)  'DIMENSIONS ',xSize-1,ySize-1,' 1'
            
            !---- X Coords plotted out
            write(unit=51, fmt=textIntText) 'X_COORDINATES ',xSize-1,' float'
			x = 0.0d0
            do i=1,xSize-1
                  write(unit=51, fmt='(11e20.10)') x
				  x = x+dx
            end do
            
            !---- Y Coords plotted out
            write(unit=51, fmt=textIntText) 'Y_COORDINATES ',ySize-1,' float'
			y = 0.0d0
            do i=1,ySize-1
                  write(unit=51, fmt='(11e20.10)') y
				  y = y + dy
            end do
            
            !---- Z Coords plotted out / placeholder for 3D code
            write(unit=51, fmt=textLine) 'Z_COORDINATES 1 float'
            write(unit=51, fmt=textLine) '0.0000000000E+00' 
            write(unit=51, fmt=textLine)
            
!     ------write particle data out
            write(unit=51, fmt=textInt) 'CELL_DATA ',(xSize-2)*(ySize-2)

            !-----begin with scalars
            write(unit=51, fmt=textLine) 'SCALARS u float'
            write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
			do j = 2, ySize-1
            	do i = 2, xSize-1
                  write(unit=51, fmt=*) u(i,j)
            	end do
			end do
            write(unit=51, fmt=*)

		write(unit=51, fmt=textLine) 'SCALARS v float'
		write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
		do j=2, ySize-1
			do i=2,xSize-1
				write(unit=51, fmt=*) v(i,j)
			end do
		end do
		write(unit=51, fmt=*)
		
		write(unit=51, fmt=textLine) 'SCALARS p float'
		write(unit=51, fmt=textLine) 'LOOKUP_TABLE default'
		do j=2, ySize-1
			do i=2, xSize-1
				write(unit=51, fmt=*) p(i,j)
			end do
		end do
		write(unit=51, fmt=*)
		close(51)
            
      end subroutine writeVTK
