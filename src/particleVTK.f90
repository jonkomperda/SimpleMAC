!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CONTAINS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         subroutine printVTK(p,numParticles,t)                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!******************************   Subroutine printVTK   ********************************!
!*******  This subroutine is intended to output the particle data into a VTK     *******!
!*******  format file which is readable by VisIt. Each timestep is contained in  *******!
!*******  within a separate file. However VisIt should automatically recognize   *******!
!*******  that these files belong to a single movie (due to their naming).       *******!
!*******                                                                         *******!
!*******  To plot in visit, simply open the VTK files and then select 'molecule' *******!
!*******  plot. Select the particle dataset. You can then adjust the size of     *******!
!*******  the molecule by right clicking its field. To plot vectors, simply      *******!
!*******  select the vector plot type and it will overlay on the molecule.       *******!
!*******  The molecules may be colored according to scalars by selecting the     *******!
!*******  scalar in the molecule plot.                                           *******!
!***************************************************************************************!
!*******  Dependancies:                                                          *******!
!*******  particles : This is the 'droplet' as defined in the 2d code            *******!
!*******  numParticles      : The total number of particles mapped at this time step     *******!
!*******  timestep  : Current integer value of the timestep we are on.           *******!
!*******              used to generate the file name.                            *******!
!***************************************************************************************!
!*********************** Author:    Jon Komperda
!*********************** Date:      Wed May 18 23:19:08 CDT 2011
!*********************** Location:  /
!***************************************************************************************!
subroutine partVTK(p,t,tk)
        use size
        use dispersed
!           use printDefinitions
        implicit none                                                       !implicit is evil!
        
!     ------Input definitions
        integer, intent(in)                     :: t
        type(particle), dimension(numParticles) :: p  
        double precision, intent(in)            :: tk                                   

!     ------Variable definitions
        character(len=32)               :: filename                         !stores the name of particle files
        character(len=5)                :: path     = 'data/'               !the length MUST be exact or it will error
        integer                         :: i                                !this is our counter
        integer                         :: ios
!     ------Format definitions
        character(len=*), parameter     :: textLine = "(A)"
        character(len=*), parameter     :: textInt  = "(A,I9)"
        character(len=*), parameter     :: textIntText = "(A,I9,A)"
        
!     ------Generate a file name
        write(*,*) 't', t
        write(filename, fmt='(a8,i7.7,a4)') 'particle',t,'.vtk'
        
!     ------open the file, make sure its a fresh file at beginning, and if theres an error, print a message
        open(unit=50, file=path//filename, iostat=ios, status="new")
        if ( ios /= 0 ) then 
            write(*,*) 'Delete all contents in /SimpleMAC/data and rerun program', ios
            stop "Error opening particle file, check if directory exists"
        end if

!     ------now write the header portion of the VTK file
        write(unit=50, fmt=textLine) '# vtk DataFile Version 3.0'
        write(unit=50, fmt=textLine) 'Two-Dimensional Particle Tracking'
        write(unit=50, fmt=textLine) 'ASCII'
        write(unit=50, fmt=textLine) 'DATASET POLYDATA'
        write(unit=50, fmt=textLine) 'FIELD FieldData 1'
        write(unit=50, fmt=textLine) 'TIME 1 1 float'
        write(unit=50, fmt=*) tk
        write(unit=50, fmt=*)

!     ------write the particle locations out
        write(unit=50, fmt=textIntText) 'POINTS ',numParticles,' float'
        do i = 1, numParticles
              write(unit=50, fmt='(2e20.10,A)') p(i)%Xp(1), &
 &                                             p(i)%Xp(2), &
 &                                              ' 0.0'
        end do
        write(unit=50, fmt=*)

!     ------write particle data out
        write(unit=50, fmt=textInt) 'POINT_DATA ',numParticles

  !-----now for vectors
        write(unit=50, fmt=textLine) 'VECTORS velocity float'
!            write(unit=50, fmt=textLine) 'LOOKUP_TABLE default'
        do i = 1, numParticles
              write(unit=50, fmt='(2e20.10,A)') p(i)%Vp1(1), &
 &                                             p(i)%Vp1(2), &
 &                                             ' 0.0'
        end do
  
   
!-----time to do some cleaning
        close(50)
      end subroutine partVTK