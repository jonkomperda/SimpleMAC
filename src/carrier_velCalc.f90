    !> Calculates Fn and Gn used for calculating velocity
    subroutine calcFnGnComplexGeometry(d)
        use size
        use domain
        use omp_lib
        implicit none
        double precision:: uMiddle,uWest,uNorth,uEast,uSouth,uNorthWest
        double precision:: vMiddle,vWest,vNorth,vEast,vSouth,vSouthEast
        integer                                         :: n, err
        type(element), Target, dimension(sizeSol), intent(inout) :: d
        
        
        do n=1,sizeSol
            uMiddle = d(n)%u
            uSouth = d(n)%S%u
            uEast = d(n)%E%u
            uNorth = d(n)%N%u
            uWest = d(n)%W%u
            vMiddle = d(n)%v
            vSouth = d(n)%S%v
            vEast = d(n)%E%v
            vWest = d(n)%W%v
            vNorth = d(n)%N%v
            vSouthEast = d(n)%E%S%v
            uNorthWest = d(n)%W%N%u
            
                d(n)%Fn = uMIddle+dt*(((uEast-2*uMIddle+uWest)/(re*dx*dx))+((uSouth               &
                          -2.0d0*uMIddle+uNorth)/(re*dy*dy))-(((uMIddle+uEast)*(uMIddle+uEast)         &
                          -(uWest+uMIddle)*(uWest+uMIddle))/(4.0d0*dx))-((((uMIddle+uNorth)*(vEast  &
                          +vMIddle))-((uSouth+uMIddle)*(vSouthEast+vSouth)))/(4.0d0*dy)))
                          
                d(N)%Gn = vMIddle+dt*(((vEast-2.0d0*vMIddle+vWest)/(re*dx*dx))+((vSouth             &
                          -2.0d0*vMIddle+vNorth)/(re*dy*dy))-(((vNorth+vMIddle)*(vNorth+vMIddle)            &
                          -(vSouth+vMIddle)*(vSouth+vMIddle))/(4.0d0*dy))-((((uMIddle+uNorth)*(vEast  &
                          +vMIddle))-((uWest+uNorthWest)*(vMIddle+vWest)))/(4.0d0*dx)))          
        end do

    end subroutine calcFnGnComplexGeometry

    
!> Calculates Qn, used for calculating pressure
    subroutine calcQnComplexGeometry(d)
        use size
        use omp_lib
        use domain
        type(element), Target, dimension(sizeSol), intent(inout) :: d
        integer                                         :: n, err
        
        do n=1,sizeSol
            d(n)%Q =((d(n)%Fn-d(n)%W%Fn+d(n)%Gn-d(n)%S%Gn)/(dt*dx))
        end do
        
    end subroutine calcQnComplexGeometry

    
!> Calculates velocity using Fn and pressure
    subroutine calcVelComplexGeometry(d)
        use size
        use omp_lib
        use domain
        type(element), Target, dimension(sizeSol), intent(inout) :: d
        integer                                         :: n, err
        
        do n=1, sizeSol
            if(d(n)%xLoc(1)<solSideSmall .and. d(n)%xLoc(2)<=boundSideSmall) then
                d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
            else if(d(n)%xLoc(1)<solSideBig .and. d(n)%xLoc(2)>boundSideSmall) then
                d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
            end if

            if(d(n)%xLoc(2)<solSideBig) then
                d(n)%v = d(n)%Gn - ( d(n)%N%p - d(n)%p ) * (dt/dy)
            end if
            
        end do

    end subroutine calcVelComplexGeometry




    

    

