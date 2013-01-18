        !> Calculates Fn and Gn used for calculating velocity
    subroutine calcFnGn(d)
        use size
        use domain
        use omp_lib
        implicit none
        double precision:: uMiddle,uWest,uNorth,uEast,uSouth,uNorthWest
        double precision:: vMiddle,vWest,vNorth,vEast,vSouth,vSouthEast
        integer                                         :: n
        type(element), Target, dimension(numPoints), intent(inout) :: d
        
        
        do n=1,numPoints
            if(d(n)%b == 0) then
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
            end if 
        end do

    end subroutine calcFnGn

    !> Calculates Qn, used for calculating pressure
    subroutine calcQn(d)
        use size
        use omp_lib
        use domain
        type(element), Target, dimension(numPoints), intent(inout) :: d
        integer                                         :: n
        
        do n=1,numPoints
            if(d(n)%b == 0) then
                d(n)%Q =((d(n)%Fn-d(n)%W%Fn+d(n)%Gn-d(n)%S%Gn)/(dt*dx))
            end if
        end do
        
    end subroutine calcQn

    !> Calculates velocity using Fn and pressure
    subroutine calcVel(d)
        use size
        use omp_lib
        use domain
        type(element), Target, dimension(numPoints), intent(inout) :: d
        integer                                         :: n, err
        
        do n=1, numPoints
            if(d(n)%b == 0) then
                if(d(n)%E%b == 0) then
                        d(n)%u = d(n)%Fn - ( d(n)%E%p - d(n)%p ) * (dt/dx)
                end if
            end if

            if(d(n)%b == 0) then
                 if(d(n)%N%b == 0) then
                       d(n)%v = d(n)%Gn - ( d(n)%N%p - d(n)%p ) * (dt/dy)
                end if
            end if
        end do

    end subroutine calcVel