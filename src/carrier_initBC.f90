!>This subroutine populates the computational domain [0,0] to [1,1] with zeros in parallel
subroutine initialConditionsComplexGeometry(d,b)
    use omp_lib
    use size
    use domain
    implicit none
    double precision:: firstX
    integer             :: n,y,x,i,j,e,xLimit, boundSize!<n,i and j are counter variables. x and y keep track of position
    type(element), Target, dimension(sizeSol), intent(inout) :: d
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
    y = 0

    
    !loop through solution domain
    e = 0
    xLimit = solSideSmall
    do n=1,sizeSol
        e = e + 1
        x = (mod(e-1,xLimit)+1.0)
        d(n)%xLoc(1) = x
        d(n)%X(1) =  x * dx
        
        if(n>1 .and. xLimit == solSideSmall) then
            if(e == (solSideSmall*boundSideSmall) + 1) then
                e = 1
                xLimit = solSideBig
                x = (mod(e-1,xLimit)+1.0) 
                d(n)%xLoc(1) = x
                d(n)%X(1) =  x * dx
            end if
        end if
        
        if(d(1)%xLoc(1)==d(n)%xLoc(1)) then!<if we see the same x value again we have entered a new row, so update y value
            y = y + 1
        end if
        d(n)%xLoc(2) = y
        d(n)%X(2) = y * dy 
        
        d(n)%u  = 0.0d0
        d(n)%v  = 0.0d0
        d(n)%p  = 0.0d0
        d(n)%Fn = 0.0d0
        d(n)%Gn = 0.0d0
        d(n)%Q  = 0.0d0
        
        !Set N, S, E, & W Neighboring positions
        if(x==1) then!At west boundary
            d(n)%W=>b(6,y+1)
            d(n)%E=>d(n+1)
        else if(x==solSideSmall .and. y<=boundSideSmall) then!At 1st East boundary
            d(n)%E=>b(2,y+1)
            d(n)%W=>d(n-1)
        else if(x==solSideBig .and. y>boundSideSmall) then
            d(n)%E=>b(4,(y-boundSideSmall)+1)
            d(n)%W=>d(n-1)
        else
            d(n)%E=>d(n+1)
            d(n)%W=>d(n-1)
        end if
        
        if(y==1) then!At South boundary
            d(n)%S=>b(1,x+1)
            d(n)%N=>d(n+solSideSmall)
        else if (y==(boundSideSmall+1) .and. x>solSideSmall) then
            d(n)%S=>b(3,x - solSideSmall)
            d(n)%N=>d(n+solSideBig)
        else if (y==solSideBig) then
            d(n)%N=>b(5,x+1)
            d(n)%S=>d(n-solSideBig)
        else if (y<=boundSideSmall .and. y>1) then
            d(n)%N=>d(n+solSideSmall)
            d(n)%S=>d(n-solSideSmall)
        else if (y==(boundSideSmall+1) .and. x<=solSideSmall) then
            d(n)%N=>d(n+solSideBig)
            d(n)%S=>d(n-solSideSmall)
        else if (y>(boundSideSmall+1) .and. y<ySizeSol) then
            d(n)%N=>d(n+solSideBig)
            d(n)%S=>d(n-solSideBig)
        end if   
        
    end do
    
    !loop though boundary domain
    do i=1,sides
        if (i == 1 .or. i == 4) then
            boundSize = boundSideSmall
        else if (i == 2 .or. i == 3) then
            boundSize = boundSideSmall + 1
        else if (i==5 .or. i==6) then
            boundSize = boundSideBig
        end if
        
        do j=1, boundSize
            if(i==1) then!Most South Wall
                b(i,j)%X(1)=(j-1)*dx
                b(i,j)%X(2)=0
                b(i,j)%xLoc(1)=j-1
                b(i,j)%xLoc(2)=0
                
                if(j==1) then
                    b(i,j)%N => b(6,j+1)
                    b(i,j)%E => b(i,j+1)
                else if (j==boundSideSmall) then
                    b(i,j)%N => b(2,2)
                    b(i,j)%W => b(i,j-1)
                else
                    b(i,j)%N => d(j-1)
                    b(i,j)%E => b(i,j+1)
                    b(i,j)%W => b(i,j-1)
                end if
            else if(i==2) then!First East Wall
                b(i,j)%X(1)=(boundSideSmall-1)*dx
                b(i,j)%X(2)=(j-1)*dy
                b(i,j)%xLoc(1)=boundSideSmall-1
                b(i,j)%xLoc(2)=j-1
                
                if(j==1) then
                    b(i,j)%N=>b(i,j+1)
                    b(i,j)%W=>b(1,boundSideSmall-1)
                else if (j==boundSideSmall+1) then
                    b(i,j)%S=>b(i,j-1)
                    b(i,j)%E=>b(3,2)
                    b(i,j)%W=>d(boundSideSmall*solSideSmall)
                    b(i,j)%N=>d((boundSideSmall*solSideSmall) + solSideSmall + 1)
                else
                    b(i,j)%N=>b(i,j+1)
                    b(i,j)%S=>b(i,j-1)
                    b(i,j)%W=>d(solSideSmall*(j-1))
                end if
            else if(i==3) then!2nd South Wall
                b(i,j)%X(1)=(solSideSmall+j)*dx
                b(i,j)%X(2)=boundSideSmall*dy
                b(i,j)%xLoc(1)=solSideSmall+j
                b(i,j)%xLoc(2)=boundSideSmall
                
                if(j==1) then
                    b(i,j)%S=>b(2,boundSideSmall)
                    b(i,j)%E=>b(3,j+1)
                    b(i,j)%W=>d(solSideSmall * boundSideSmall)
                    b(i,j)%N=>d((solSideSmall * boundSideSmall) + solSideSmall + 1)
                else if(j==boundSideSmall+1) then
                    b(i,j)%N => b(4,2)
                    b(i,j)%W => b(i,j-1)
                else
                    b(i,j)%W => b(i,j-1)
                    b(i,j)%E => b(i,j+1)
                    b(i,j)%N => d((solSideSmall * boundSideSmall) + solSideSmall + j)
                end if
            else if(i==4) then!Most East Wall
                b(i,j)%X(1)=(boundSideBig-1)*dx
                b(i,j)%X(2)=(boundSideSmall+(j-1))*dy
                b(i,j)%xLoc(1)=boundSideBig-1
                b(i,j)%xLoc(2)=boundSideSmall+(j-1)
                
                if(j==1) then
                    b(i,j)%N=>b(i,j+1)
                    b(i,j)%W=>b(3,boundSideSmall)
                else if (j==boundSideSmall) then
                    b(i,j)%S=>b(i,j-1)
                    b(i,j)%W=>b(5,boundSideBig-1)
                else
                    b(i,j)%N=>b(i,j+1)
                    b(i,j)%S=>b(i,j-1)
                    b(i,j)%W=>d((solSideSmall * boundSideSmall) + (solSideBig*(j-1)) )
                end if
            else if(i==5) then!North Most Wall
                b(i,j)%X(1)=(j-1)*dx
                b(i,j)%X(2)=(ySize-1)*dy
                b(i,j)%xLoc(1)=j-1
                b(i,j)%xLoc(2)=ySize-1
                
                if(j==1) then 
                    b(i,j)%S => b(6,boundSideBig-1)
                    b(i,j)%E => b(i,j+1)
                else if(j==boundSideBig) then
                    b(i,j)%S => b(4,boundSideSmall-1)
                    b(i,j)%W => b(i,j-1)
                else
                    b(i,j)%W => b(i,j-1)
                    b(i,j)%E => b(i,j+1)
                    b(i,j)%S => d((solSideSmall * boundSideSmall) + (solSideBig * (solSideSmall-1)) + (j-1) ) 
                end if
            else if(i==6) then!West
                b(i,j)%X(1)=0
                b(i,j)%X(2)=(j-1)*dy
                b(i,j)%xLoc(1)=0
                b(i,j)%xLoc(2)=(j-1)
                
                if(j==1) then
                    b(i,j)%N=>b(i,j+1)
                    b(i,j)%E=>b(1,2)
                else if (j==boundSideBig) then
                    b(i,j)%S => b(i,j-1)
                    b(i,j)%E => b(5,2)
                else 
                    b(i,j)%N => b(i,j+1)
                    b(i,j)%S => b(i,j-1)
                    if(j<boundSideSmall+2) then
                        b(i,j)%E => d( (solSideSmall * (j-2)) + 1)
                    else
                        b(i,j)%E =>d((solSideSmall * boundSideSmall) + (solSideBig * (j-(boundSideSmall+2)) + 1 ) )
                    end if
                end if
            end if
            
            b(i,j)%u  = 0.0d0
            b(i,j)%v  = 0.0d0
            b(i,j)%p  = 0.0d0
            b(i,j)%Fn = 0.0d0
            b(i,j)%Gn = 0.0d0
            b(i,j)%Q  = 0.0d0
        end do
    end do
    
end subroutine initialConditionsComplexGeometry

!>This subroutine applies the ghost cell boundary condition
subroutine ghostConditionComplexGeometry(b)
    use omp_lib
    use size
    use domain
    implicit none
    integer                         :: i,j,boundSize
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b

    do i=1,sides
        if (i == 1 .or. i == 4) then
            boundSize = boundSideSmall
        else if (i == 2 .or. i == 3) then
            boundSize = boundSideSmall + 1
        else if (i==5 .or. i==6) then
            boundSize = boundSideBig
        end if
        do j=1,boundSize
            if(i==1 .or. i==3 .or. i==5) then!North south
                b(i,j)%v = 0.0d0
            else if(i==2 .or. i==4 .or. i==6) then
                b(i,j)%u = 0.0d0
            end if
        end do
    end do

end subroutine ghostConditionComplexGeometry


!>our moving lid condition
subroutine lidConditionComplexGeometry(b)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(sides, boundSideBig), intent(inout) :: b
    integer                         :: i,j,boundSize
    !U and V velocity condition

    do i=1,sides
        if (i == 1 .or. i == 4) then
            boundSize = boundSideSmall
        else if (i == 2 .or. i == 3) then
            boundSize = boundSideSmall + 1
        else if (i==5 .or. i==6) then
            boundSize = boundSideBig
        end if
        do j=2, boundSize 
            if(i==1 .or. i==3) then!South
                b(i,j)%u = -b(i,j)%N%u
            end if
            if (i==2 .or. i==4) then!East
                b(i,j)%v = -b(i,j)%W%v          
            end if
            if (i==5) then!North
                b(i,j)%u = -2.0d0 - b(i,j)%S%u
            end if
            if (i==6) then!West
                b(i,j)%v = -b(i,j)%E%v
            end if
        end do
    end do
    
end subroutine lidConditionComplexGeometry
