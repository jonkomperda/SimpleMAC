!>This subroutine intializes all values for each element in d for calculation. 
subroutine initialConditions(d)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n

    !This is the main intialization loop
    do n=1,numPoints
        d(n)%u  = 0.0d0
        d(n)%v  = 0.0d0
        d(n)%p  = 0.0d0
        d(n)%Fn = 0.0d0
        d(n)%Gn = 0.0d0
        d(n)%Q  = 0.0d0
    end do
end subroutine initialConditions

!>This subroutine applies the ghost cell boundary condition
subroutine ghostCondition(d)
    use omp_lib
    use size
    use domain
    implicit none
    integer                         :: n
    type(element), Target, dimension(numPoints), intent(inout) :: d

    !This is the main ghost condition loop. 
    do n=1,numPoints

        if(d(n)%b > 0) then !This line can be read as "if this point is a boundary point"
            if((associated(d(n)%N) .eqv. .false.).or.(associated(d(n)%S) .eqv. .false.) )then!if this is a North or South boundary
                d(n)%v = 0.0d0
            else if((associated(d(n)%E) .eqv. .false.).or.(associated(d(n)%W).eqv. .false.) )then!Else if this is a East or West boundary
                d(n)%u = 0.0d0
            end if
        end if

    end do

end subroutine ghostCondition

!>our moving lid condition
subroutine lidCondition(d)
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer                         :: n
    !U and V velocity condition

    do n=1,numPoints
        if(d(n)%b > 0) then !if the point is a boundary 
            if(d(n)%b == 1) then !if its a normal boundary 
                if(associated(d(n)%S) .eqv. .false.) then !if its a southern wall boundary
                    d(n)%u = -d(n)%N%u
                else if(associated(d(n)%E) .eqv. .false.) then !if its an eastern wall boundary
                    d(n)%v = -d(n)%W%v 
                else if(associated(d(n)%W) .eqv. .false.) then !if its a western wall boundary
                    d(n)%v = -d(n)%E%v
                else if(associated(d(n)%N) .eqv. .false.) then !if its a northern wall boundary
                    d(n)%u = -d(n)%S%u
                end if
            else if(d(n)%b == 2) then !if its a lid condition boundary 
                if(associated(d(n)%N) .eqv. .false.) then !if its a northern wall boundary 
                    d(n)%u = 2.0d0 - d(n)%S%u
                else if(associated(d(n)%S) .eqv. .false.) then !if its a southern wall boundary 
                    d(n)%u = 2.0d0 - d(n)%N%u
                else if(associated(d(n)%E) .eqv. .false.) then !if its a eastern wall boundary 
                    d(n)%v = 2.0d0 - d(n)%W%v
                else if(associated(d(n)%W) .eqv. .false.) then !if its a western wall boundary 
                    d(n)%v = 2.0d0 - d(n)%E%v
                end if
            end if
        end if
    end do
    
end subroutine lidCondition


