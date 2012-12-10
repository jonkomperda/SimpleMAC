subroutine initialConditions(d)
    use omp_lib
    use size
    use domain
    implicit none
    type(element), Target, dimension(numPoints), intent(inout) :: d
    integer :: n

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

    do n=1,numPoints

        if(d(n)%isBoundary .eqv. .true.) then
            if((associated(d(n)%N) .eqv. .false.).or.(associated(d(n)%S) .eqv. .false.) )then!North south
                d(n)%v = 0.0d0
            else if((associated(d(n)%E) .eqv. .false.).or.(associated(d(n)%W).eqv. .false.) )then!East West
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
        if(d(n)%isBoundary .eqv. .true.) then
            if(associated(d(n)%S) .eqv. .false.) d(n)%u = -d(n)%N%u
            if(associated(d(n)%E) .eqv. .false.) d(n)%v = -d(n)%W%v 
            if(associated(d(n)%N) .eqv. .false.) d(n)%u = 2.0d0 - d(n)%S%u
            if(associated(d(n)%W) .eqv. .false.) d(n)%v = -d(n)%E%v
        end if
    end do
    
end subroutine lidCondition


