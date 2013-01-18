!> All fluid domain variables are stored within this module.
module domain
    !> The element datatype carries all Eulerian frame variables. It is intended to represent a physical point on the grid. 
    type element 
        ! Position variables
        double precision, dimension(2)  :: X    !< Position of knot in physical space 
        double precision, dimension(2)  :: xLoc !< Position of knot in cartesian coordinates (i,j)
        integer                         :: b    !<Indicates if elemnt is a boundary. 0: Interior Point 1: Normal Boundary 2: Boundary with Lid Condition 
        ! Solution variables
        double precision                :: u    !< Scalar velocity U in the x, or X(1), direction
        double precision                :: v    !< Scalar velocity V in the y, or X(2), direction
        double precision                :: p    !< Pressure
        double precision                :: Fn   !< Intermediate variable \f$F_n\f$
        double precision                :: Gn   !< Intermediate variable \f$G_n\f$
        double precision                :: Q    !< Intermediate variable \f$Q\f$
        ! Connectivity variables
        type (element), pointer         :: N    !< Element located to the north of current elements location in the grid
        type (element), pointer         :: S    !< Element located to the south of current elements location in the grid
        type (element), pointer         :: E    !< Element located to the east of current elements location in the grid
        type (element), pointer         :: W    !< Element located to the west of current elements location in the grid
    end type element 
end module domain
