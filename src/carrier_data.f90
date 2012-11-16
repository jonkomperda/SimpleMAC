!! All fluid domain variables are stored within this module.
module domain
    !! The element datatype carries all Eulerian frame variables. 
    type element 
        ! Position variables
        double precision, dimension(2)  :: X    !< Position of knot in physical space
        integer, dimension(2)           :: xLoc !< Position of knot in integer cartesian coordinates (i,j)
        logical                         :: isBoundary !<Boolean that indicates if element is on mesh boundary
        ! Solution variables
        double precision                :: u    !< Scalar velocity U in X(1) direction
        double precision                :: v    !< Scalar velocity V in X(2) direction
        double precision                :: p    !< Pressure
        double precision                :: Fn   !< Intermediate variable \f$F_n\f$
        double precision                :: Gn   !< Intermediate variable \f$G_n\f$
        double precision                :: Q    !< Intermediate variable \f$Q\f$
        ! Connectivity variables
        type (element), pointer         :: N    !< Element connected to the north of current elements location
        type (element), pointer         :: S    !< Element connected to the south of current elements location
        type (element), pointer         :: E    !< Element connected to the east of current elements location
        type (element), pointer         :: W    !< Element connected to the west of current elements location
    end type element 
end module domain
