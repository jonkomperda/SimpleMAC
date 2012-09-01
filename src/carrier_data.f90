module domain
    type element 
        double precision, dimension(2):: X !<Scaled location coordinates
        integer, dimension(2)::xLoc !<unScaled location coordinates 
        double precision:: u, v, p, Fn, Gn, Q !<solution values
        type (element), pointer:: N, S, E, W !<neighboring elements 

    end type element 


end module domain
