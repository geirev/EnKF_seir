module mod_dimensions
   integer, parameter :: na=11          ! Number of agegroups
   integer, parameter :: nrint=3        ! Number of R matrices 
   integer, parameter :: rdim=400      ! Number of days in estimated R
   integer, parameter :: nrpar=rdim+10  ! number of paramters to be estimated
   integer nt
   integer nrens
end module mod_dimensions
