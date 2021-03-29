module mod_dimensions
   integer, parameter :: na=11          ! Number of agegroups
   integer, parameter :: nrint=3        ! Number of R matrices 
   integer, parameter :: rdim=550       ! Dimension of R(t)
   integer, parameter :: nc=1           ! Number of countries (or states, or etnic groups) in the model
   integer nt                           ! Number of timesteps
   integer nrens                        ! Ensemble size
   integer nrpar                        ! Number of parameters to be estimated
end module mod_dimensions
