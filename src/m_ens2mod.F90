module m_ens2mod
contains
subroutine ens2mod(nrpar, nrens, neq, enspar , j)
! sets the local model parameters used in the function for the ode solver
   use mod_parameters
   implicit none
   integer, intent(in) :: nrpar
   integer, intent(in) :: nrens
   integer, intent(in) :: neq
   integer, intent(in) :: j
   real,    intent(in) :: enspar(nrpar+neq,nrens)

   T2death           = enspar(1,j)    ! Days to death
   N                 = enspar(2,j)    ! Initial population
   I0                = enspar(3,j)    ! Initial infectious
   R0                = enspar(4,j)    ! Basic Reproduction Number
   Tinc              = enspar(5,j)    ! Incubation period (Tinc)
   Tinf              = enspar(6,j)    ! Duration patient is infectious (Tinf)
   Trecm             = enspar(7,j)    ! Recovery time mild cases (11.1)
   Trecs             = enspar(8,j)    ! Recovery time severe cases (28.6) Lengt of hospital stay
   Thosp             = enspar(9,j)   ! Time to hospitalization.
   CFR               = enspar(10,j)   ! Case fatality rate 
   p_severe          = enspar(11,j)   ! Hospitalization rate % for severe cases
   Rt                = enspar(12,j)   ! Basic Reproduction Number during intervention
   Tinterv           = enspar(13,j)   ! Interventions start here
end subroutine
end module

