module m_ens2mod
contains
subroutine ens2mod(nrpar, nrens, enspar , j)
! Copies initial conditions from ens(:,j) to y before simulation
   use mod_parameters
   implicit none
   integer, intent(in) :: nrpar
   integer, intent(in) :: nrens
   integer, intent(in) :: j
   real,    intent(in) :: enspar(nrpar,nrens)

   Time_to_death     = enspar(1,j)    ! Days to death
   N                 = enspar(2,j)    ! Initial population
   I0                = enspar(3,j)    ! Initial infectious
   R0                = enspar(4,j)    ! Basic Reproduction Number
   D_incbation       = enspar(5,j)    ! Incubation period (Tinc)
   D_infectious      = enspar(6,j)    ! Duration patient is infectious (Tinf)
   D_recovery_mild   = enspar(7,j)    ! Recovery time mild cases (11.1)
   D_recovery_severe = enspar(8,j)    ! Recovery time severe cases (28.6) Lengt of hospital stay
   D_death           = enspar(9,j)    ! Time sick
   D_hospital_lag    = enspar(10,j)   ! Time to hospitalization.
   CFR               = enspar(11,j)   ! Case fatality rate 
   Time              = enspar(12,j)   ! Length of simulation
   p_severe          = enspar(13,j)   ! Hospitalization rate % for severe cases
   Rt                = enspar(14,j)   ! Basic Reproduction Number during intervention
   InterventionTime  = enspar(15,j)   ! Interventions start here
   duration          = enspar(16,j)   ! Duration of measures
end subroutine
end module

