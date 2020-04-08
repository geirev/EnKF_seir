module m_mod2ens
contains
subroutine mod2ens(nrpar, nrens, neq, enspar , j)
! Copies initial conditions from ens(:,j) to y before simulation
   use mod_parameters
   implicit none
   integer, intent(in) :: nrpar
   integer, intent(in) :: nrens
   integer, intent(in) :: neq
   integer, intent(in) :: j
   real,    intent(inout) :: enspar(nrpar+neq,nrens)

   enspar(1,j)   =  T2death   + enspar(1,j)  ! Days to death
   enspar(2,j)   =  N         + enspar(2,j)  ! Initial population
   enspar(3,j)   =  I0        + enspar(3,j)  ! Initial infectious
   enspar(4,j)   =  R0        + enspar(4,j)  ! Basic Reproduction Number
   enspar(5,j)   =  Tinc      + enspar(5,j)  ! Incubation period (Tinc)
   enspar(6,j)   =  Tinf      + enspar(6,j)  ! Duration patient is infectious (Tinf)
   enspar(7,j)   =  Trecm     + enspar(7,j)  ! Recovery time mild cases (11.1)
   enspar(8,j)   =  Trecs     + enspar(8,j)  ! Recovery time severe cases (28.6) Lengt of hospital stay
   enspar(9,j)   =  Thosp     + enspar(9,j)  ! Time to hospitalization.
   enspar(10,j)  =  CFR       + enspar(10,j) ! Case fatality rate 
   enspar(11,j)  =  p_severe  + enspar(11,j) ! Hospitalization rate % for severe cases
   enspar(12,j)  =  Rt        + enspar(12,j) ! Basic Reproduction Number during intervention
   enspar(13,j)  =  Tinterv   + enspar(13,j) ! Interventions start here
end subroutine
end module
