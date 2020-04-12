module m_inienspar
contains
subroutine inienspar(enspar,parstd,nrpar,nrens)
   use m_random
   use mod_parameters
   implicit none
   integer, intent(in)   :: nrpar
   integer, intent(in)   :: nrens
   real,    intent(in)   :: parstd(nrpar)
   real,    intent(out)  :: enspar(1:nrpar,1:nrens)
   integer j

   call random(enspar,nrpar*nrens)
   do j=1,nrens
      enspar(1,j)   =  Tdead     + parstd( 1) * enspar(1,j)  ! Days to death
      enspar(2,j)   =  1.0                      ! Not used
      enspar(3,j)   =  I0        + parstd( 3) * enspar(3,j)  ! Initial infectious
      enspar(4,j)   =  R0        + parstd( 4) * enspar(4,j)  ! Basic Reproduction Number
      enspar(5,j)   =  Tinc      + parstd( 5) * enspar(5,j)  ! Incubation period (Tinc)
      enspar(6,j)   =  Tinf      + parstd( 6) * enspar(6,j)  ! Duration patient is infectious (Tinf)
      enspar(7,j)   =  Trecm     + parstd( 7) * enspar(7,j)  ! Recovery time mild cases (11.1)
      enspar(8,j)   =  Trecs     + parstd( 8) * enspar(8,j)  ! Recovery time severe cases (28.6) Lengt of hospital stay
      enspar(9,j)   =  Thosp     + parstd( 9) * enspar(9,j)  ! Time to hospitalization.
      enspar(10,j)  =  CFR       + parstd(10) * enspar(10,j) ! Case fatality rate 
      enspar(11,j)  =  p_severe  + parstd(11) * enspar(11,j) ! Hospitalization rate % for severe cases
      enspar(12,j)  =  Rt        + parstd(12) * enspar(12,j) ! Basic Reproduction Number during intervention
   enddo

   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),minpar) ! Ensure positive parameters
   enspar(12,:)=min(enspar(12,:),rtmax)            ! Ensure Rt < 1.0

end subroutine
end module
