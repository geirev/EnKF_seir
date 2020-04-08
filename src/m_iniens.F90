module m_iniens
contains
subroutine iniens(ens,enspar,neq,nrens,nt,nrpar)
   use m_agegroups
   implicit none
   integer, intent(in)   ::  neq
   integer, intent(in)   ::  nrens
   integer, intent(in)   ::  nt
   integer, intent(in)   ::  nrpar
   real,    intent(out)  ::  ens(0:neq-1, 0:nt, 1:nrens)
   real,    intent(inout)::  enspar(1:nrpar+neq,1:nrens)
   integer j
   real I0   ! Initially infected
   
   print '(a)','Simple ensemble initialization'
   do j=1,nrens
      I0=enspar(3,j)     
      ens(0    :   na-1, 0, j) = agegroup(0:na-1)  ! Susceptible na agegroups                 S_i
      ens(na   : 2*na-1, 0, j) = 4*I0/real(na)     ! Exposed     na agegroups                 E_i
      ens(2*na : 3*na-1, 0, j) =   I0/real(na)     ! Infected    na agegroups                 I_i
      ens(3*na         , 0, j) = 0.0               ! Sick Mild                                Q_m
      ens(3*na+1       , 0, j) = 0.0               ! Sick (Severe at home)                    Q_s
      ens(3*na+2       , 0, j) = 0.0               ! Sick (Severe at hospital)                Q_h
      ens(3*na+3       , 0, j) = 0.0               ! Sick (Severe at hospital that will die)  Q_f
      ens(3*na+4       , 0, j) = 0.0               ! Removed_mild   (recovered)               R_m
      ens(3*na+5       , 0, j) = 0.0               ! Removed_severe (recovered)               R_s
      ens(3*na+6       , 0, j) = 0.0               ! Removed_fatal (dead)                     D
      ens(0:na-1 ,0, j) = ens(0:na-1, 0, j) - ens(na:2*na-1, 0, j) - ens(2*na : 3*na-1, 0, j)
   enddo
   ens(:,0,:)=ens(:,0,:)/sum(agegroup(0:na-1))

! Copy initial conditions from ensemble to enspar for later updating
   enspar(nrpar+1:nrpar+neq,:)=ens(0:neq-1,0,:) 
   print *

end subroutine
end module
