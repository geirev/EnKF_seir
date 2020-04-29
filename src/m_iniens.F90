module m_iniens
contains
subroutine iniens(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_agegroups
   implicit none
   type(states), intent(out)  ::  ens(0:nt,nrens)
   type(params), intent(in)   ::  enspar(nrens)
   integer j
   real I0   ! Initially infected
   
   print '(a)','Simple ensemble initialization'
   do j=1,nrens
      I0=enspar(j)%I0    
      ens(0,j)%S  = agegroup(1:na)    ! Susceptible na agegroups                 S_i
      ens(0,j)%E  = 4.0*I0/real(na)   ! Exposed     na agegroups                 E_i
      ens(0,j)%I  = I0/real(na)       ! Infected    na agegroups                 I_i
      ens(0,j)%Qm = 0.0               ! Sick Mild                                Q_m
      ens(0,j)%Qs = 0.0               ! Sick (Severe at home)                    Q_s
      ens(0,j)%Qf = 0.0               ! Sick (Severe at hospital)                Q_h
      ens(0,j)%Hs = 0.0               ! Sick (Severe at hospital that will die)  Q_f
      ens(0,j)%Hf = 0.0               ! Sick (Fatal at hospital that will die)   H_f
      ens(0,j)%C  = 0.0               ! Sick (Fatal at Care home that will die)  C
      ens(0,j)%Rm = 0.0               ! Removed_mild   (recovered)               R_m
      ens(0,j)%Rs = 0.0               ! Removed_severe (recovered)               R_s
      ens(0,j)%D  = 0.0               ! Removed_fatal (dead)                     D
      ens(0,j)%S  = ens(0,j)%S - ens(0,j)%E - ens(0, j)%I
      ens(0,j)=ens(0,j)*(1.0/sum(agegroup))
   enddo

   print *

end subroutine
end module
