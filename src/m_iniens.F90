module m_iniens
contains
subroutine iniens(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_readagegroups
   implicit none
   type(states), intent(out)  ::  ens(0:nt,nrens)
   type(params), intent(in)   ::  enspar(nrens)
   integer j,ic
   
   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Simple ensemble initialization (m_iniens.F90)'
   do j=1,nrens
      do ic=1,nc
         ens(0,j)%group(ic)%S(1:na)  = agegroup(1:na,ic)          ! Susceptible na agegroups                 S_i
         ens(0,j)%group(ic)%E(1:na)  = enspar(j)%E0(ic)/real(na)  ! Exposed     na agegroups                 E_i
         ens(0,j)%group(ic)%I(1:na)  = enspar(j)%I0(ic)/real(na)  ! Infected    na agegroups                 I_i
         ens(0,j)%group(ic)%Qm = 0.0                    ! Sick Mild                                Q_m
         ens(0,j)%group(ic)%Qs = 0.0                    ! Sick (Severe at home)                    Q_s
         ens(0,j)%group(ic)%Qf = 0.0                    ! Sick (Severe at hospital)                Q_h
         ens(0,j)%group(ic)%Hs = 0.0                    ! Sick (Severe at hospital that will die)  Q_f
         ens(0,j)%group(ic)%Hf = 0.0                    ! Sick (Fatal at hospital that will die)   H_f
         ens(0,j)%group(ic)%C  = 0.0                    ! Sick (Fatal at Care home that will die)  C
         ens(0,j)%group(ic)%Rm = 0.0                    ! Removed_mild   (recovered)               R_m
         ens(0,j)%group(ic)%Rs = 0.0                    ! Removed_severe (recovered)               R_s
         ens(0,j)%group(ic)%D  = 0.0                    ! Removed_fatal (dead)                     D

         ens(0,j)%group(ic)%S  = ens(0,j)%group(ic)%S - ens(0,j)%group(ic)%E - ens(0,j)%group(ic)%I

         ens(0,j)%group(ic)=ens(0,j)%group(ic)*(1.0/Ntot(ic))

      enddo
   enddo
   print '(a)','Simple ensemble initialization completed (m_iniens.F90)'

end subroutine
end module
