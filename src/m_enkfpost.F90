module m_enkfpost
contains 
subroutine enkfpost(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   implicit none
   type(states), intent(inout) :: ens(0:nt,nrens)
   type(params), intent(inout) :: enspar(nrens)
   type(params) avepar
   integer j

! Ensure all parameters are larger than minpar (from infile.in)
   avepar=0.0
   do j=1,nrens
      enspar(j)=max(enspar(j),minpar)
      avepar=avepar+enspar(j)*(1.0/real(nrens))
   enddo

   print *
   print '(a)','Posterior ensemble mean of parameters:'
   print '(100a10)',parnames%I0,parnames%Tinf,parnames%Tinc,parnames%Trecm,parnames%Trecs,parnames%Thosp,&
                                             parnames%Tdead,parnames%p_sev,parnames%CFR
   print '(100f10.4)', pfg%I0,      &
                       pfg%Tinf,    &
                       pfg%Tinc,    &
                       pfg%Trecm,   &
                       pfg%Trecs,   &
                       pfg%Thosp,   &
                       pfg%Tdead,   &
                       pfg%p_sev,   &
                       pfg%CFR 
   print '(100f10.4)', avepar%I0,      &
                       avepar%Tinf,    &
                       avepar%Tinc,    &
                       avepar%Trecm,   &
                       avepar%Trecs,   &
                       avepar%Thosp,   &
                       avepar%Tdead,   &
                       avepar%p_sev,   &
                       avepar%CFR 
   print *
end subroutine
end module
