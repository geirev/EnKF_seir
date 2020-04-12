module m_enkfpost
contains 
subroutine enkfpost(ens,enspar,nrens,nt)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   implicit none
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   type(states), intent(inout) :: ens(0:nt,nrens)
   type(params), intent(inout) :: enspar(nrens)
   type(params) avepar
   integer j

! Ensure all parameters are larger than minpar (from infile.in)
   do j=1,nrens
      enspar(j)=max(enspar(j),minpar)
      avepar=avepar+enspar(j)*(1.0/real(nrens))
   enddo

   print *
   print '(a)','Posterior ensemble mean of parameters:'
   print '(100a10)',parnames
   print '(100f10.3)',avepar
   print *
end subroutine
end module
