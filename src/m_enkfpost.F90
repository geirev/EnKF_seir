module m_enkfpost
contains 
subroutine enkfpost(ens,enspar,nrpar,nrens,nt,neq)
   use mod_parameters
   implicit none
   integer, intent(in) :: neq
   integer, intent(in) :: nrpar
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   real,    intent(inout) :: ens(0:neq-1,0:nt,nrens)
   real,    intent(inout) :: enspar(1:nrpar+neq,nrens)
   integer i

! Ensure all parameters are larger than minpar (from infile.in)
   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),minpar)

! Ensure all variables are larger than 0.0
   ens(0:neq-1,0,:)=max(enspar(nrpar+1:nrpar+neq,:),0.0)

   print *
   print '(a)','Posterior ensemble parameters:'
   do i=1,3
      print '(i2,100g10.3)',i, enspar(1:nrpar,i)
   enddo
   print '(a)','Posterior ensemble initial conditions:'
   do i=1,2
      print '(i1,a)',i,':'
      print '(10g12.3)',N*ens(:,0,i)
   enddo 
   print *
end subroutine
end module
