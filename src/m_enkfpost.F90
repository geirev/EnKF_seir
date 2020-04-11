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
   real avepar(nrpar)
   integer i
   character(len=9) parname(nrpar)
   parname(:)=(/'  T2death','        N','       I0','       R0','     Tinc','     Tinf','    Trecm',&
               &'    Trecs','    Thosp','      CFR',' p_severe','       Rt'/)

! Ensure all parameters are larger than minpar (from infile.in)
   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),minpar)

! Ensure all variables are larger than 0.0
   ens(0:neq-1,0,:)=max(enspar(nrpar+1:nrpar+neq,:),0.000000)

   print *
   print '(a)','Posterior ensemble mean of parameters:'
   do i=1,nrpar
      avepar(i)=sum(enspar(i,:))/real(nrens)
   enddo
   print '(100a10)',parname(:)
   print '(100f10.3)',avepar(:)
   print *
end subroutine
end module
