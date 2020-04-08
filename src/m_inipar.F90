module m_inipar
contains
subroutine inipar(enspar,parstd,nrpar,nrens,neq)
   use m_random
   use mod_parameters
   use m_mod2ens
   implicit none
   integer, intent(in)   :: nrpar
   integer, intent(in)   :: nrens
   integer, intent(in)   :: neq
   real,    intent(in)   :: parstd(nrpar)
   real,    intent(out)  :: enspar(1:nrpar+neq,1:nrens)
   integer j

   call random(enspar(1:nrpar,1:nrens),nrpar*nrens)
   do j=1,nrens
      enspar(1:nrpar,j)=parstd(1:nrpar)*enspar(1:nrpar,j)
      call mod2ens(nrpar,nrens,neq,enspar,j)
   enddo
   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),minpar) ! Ensure positive parameters
   enspar(12,:)=min(enspar(12,:),rtmax)            ! Ensure Rt < 1.0
end subroutine
end module
