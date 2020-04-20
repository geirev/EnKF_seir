module m_inienspar
contains
subroutine inienspar(enspar,nrens)
   use mod_dimensions
   use m_random
   use mod_params
   use mod_parameters
   implicit none
   integer,       intent(in)   :: nrens
   type(params), intent(out)  :: enspar(nrens)
   integer j

   call random(enspar%I0   ,nrens)
   do j=1,nrint
      call random(enspar%R(j) ,nrens)
   enddo
   call random(enspar%Tinf ,nrens)
   call random(enspar%Tinc ,nrens)
   call random(enspar%Trecm,nrens)
   call random(enspar%Trecs,nrens)
   call random(enspar%Thosp,nrens)
   call random(enspar%Tdead,nrens)
   call random(enspar%p_sev,nrens)
   call random(enspar%CFR  ,nrens)

   do j=1,nrens
      enspar(j)   =  p     + parstd * enspar(j) 
      enspar(j)=max(enspar(j),minpar)        ! Ensure positive parameters
      enspar(j)%R=min(enspar(j)%R,rtmax)   ! Ensure Rt < Rtmax
   enddo

end subroutine
end module
