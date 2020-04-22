module m_inienspar
contains
subroutine inienspar(enspar)
   use mod_dimensions
   use m_readinputs
   use m_random
   use m_pseudo1D
   use m_fixsample1D
   use mod_params
   use mod_parameters
   implicit none
   type(params), intent(out)  :: enspar(nrens)
   integer j

   call random(enspar%I0   ,nrens)
   if (lrtime) then
      print *,'lrtime=',lrtime
      do j=1,nrens
         call pseudo1D(enspar(j)%R,rdim+1,1,rdcorr,1.0,rdim+50)
      enddo
   else
      call random(enspar%R(0) ,nrens)
      do j=1,nrens
         enspar(j)%R(1:rdim)=enspar(j)%R(0)
      enddo
   endif

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
      enspar(j)%R=min(enspar(j)%R,rtmax)     ! Ensure Rt < Rtmax
   enddo

end subroutine
end module
