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
   integer j,ia,ib,isize

   call random(enspar%E0   ,nrens)
   call random(enspar%I0   ,nrens)

   if (nint(Tinterv(1)) > rdim) then
      print *,'Tinterv(1)) > rdim: consider increasing rdim=',rdim,' in mod_dimensions to be larger than: ', nint(Tinterv(1))
   endif
   if (nint(Tinterv(2)) > rdim) then
      print *,'Tinterv(2)) > rdim: consider increasing rdim=',rdim,' in mod_dimensions to be larger than: ', nint(Tinterv(2))
   endif

   if (lrtime) then
! simulate continous random functions for R(t) ensemble
      do j=1,nrens
         ia=0           ; ib=rdim             ; isize=ib-ia+1  ; if (j==1)  print *,'ia to ib:',ia,ib,isize
         call pseudo1D(enspar(j)%R(ia:ib),isize,1,rdcorr,1.0,isize+50)
      enddo

! introduce discontinuous R(t) if the prior mean changes over intervention times.
      ia=nint(Tinterv(1))+1
      if ( p%R(ia) /= p%R(ia-1) ) then
         print *,'Decorrelating R(t) over i=',ia-1,' to ', ia
         ia=Tinterv(1)+1   ; ib=rdim ; isize=ib-ia+1  ; if (j==1)  print *,'ia to ib:',ia,ib,isize
         do j=1,nrens
            if (isize > 0)   call pseudo1D(enspar(j)%R(ia:ib),isize,1,rdcorr,1.0,isize+50)
         enddo
      endif

      ia=nint(Tinterv(2))+1
      if ( p%R(ia-1) /= p%R(ia) ) then
         print *,'Decorrelating R(t) over i=',ia-1,' to ', ia
         ia=Tinterv(2)+1   ; ib=rdim ; isize=ib-ia+1  ; if (j==1)  print *,'ia to ib:',ia,ib,isize
         do j=1,nrens
            if (isize > 0)   call pseudo1D(enspar(j)%R(ia:ib),isize,1,rdcorr,1.0,isize+50)
         enddo
      endif
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
