module m_inienspar
contains
subroutine inienspar(enspar)
   use mod_dimensions
   use m_readinfile
   use m_random
   use m_pseudo1D
   use m_fixsample1D
   use mod_params
   use mod_parameters
   implicit none
   type(params), intent(out)  :: enspar(nrens)
   integer j,ia,ib,isize,ic

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Simulating the ensembles of uncertain model parameters (m_inienspar.F90)'

   do ic=1,nc
      call random(enspar%E0(ic)   ,nrens)
      call random(enspar%I0(ic)   ,nrens)
   enddo

   if (nint(Tinterv(1)) > rdim) then
      print *,'Tinterv(1)) > rdim: consider increasing rdim=',rdim,' in mod_dimensions to be larger than: ', nint(Tinterv(1))
   endif
   if (nint(Tinterv(2)) > rdim) then
      print *,'Tinterv(2)) > rdim: consider increasing rdim=',rdim,' in mod_dimensions to be larger than: ', nint(Tinterv(2))
   endif

   if (lrtime) then
! simulate continous random functions for R(t) ensemble
      do ic=1,nc
         do j=1,nrens
            ia=0
            ib=rdim
            isize=ib-ia+1  
            if (j==1)  print '(tr2,a,2i5,a,i5,a,i6)','ia to ib:',ia,ib,'  rdim=',isize,'  simdim',nint(real(isize)*rdcorr)
            call pseudo1D(enspar(j)%R(ia:ib,ic),isize,1,rdcorr,1.0,nint(real(isize)*rdcorr))
         enddo

! introduce discontinuous R(t) if the prior mean changes over intervention times.
!         ia=nint(Tinterv(1))+0
!         if ( p%R(ia,ic) /= p%R(ia-1,ic) ) then
!            print *,'Decorrelating R(t) over i=',ia-1,' to ', ia
!            ia=Tinterv(1)+0
!            ib=rdim
!            isize=ib-ia+1
!            if (j==1)  print *,'ia to ib:',ia,ib,isize
!            do j=1,nrens
!               if (isize > 0)   call pseudo1D(enspar(j)%R(ia:ib,ic),isize,1,rdcorr,1.0,nint(real(isize)*rdcorr))
!            enddo
!         endif

!         ia=nint(Tinterv(2))+0
!         if ( p%R(ia-1,ic) /= p%R(ia,ic) ) then
!            print *,'Decorrelating R(t) over i=',ia-1,' to ', ia
!            ia=Tinterv(2)+0   ; ib=rdim ; isize=ib-ia+1  ; if (j==1)  print *,'ia to ib:',ia,ib,isize
!            do j=1,nrens
!               if (isize > 0)   call pseudo1D(enspar(j)%R(ia:ib,ic),isize,1,rdcorr,1.0,nint(real(isize)*rdcorr))
!            enddo
!         endif
      enddo
   else
      do ic=1,nc
         call random(enspar%R(0,ic) ,nrens)
         do j=1,nrens
            enspar(j)%R(1:rdim,ic)=enspar(j)%R(0,ic)
         enddo
      enddo
   endif

   call random(enspar%Tinf ,nrens)
   call random(enspar%Tinc ,nrens)
   call random(enspar%Trecm,nrens)
   call random(enspar%Trecs,nrens)
   call random(enspar%Thosp,nrens)
   call random(enspar%Tdead,nrens)
   do ic=1,nc
      call random(enspar%sev(ic),nrens)
      call random(enspar%CFR(ic)  ,nrens)
   enddo

   do j=1,nrens
      enspar(j)=p + parstd * enspar(j)    ! Adds all the simulated noise in enspar(j) to prior
      enspar(j)=max(enspar(j),minpar)     ! Ensure positive parameters
      enspar(j)%R=min(enspar(j)%R,rtmax)  ! Ensure Rt < Rtmax
   enddo
   print '(a)','Finished simulating the ensembles of uncertain model parameters (m_inienspar.F90)'

end subroutine
end module
