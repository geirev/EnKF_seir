module m_enkfprep
contains
subroutine enkfprep(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_enkfini
   use m_agegroups
   use m_pseudo1D
   use m_fixsample1D
   implicit none
   type(states), intent(in) :: ens(0:nt,nrens)
   type(params), intent(in) :: enspar(nrens)
   type(params)  avepar
   real, allocatable :: obspertd(:,:)
   real, allocatable :: obsperth(:,:)
   integer i,m,j
   logical, save :: lprt =.false.
 
   print *
   if (lprt) then
      print '(a)','Prior ensemble mean of parameters:'
      avepar=0.0
      do j=1,nrens
         avepar=avepar+enspar(j)*(1.0/real(nrens))
      enddo
      print '(100a10)',parnames
      print '(100f10.3)',avepar
      print *
      lprt=.false.
   endif

   print '(a)','Preparing for analysis computation'
   if (lmeascorr) then
      allocate(obspertd(0:nt,nrens))
      allocate(obsperth(0:nt,nrens))
      call pseudo1D(obspertd,nt+1,nrens,rh,1.0,nt+10)
      call pseudo1D(obsperth,nt+1,nrens,rh,1.0,nt+10)
      call fixsample1D(obspertd,nrobs,nrens)
      call fixsample1D(obsperth,nrobs,nrens)
   else
      call random(E,nrobs*nrens)
      call fixsample1D(E,nrobs,nrens)
   endif

   R=0.0
   do m=1,nrobs
      R(m,m)=real(nesmda)*min(maxobserr,max(relobserr*dobs(m),minobserr))**2
      if (lmeascorr) then
         select case (cobs(m))
         case('d')
            E(m,:)=sqrt(R(m,m))*obspertd(iobs(m),:)
         case('h')
            E(m,:)=sqrt(R(m,m))*obsperth(iobs(m),:)
         case default
            stop 'Measurement type not found'
         end select
      else
         E(m,:)=sqrt(R(m,m))*E(m,:)
      endif
      D(m,:)=dobs(m)+E(m,:)
   enddo

   if (lmeascorr) then
      R=matmul(E,transpose(E))/real(nrens)
      deallocate(obspertd,obsperth)
   endif

   do m=1,nrobs
      select case (cobs(m))
      case('d')
         D(m,:) = D(m,:)-N*ens(iobs(m),:)%D
         S(m,:) = N*( ens(iobs(m),:)%D - sum(ens(iobs(m),:)%D )*(1.0/real(nrens)) )
      case('h')
         D(m,:) = D(m,:)-N*(ens(iobs(m),:)%Hs + ens(iobs(m),:)%Hf)
         S(m,:) = N*( ens(iobs(m),:)%Hs - sum(ens(iobs(m),:)%Hs)*(1.0/real(nrens)) &
                &    +ens(iobs(m),:)%Hf - sum(ens(iobs(m),:)%Hf)*(1.0/real(nrens)) )
      case default
         stop 'Measurement type not found'
      end select
!      print '(a,f12.2)','N=',N
!      print '(a,i3,10f10.2)','D',m,D(m,1:10)
!      print '(a,i3,10f10.2)','S',m,S(m,1:10)
      innovation(m)=sum(D(m,:))/real(nrens)
   enddo


end subroutine
end module
