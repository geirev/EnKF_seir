module m_enkfprep
contains
subroutine enkfprep(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_enkfini
   use m_readagegroups
   use m_pseudo1D
   use m_fixsample1D
   use m_readinfile
   implicit none
   type(states), intent(in) :: ens(0:nt,nrens)
   type(params), intent(in) :: enspar(nrens)
   type(states) aveens
   type(params)  avepar
   real, allocatable :: obspertd(:,:)
   real, allocatable :: obsperth(:,:)
   real, allocatable :: obspertc(:,:)
   real, allocatable :: scaling(:)
   integer i,m,j,ic
   logical, save :: lprt =.true.
 
   if (lprt) then
      print '(a)','Prior ensemble mean of parameters:'
      avepar=0.0
      do j=1,nrens
         avepar=avepar+enspar(j)*(1.0/real(nrens))
      enddo
      print '(100a10)','group ',parnames
      do ic=1,nc
         print '(i8,100f10.4)',ic,avepar%E0(ic), avepar%I0(ic), avepar%Tinf, avepar%Tinc, avepar%Trecm,&
                            avepar%Trecs, avepar%Thosp, avepar%Tdead, avepar%sev(ic), avepar%CFR(ic)
      enddo
      print *
      lprt=.false.
   endif

   print '(a)','Preparing for analysis computation'
   if (lmeascorr) then
      allocate(obspertd(0:nt,nrens))
      allocate(obsperth(0:nt,nrens))
      allocate(obspertc(0:nt,nrens))
   endif

   do ic=1,nc
      if (lmeascorr) then
         print '(2(a,i5))','nt+1=',nt+1, ' n=',nint(real(nt+1)*rh)
         call pseudo1D(obspertd,nt+1,nrens,rh,1.0,nint(real(nt+1)*rh))
         call pseudo1D(obsperth,nt+1,nrens,rh,1.0,nint(real(nt+1)*rh))
         call pseudo1D(obspertc,nt+1,nrens,rh,1.0,nint(real(nt+1)*rh))
         call fixsample1D(obspertd,nt+1,nrens)
         call fixsample1D(obsperth,nt+1,nrens)
         call fixsample1D(obspertc,nt+1,nrens)
      else
         call random(E,nrobs*nrens)
         call fixsample1D(E,nrobs,nrens)
      endif

      R=0.0
      do m=1,nrobs
         if (obs(m)%ic == ic) then
            select case (obs(m)%c)
            case('d')
               R(m,m)=real(nesmda)*min(maxerrd,max(relerrd*obs(m)%d,minerrd))**2
               if (lmeascorr) E(m,:)=sqrt(R(m,m))*obspertd(obs(m)%i,:)
            case('h')
               R(m,m)=real(nesmda)*min(maxerrh,max(relerrh*obs(m)%d,minerrh))**2
               if (lmeascorr) E(m,:)=sqrt(R(m,m))*obsperth(obs(m)%i,:)
            case('c')
               R(m,m)=real(nesmda)*min(maxerrc,max(relerrc*obs(m)%d,minerrc))**2
               if (lmeascorr) E(m,:)=sqrt(R(m,m))*obspertc(obs(m)%i,:)
            case default
               stop 'Measurement type not found'
            end select
            if (.not.lmeascorr) E(m,:)=sqrt(R(m,m))*E(m,:)
            D(m,:)=obs(m)%d+E(m,:)
         endif
      enddo
   enddo

   if (lmeascorr) then
      R=matmul(E,transpose(E))/real(nrens)
      deallocate(obspertd,obsperth)
   endif


   do m=1,nrobs
      i=obs(m)%i
      ic=obs(m)%ic

! ensemble average of state for observation m
      aveens=0.0
      do j=1,nrens
         aveens= aveens + ens(i,j)
      enddo
      aveens=aveens*(1.0/real(nrens))

! Innovations in D' and S'
      select case (obs(m)%c)
      case('d')
         D(m,:) = D(m,:) - Ntot(ic)*ens(i,:)%group(ic)%D
         S(m,:) = Ntot(ic)*( ens(i,:)%group(ic)%D - aveens%group(ic)%D )
      case('h')
         D(m,:) = D(m,:) - Ntot(ic)*(ens(i,:)%group(ic)%Hs + ens(i,:)%group(ic)%Hf)
         S(m,:) = Ntot(ic)*( ens(i,:)%group(ic)%Hs - aveens%group(ic)%Hs &
                &           +ens(i,:)%group(ic)%Hf - aveens%group(ic)%Hf )
      case('c')
         do j=1,nrens
            D(m,j) = D(m,j)- &
                 cfrac*Ntot(ic)*(sum(ens(i,j)%group(ic)%I(1:na)) &
                                    +ens(i,j)%group(ic)%Qm       &
                                    +ens(i,j)%group(ic)%Qs       &
                                    +ens(i,j)%group(ic)%Qf       &
                                    +ens(i,j)%group(ic)%Hs       &
                                    +ens(i,j)%group(ic)%Hf       &
                                    +ens(i,j)%group(ic)%C        &
                                    +ens(i,j)%group(ic)%Rm       &
                                    +ens(i,j)%group(ic)%Rs       &
                                    +ens(i,j)%group(ic)%D        )

            S(m,j) =  Ntot(ic)*( sum(ens(i,j)%group(ic)%I(:)) - sum(aveens%group(ic)%I(:))  &
                                    +ens(i,j)%group(ic)%Qm         -aveens%group(ic)%Qm     &
                                    +ens(i,j)%group(ic)%Qs         -aveens%group(ic)%Qs     &
                                    +ens(i,j)%group(ic)%Qf         -aveens%group(ic)%Qf     &
                                    +ens(i,j)%group(ic)%Hs         -aveens%group(ic)%Hs     &
                                    +ens(i,j)%group(ic)%Hf         -aveens%group(ic)%Hf     &
                                    +ens(i,j)%group(ic)%C          -aveens%group(ic)%C      &
                                    +ens(i,j)%group(ic)%Rm         -aveens%group(ic)%Rm     &
                                    +ens(i,j)%group(ic)%Rs         -aveens%group(ic)%Rs     &
                                    +ens(i,j)%group(ic)%D          -aveens%group(ic)%D      )
         enddo
      case default
         stop 'Measurement type not found'
      end select
!      print '(a,i3,10f10.2)','D',m,D(m,1:10)
!      print '(a,i3,10f10.2)','S',m,S(m,1:10)
      innovation(m)=sum(D(m,:))/real(nrens)
   enddo

! Scaling of matrices
   allocate(scaling(nrobs))
   do m=1,nrobs
      scaling(m)=1./sqrt(R(m,m))
      S(m,:)=scaling(m)*S(m,:)
      E(m,:)=scaling(m)*E(m,:)
      D(m,:)=scaling(m)*D(m,:)
      innovation(m)=scaling(m)*innovation(m)
   enddo

   do j=1,nrobs
   do i=1,nrobs
      R(i,j)=scaling(i)*R(i,j)*scaling(j)
   enddo
   enddo


end subroutine
end module
