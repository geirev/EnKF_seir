module m_enkfprep
contains 
subroutine enkfprep(ens,enspar,nrpar,nrens,nt,neq)
   use mod_parameters
   use m_enkfini
   use m_agegroups
   use m_pseudo1D
   use m_fixsample1D
   implicit none
   integer, intent(in) :: neq
   integer, intent(in) :: nrpar
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   real,    intent(in) :: ens(0:neq-1,0:nt,nrens)
   real,    intent(in) :: enspar(1:nrpar+neq,nrens)
   real avepar(nrpar)
   integer i,m
   logical, save :: lprt =.true.
   character(len=9) parname(nrpar)
   parname(:)=(/'  T2death','        N','       I0','       R0','     Tinc','     Tinf','    Trecm',&
               &'    Trecs','    Thosp','      CFR',' p_severe','       Rt'/)
      
   print *
   if (lprt) then
      print '(a)','Prior ensemble mean of parameters:'
      do i=1,nrpar
         avepar(i)=sum(enspar(i,:))/real(nrens)
      enddo
      print '(100a10)',parname(:)
      print '(100f10.3)',avepar(:)
      print *
      lprt=.false.
   endif

   print '(a)','Preparing for analysis computation'
   if (lmeascorr) then
      call pseudo1D(E(1:nrobs/2,:),nrobs/2,nrens,rh,1.0,nrobs)
      call pseudo1D(E(nrobs/2+1:nrobs,:),nrobs/2,nrens,rh,1.0,nrobs)
      call fixsample1D(E,nrobs,nrens)
   else 
      call random(E,nrobs*nrens)
   endif


   R=0.0
   do m=1,nrobs
      R(m,m)=min(maxobserr,max(relobserr*dobs(m),minobserr))**2 
      E(m,:)=sqrt(R(m,m))*E(m,:)          

      R(m,m)=R(m,m)*real(nesmda)
      E(m,:)=E(m,:)*sqrt(real(nesmda))

      D(m,:)=dobs(m)+E(m,:)
   enddo

   do m=1,nrobs
      select case (cobs(m))
      case('d')
         D(m,:) = D(m,:)-N*ens(3*na+6,iobs(m),:)
         S(m,:) = N*( ens(3*na+6,iobs(m),:) - sum(ens(3*na+6,iobs(m),:))/real(nrens) )           
      case('h')
         D(m,:) = D(m,:)-N*(ens(3*na+2,iobs(m),:)+ens(3*na+3,iobs(m),:))
         S(m,:) = N*( ens(3*na+2,iobs(m),:) - sum(ens(3*na+2,iobs(m),:))/real(nrens) &
                &    +ens(3*na+3,iobs(m),:) - sum(ens(3*na+3,iobs(m),:))/real(nrens) )
      case default
         stop 'Measurement type not found'
      end select
   enddo

end subroutine
end module
