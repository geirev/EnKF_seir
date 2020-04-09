module m_enkfprep
contains 
subroutine enkfprep(ens,enspar,nrpar,nrens,nt,neq)
   use mod_parameters
   use m_enkfini
   use m_agegroups
   implicit none
   integer, intent(in) :: neq
   integer, intent(in) :: nrpar
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   real,    intent(in) :: ens(0:neq-1,0:nt,nrens)
   real,    intent(in) :: enspar(1:nrpar+neq,nrens)

   integer i,m

   print '(a)','Prior ensemble parameters:'
   do i=1,2
      print '(i2,100g10.3)',i, enspar(1:nrpar,i)
   enddo
   print '(a)','Prior ensemble initial conditions:'
   do i=1,2
      print '(i1,a)',i,':'
      print '(10g12.3)',N*ens(:,0,i)
   enddo 

   print *
   print '(a)','Preparing for analysis computation'
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
      print '(a,i3,100f10.2)','D:',m,D(m,1:10)
      print '(a,i3,100f10.2)','S:',m,S(m,1:10)
   enddo

end subroutine
end module
