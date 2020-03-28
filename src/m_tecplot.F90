module m_tecplot
contains
subroutine tecplot(ens,nt,nrens,neq)
   use mod_parameters
   use m_ensave
   use m_ensstd
   implicit none
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   integer, intent(in) :: neq

   integer, parameter  :: nout=6

   real ens(0:neq-1,0:nt,nrens)

   real tmpens(1:nout,0:nt,nrens)
   real ave(nout,0:nt)
   real std(nout,0:nt)

   integer i
   real t,dt

   dt= time/real(nt-1)

   tmpens(1,:,:)=ens(0,:,:)             ! Susceptible
   tmpens(2,:,:)=ens(9,:,:)             ! Dead
   tmpens(3,:,:)=ens(5,:,:)+ens(6,:,:)  ! Hospilitized
   tmpens(4,:,:)=ens(7,:,:)+ens(8,:,:)  ! Recovered
   tmpens(5,:,:)=ens(2,:,:)             ! Infected
   tmpens(6,:,:)=ens(1,:,:)             ! exposed 


   call ensave(tmpens,ave,nout,nt,nrens)
   call ensstd(tmpens,std,ave,nout,nt,nrens)
   std=3.0*std

   open(10,file='susc.dat')
      write(10,*)'TITLE = "Susceptible"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Susceptible"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(1,i), N*std(1,i), N*tmpens(1,i,:)
      enddo
   close(10)

   open(10,file='dead.dat')
      write(10,*)'TITLE = "Dead"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Dead"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(2,i), N*std(2,i), N*tmpens(2,i,:)
      enddo
   close(10)

   open(10,file='hosp.dat')
      write(10,*)'TITLE = "Hospitalized"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Hospitalized"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(3,i), N*std(3,i), N*tmpens(3,i,:)
      enddo
   close(10)

   open(10,file='recov.dat')
      write(10,*)'TITLE = "Recovered"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Recovered"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(4,i), N*std(4,i), N*tmpens(4,i,:)
      enddo
   close(10)

   open(10,file='infec.dat')
      write(10,*)'TITLE = "Infected"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Infected"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(5,i), N*std(5,i), N*tmpens(5,i,:)
      enddo
   close(10)

   open(10,file='expos.dat')
      write(10,*)'TITLE = "Exposed"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Exposed"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(6,i), N*std(6,i), N*tmpens(6,i,:)
      enddo
   close(10)

!      write(10,*)'VARIABLES = "time" "Susceptible" "Dead" "Hospitalized" "Recovered" "Infected" "Exposed"' 
!      write(10,'(a,i5,a,i5,a)')' ZONE T="Average"  F=POINT, I=',nt,', J=1, K=1'
!      write(10,'(20g13.4)')t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1) 
!      print '(20f12.2)',t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1), y(0)+y(9)+y(7)+y(8) 
end subroutine
end module
