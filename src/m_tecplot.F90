module m_tecplot
contains
subroutine tecplot(ens,enspar,nt,nrens,neq,nrpar,pri)
   use mod_parameters
   use m_ensave
   use m_ensstd
   implicit none
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   integer, intent(in) :: neq
   integer, intent(in) :: nrpar
   integer, intent(in) :: pri

   integer, parameter  :: nout=6

   real ens(0:neq-1,0:nt,nrens)
   real enspar(nrpar,nrens)

   real tmpens(1:nout,0:nt,nrens)
   real ave(nout,0:nt)
   real std(nout,0:nt)

   integer i,j
   real t,dt
   character(len=1) tag

   write(tag,'(i1.1)')pri
   dt= time/real(nt-1)

   tmpens(1,:,:)=ens(0,:,:)             ! Susceptible
   tmpens(2,:,:)=ens(9,:,:)             ! Dead
   tmpens(3,:,:)=ens(5,:,:)+ens(6,:,:)  ! Hospilitized
   tmpens(4,:,:)=ens(7,:,:)+ens(8,:,:)  ! Recovered
   tmpens(5,:,:)=ens(2,:,:)             ! Infected
   tmpens(6,:,:)=ens(1,:,:)             ! exposed 


   call ensave(tmpens,ave,nout,nt,nrens)
   call ensstd(tmpens,std,ave,nout,nt,nrens)
   std=1.0*std

   open(10,file='susc_'//tag//'.dat')
      write(10,*)'TITLE = "Susceptible_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Susceptible_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(1,i), N*std(1,i), N*tmpens(1,i,:)
      enddo
   close(10)

   open(10,file='dead_'//tag//'.dat')
      write(10,*)'TITLE = "Dead_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Dead_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(2,i), N*std(2,i), N*tmpens(2,i,:)
      enddo
   close(10)

   open(10,file='hosp_'//tag//'.dat')
      write(10,*)'TITLE = "Hospitalized_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Hospitalized_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(3,i), N*std(3,i), N*tmpens(3,i,:)
      enddo
   close(10)

   open(10,file='recov_'//tag//'.dat')
      write(10,*)'TITLE = "Recovered_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Recovered_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(4,i), N*std(4,i), N*tmpens(4,i,:)
      enddo
   close(10)

   open(10,file='infec_'//tag//'.dat')
      write(10,*)'TITLE = "Infected_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Infected_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(5,i), N*std(5,i), N*tmpens(5,i,:)
      enddo
   close(10)

   open(10,file='expos_'//tag//'.dat')
      write(10,*)'TITLE = "Exposed_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Exposed_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(1000g13.4)')t, N*ave(6,i), N*std(6,i), N*tmpens(6,i,:)
      enddo
   close(10)

   
   open(10,file='par'//tag//'.dat')
      write(10,*)'TITLE = "Parameters_'//tag//'"'
      write(10,*)'VARIABLES = "iens" "pri" &
                           &"Time_to_death" "N" "I0" "R0" "D_incbation" "D_infectious" &
                           &"D_recovery_mild" "D_recovery_severe" "D_hospital_lag" "CFR" &
                           &"p_severe" "Rt" "InterventionTime"' 
      write(10,'(a,i5,a,i5,a)')' ZONE T="Parameters_'//tag//'"  F=POINT, I=',nrens,', J=1, K=1'
      do j=1,nrens
         write(10,'(2i5,100f13.2)')j,pri,enspar(:,j)
      enddo
   close(10)
  
end subroutine
end module
