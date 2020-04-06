module m_tecplot
contains
subroutine tecplot(ens,enspar,nt,nrens,neq,nrpar,pri)
   use mod_parameters
   use m_agegroups
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
   real ave(0:neq-1,0:nt)
   real std(0:neq-1,0:nt)

   real tmpens(0:nout,0:nt,nrens)
   real avet(0:nout,0:nt)
   real stdt(0:nout,0:nt)

   real enspar(nrpar+neq,nrens)


   integer i,j
   real t,dt
   character(len=1) tag
   character(len=3) tag3

   write(tag,'(i1.1)')pri
   dt= time/real(nt)

! Big tecplot dump
   call ensave(ens,ave,neq,nt,nrens)
   call ensstd(ens,std,ave,neq,nt,nrens)
   std=2.0*std
   open(10,file='bigdump'//tag//'.dat')
      write(10,*)'TITLE = "Bigdump"'
      write(10,*)'VARIABLES = "i" "time" "S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9" "S10" "S11" &
                                    &"E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10" "E11" &
                                    &"I1" "I2" "I3" "I4" "I5" "I6" "I7" "I8" "I9" "I10" "I11" &
                                    &"Qm" "Qs" "Qh" "Qf" "Rm" "Rs" "D"'
      write(10,'(a,i5,a,i5,a)')' ZONE T="ave"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(i5,f10.2,50g13.5)')i,t,N*ave(:,i)
      enddo

      write(10,'(a,i5,a,i5,a)')' ZONE T="std"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(i5,f10.2,50g13.5)')i,t,N*std(:,i)
      enddo

      do j=1,min(nrens,100)
         write(tag3,'(i3.3)')j
         write(10,'(a,i5,a,i5,a)')' ZONE T="mem'//tag3//'"  F=POINT, I=',nt+1,', J=1, K=1'
         do i=0,nt
            t=0+real(i)*dt
            write(10,'(i5,f10.2,50g13.5)')i,t,N*ens(:,i,j)
         enddo
      enddo
   close(10)

!   S_i        = y(0    : na-1   ) ! Susectable
!   E_i        = y(na   : 2*na-1 ) ! Exposed
!   I_i        = y(2*na : 3*na-1 ) ! Infected
!   Q_m        = y(3*na)   ! Recovering (Mild)
!   Q_s        = y(3*na+1) ! Recovering (Severe at home)
!   Q_h        = y(3*na+2) ! Recovering (Severe in hospital)
!   Q_f        = y(3*na+3) ! fatally sick to die
!   R_m        = y(3*na+4) ! Recovered
!   R_s        = y(3*na+5) ! Recovered 
!   D          = y(3*na+6) ! Dead
   
   tmpens(:,:,:)=0.0
   do i=0,na-1
      tmpens(0,:,:)=tmpens(0,:,:)+ens(i,:,:)      ! Susceptible 
      tmpens(5,:,:)=tmpens(5,:,:)+ens(2*na+i,:,:) ! Infected
      tmpens(6,:,:)=tmpens(6,:,:)+ens(1*na+i,:,:) ! Exposed
   enddo
   do i=na,neq-1
      tmpens(1,:,:)=tmpens(1,:,:)+ens(i,:,:) ! All cases
   enddo
   tmpens(2,:,:)=ens(3*na+6,:,:)                  ! Dead
   tmpens(3,:,:)=ens(3*na+2,:,:)+ens(3*na+3,:,:)  ! Hospilitized
   tmpens(4,:,:)=ens(3*na+4,:,:)+ens(3*na+5,:,:)  ! Recovered

   call ensave(tmpens,avet,nout+1,nt,nrens)
   call ensstd(tmpens,stdt,avet,nout+1,nt,nrens)
   std=2.0*std

   open(10,file='susc_'//tag//'.dat')
      write(10,*)'TITLE = "Susceptible_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Susceptible_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(0,i), N*stdt(0,i), N*tmpens(0,i,:)
      enddo
   close(10)

   open(10,file='case_'//tag//'.dat')
      write(10,*)'TITLE = "Cases_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Cases_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(1,i), N*stdt(1,i), N*tmpens(1,i,:)
      enddo
   close(10)

   open(10,file='dead_'//tag//'.dat')
      write(10,*)'TITLE = "Dead_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Dead_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(2,i), N*stdt(2,i), N*tmpens(2,i,:)
      enddo
   close(10)

   open(10,file='hosp_'//tag//'.dat')
      write(10,*)'TITLE = "Hospitalized_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Hospitalized_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(3,i), N*stdt(3,i), N*tmpens(3,i,:)
      enddo
   close(10)

   open(10,file='recov_'//tag//'.dat')
      write(10,*)'TITLE = "Recovered_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Recovered_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(4,i), N*stdt(4,i), N*tmpens(4,i,:)
      enddo
   close(10)

   open(10,file='infec_'//tag//'.dat')
      write(10,*)'TITLE = "Infected_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Infected_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(5,i), N*stdt(5,i), N*tmpens(5,i,:)
      enddo
   close(10)

   open(10,file='expos_'//tag//'.dat')
      write(10,*)'TITLE = "Exposed_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i3,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Exposed_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.4)')t, N*avet(6,i), N*stdt(6,i), N*tmpens(6,i,:)
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
         write(10,'(2i5,2000f13.4)')j,pri,enspar(1:nrpar,j)
      enddo
   close(10)
  
end subroutine
end module
