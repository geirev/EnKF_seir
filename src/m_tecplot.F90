module m_tecplot
contains
subroutine tecplot(ens,enspar,nt,nrens,nrpar,pri)
   use mod_dimensions
   use mod_states
   use mod_diag
   use mod_parameters
   use m_agegroups
   use m_ensave
   use m_ensstd
   use m_enkfini
   implicit none
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   integer, intent(in) :: nrpar
   integer, intent(in) :: pri

   real, intent(in) :: enspar(nrpar,nrens)

   type(states), intent(in) :: ens(0:nt,nrens)
   type(states) ave(0:nt)
   type(states) std(0:nt)

   type(diag) ensd(0:nt,nrens)
   type(diag) aved(0:nt)
   type(diag) stdd(0:nt)

   integer i,j,m
   real t,dt
   character(len=1) tag
   character(len=3) tag3

   write(tag,'(i1.1)')pri
   dt= time/real(nt-1)
   
! ensemble average and std as a function of time
   do i=0,nt
      ave(i)=0.0
      std(i)=0.0
      do j=1,nrens
         ave(i)=ave(i) + ens(i,j)
      enddo
      ave(i)=ave(i)*(1.0/real(nrens))
      do j=1,nrens
         std(i)=std(i) + (ens(i,j)-ave(i))*(ens(i,j)-ave(i))
      enddo
      std(i)=std(i)*(1.0/real(nrens-1))
      std(i)=sqrt(std(i))
      std(i)=2.0*std(i)
   enddo

! Big tecplot dump
   open(10,file='bigdump'//tag//'.dat')
      write(10,*)'TITLE = "Bigdump"'
      write(10,*)'VARIABLES = "i" "time" "S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9" "S10" "S11" &
                                    &"E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8" "E9" "E10" "E11" &
                                    &"I1" "I2" "I3" "I4" "I5" "I6" "I7" "I8" "I9" "I10" "I11" &
                                    &"Qm" "Qs" "Qh" "Qf" "Rm" "Rs" "D"'
      write(10,'(a,i5,a,i5,a)')' ZONE T="ave"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(i5,f10.2,50g13.5)')i,t,N*ave(i)
      enddo
 
      write(10,'(a,i5,a,i5,a)')' ZONE T="std"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(i5,f10.2,50g13.5)')i,t,N*std(i)
      enddo
 
!      do j=1,min(nrens,100)
!         write(tag3,'(i3.3)')j
!         write(10,'(a,i5,a,i5,a)')' ZONE T="mem'//tag3//'"  F=POINT, I=',nt+1,', J=1, K=1'
!         do i=0,nt
!            t=0+real(i)*dt
!            write(10,'(i5,f10.2,50g13.5)')i,t,N*ens(i,j)
!         enddo
!      enddo
   close(10)

   do j=1,nrens
      do i=1,nt
         ensd(i,j)%S=sum(ens(i,j)%S(:))
         ensd(i,j)%E=sum(ens(i,j)%E(:))
         ensd(i,j)%I=sum(ens(i,j)%I(:))
         ensd(i,j)%H=ens(i,j)%Hs + ens(i,j)%Hf
         ensd(i,j)%R=ens(i,j)%Rm + ens(i,j)%Rs
         ensd(i,j)%D=ens(i,j)%D
         ensd(i,j)%C= ensd(i,j)%E  &
                    + ensd(i,j)%I  &
                     + ens(i,j)%Qm &
                     + ens(i,j)%Qs &
                     + ens(i,j)%Qf &
                     + ens(i,j)%Hs &
                     + ens(i,j)%Hf &
                     + ens(i,j)%Rm &
                     + ens(i,j)%Rs &
                     + ens(i,j)%D 
         ensd(i,j)=N*ensd(i,j)
      enddo
   enddo

   do i=0,nt
      aved(i)=0.0
      stdd(i)=0.0
      do j=1,nrens
         aved(i)=aved(i) + ensd(i,j)
      enddo
      aved(i)=aved(i)*(1.0/real(nrens))
      do j=1,nrens
         stdd(i)=stdd(i) + (ensd(i,j)-aved(i))*(ensd(i,j)-aved(i))
      enddo
      stdd(i)=stdd(i)*(1.0/real(nrens-1))
      stdd(i)=sqrt(stdd(i))
      stdd(i)=2.0*stdd(i)
   enddo

   open(10,file='susc_'//tag//'.dat')
      write(10,*)'TITLE = "Susceptible_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Susceptible_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%S, stdd(i)%S, (ensd(i,j)%S,j=1,nrens)
      enddo
   close(10)

   open(10,file='expos_'//tag//'.dat')
      write(10,*)'TITLE = "Exposed_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Exposed_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%E, stdd(i)%E, (ensd(i,j)%E,j=1,nrens)
      enddo
   close(10)

   open(10,file='infec_'//tag//'.dat')
      write(10,*)'TITLE = "Infected_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Infected_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%I, stdd(i)%I, (N*ensd(i,j)%I,j=1,nrens)
      enddo
   close(10)

   open(10,file='case_'//tag//'.dat')
      write(10,*)'TITLE = "Cases_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Cases_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%C, stdd(i)%C, (ensd(i,j)%C,j=1,nrens)
      enddo
   close(10)


   open(10,file='hosp_'//tag//'.dat')
      write(10,*)'TITLE = "Hospitalized_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Hospitalized_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%H, stdd(i)%H, (ensd(i,j)%H,j=1,nrens)
      enddo
      m=0
      do i=1,nrobs
         if (cobs(i)=='h') m=m+1
      enddo
      if (m==0) then
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',1,', J=1, K=1'
         write(10,'(2000g13.5)')(0.0,i=1,nrens+3)
      else
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',m,', J=1, K=1'
         do i=1,nrobs
            if (cobs(i)=='h') write(10,'(2000g13.5)')tobs(i), dobs(i), 3.0*sqrt(R(i,i)), Dprt(i,1:nrens)
         enddo
      endif
   close(10)

   open(10,file='recov_'//tag//'.dat')
      write(10,*)'TITLE = "Recovered_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Recovered_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%R, stdd(i)%R, (ensd(i,j)%R,j=1,nrens)
      enddo
   close(10)
   
   open(10,file='dead_'//tag//'.dat')
      write(10,*)'TITLE = "Dead_'//tag//'"'
      write(10,*)'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,nrens)
      write(10,'(a,i5,a,i5,a)')' ZONE T="Dead_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      do i=0,nt
         t=0+real(i)*dt
         write(10,'(2000g13.5)')t, aved(i)%D, stdd(i)%D, (ensd(i,j)%D,j=1,nrens)
      enddo

      m=0
      do i=1,nrobs
         if (cobs(i)=='d') m=m+1
      enddo
      if (m==0) then
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',1,', J=1, K=1'
         write(10,'(2000g13.5)')(0.0,i=1,nrens+3)
      else
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',m,', J=1, K=1'
         do i=1,nrobs
            if (cobs(i)=='d') write(10,'(2000g13.5)')tobs(i), dobs(i), 3.0*sqrt(R(i,i)), Dprt(i,1:nrens)
         enddo
      endif
   close(10)

! Parameters   
   open(10,file='par'//tag//'.dat')
      write(10,*)'TITLE = "Parameters_'//tag//'"'
      write(10,*)'VARIABLES = "iens" "pri" &
                           &"Time_to_death" "N" "I0" "R0" "D_incbation" "D_infectious" &
                           &"D_recovery_mild" "D_recovery_severe" "D_hospital_lag" "CFR" &
                           &"p_severe" "Rt"' 
      write(10,'(a,i5,a,i5,a)')' ZONE T="Parameters_'//tag//'"  F=POINT, I=',nrens,', J=1, K=1'
      do j=1,nrens
         write(10,'(2i5,200f13.4)')j,pri,enspar(1:nrpar,j)
      enddo
   close(10)
  
end subroutine
end module
