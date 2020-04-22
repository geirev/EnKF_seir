module m_tecplot
contains
subroutine saveresult(fname,varname,aved,stdd,ensd,tag,dt)
   use mod_dimensions
   implicit none
   real, intent(in)    :: dt
   real, intent(in) :: aved(0:nt)
   real, intent(inout) :: stdd(0:nt)
   real, intent(in) :: ensd(0:nt,nrens)
   character(len=1), intent(in) :: tag
   character(len=*), intent(in) :: fname
   character(len=*), intent(in) :: varname
   integer i,j
   real t
   
   open(10,file=fname//'_'//tag//'.dat')
      write(10,'(5a)')'TITLE = "'//varname//'_'//tag//'"'
      write(10,'(a)')'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,min(nrens,1000))
      write(10,*)'ZONE T="'//varname//'_'//tag//'"  F=POINT, I=',nt+1,', J=1, K=1'
      if (stdd(0) < 1.0E-30) stdd(0)=0.0
      do i=0,nt
         t=0.0+real(i)*dt
         write(10,'(2000E13.5)')t, aved(i), stdd(i), (ensd(i,j),j=1,min(nrens,1000))
      enddo
   close(10)

end subroutine

subroutine tecplot(ens,enspar,pri)
   use mod_dimensions
   use mod_states
   use mod_diag
   use mod_params
   use mod_parameters
   use m_agegroups
   use m_ensave
   use m_ensstd
   use m_enkfini
   implicit none
   integer, intent(in) :: pri

   type(params), intent(in) :: enspar(nrens)

   type(states), intent(in) :: ens(0:nt,nrens)
   type(states) ave(0:nt)
   type(states) std(0:nt)

   type(diag) ensd(0:nt,nrens)
   type(diag) aved(0:nt)
   type(diag) stdd(0:nt)

   integer i,j,m
   real t,dt
   real aveR(0:nt),stdR(0:nt)
   character(len=1) tag
   character(len=3) tag3
   character(len=30) fm

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
   enddo

! Big tecplot dump
   write(fm,'(a,i2.2,a)')'(a,3(',na,'(a,i2.2,a)),a)'   
   open(10,file='bigdump'//tag//'.dat')
      write(10,*)'TITLE = "Bigdump"'
      write(10,fm)'VARIABLES = "i" "time" ', &
                          & ('"S',i,'" ',i=1,na),  &
                          & ('"E',i,'" ',i=1,na),  &
                          & ('"I',i,'" ',i=1,na),  &
                          &' "Qm" "Qs" "Qf" "Hs" "Hf" "Rm" "Rs" "D"'
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
 
      do j=1,min(nrens,100)
         write(tag3,'(i3.3)')j
         write(10,'(a,i5,a,i5,a)')' ZONE T="mem'//tag3//'"  F=POINT, I=',nt+1,', J=1, K=1'
         do i=0,nt
            t=0+real(i)*dt
            write(10,'(i5,f10.2,50g13.5)')i,t,N*ens(i,j)
         enddo
      enddo
   close(10)

   do j=1,nrens
      do i=0,nt
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
      do j=1,nrens
         aved(i)=aved(i) + ensd(i,j)
      enddo
      aved(i)=aved(i)*(1.0/real(nrens))

      stdd(i)=0.0
      do j=1,nrens
         stdd(i)=stdd(i) + (ensd(i,j)-aved(i))*(ensd(i,j)-aved(i))
      enddo
      stdd(i)=stdd(i)*(1.0/real(nrens-1))
      stdd(i)=sqrt(stdd(i))

   enddo


   call saveresult('susc' ,'Susceptible'  ,aved(:)%S,stdd(:)%S,ensd(:,:)%S,tag,dt)
   call saveresult('expos','Exposed'      ,aved(:)%E,stdd(:)%E,ensd(:,:)%E,tag,dt)
   call saveresult('infec','Infecteus'    ,aved(:)%I,stdd(:)%I,ensd(:,:)%I,tag,dt)
   call saveresult('recov','Recovered'    ,aved(:)%R,stdd(:)%R,ensd(:,:)%R,tag,dt)
   call saveresult('hosp' ,'Hospitalized' ,aved(:)%H,stdd(:)%H,ensd(:,:)%H,tag,dt)
   call saveresult('dead' ,'Dead'         ,aved(:)%D,stdd(:)%D,ensd(:,:)%D,tag,dt)
   call saveresult('case' ,'Cases'        ,aved(:)%C,stdd(:)%C,ensd(:,:)%C,tag,dt)

 
   open(10,file='obs.dat')
   open(11,file='obsD.dat')
   open(12,file='obsH.dat')
      write(10,*)'TITLE = "Observations"'
      write(10,*)'VARIABLES = "i" "time" "ave" "std" '
      write(11,*)'TITLE = "Observations"'
      write(11,*)'VARIABLES = "i" "time" "ave" "std" '
      write(12,*)'TITLE = "Observations"'
      write(12,*)'VARIABLES = "i" "time" "ave" "std" '

      m=0
      do i=1,nrobs
         if (cobs(i)=='d') m=m+1
      enddo
      if (m==0) then
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',1,', J=1, K=1'
         write(10,'(2000g13.5)')(0.0,i=1,nrens+3)
         write(11,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',1,', J=1, K=1'
         write(11,'(2000g13.5)')(0.0,i=1,nrens+3)
      else
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',m,', J=1, K=1'
         write(11,'(a,i5,a,i5,a)')' ZONE T="Observed deaths"  F=POINT, I=',m,', J=1, K=1'
         do i=1,nrobs
            if (cobs(i)=='d') write(10,'(2i5,2f12.4)')i,tobs(i), dobs(i), sqrt(Rprt(i))
            if (cobs(i)=='d') write(11,'(2i5,2f12.4)')i,tobs(i), dobs(i), sqrt(Rprt(i))
         enddo
      endif

      m=0
      do i=1,nrobs
         if (cobs(i)=='h') m=m+1
      enddo
      if (m==0) then
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',1,', J=1, K=1'
         write(10,'(2000g13.5)')(0.0,i=1,nrens+3)
         write(12,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',1,', J=1, K=1'
         write(12,'(2000g13.5)')(0.0,i=1,nrens+3)
      else
         write(10,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',m,', J=1, K=1'
         write(12,'(a,i5,a,i5,a)')' ZONE T="Observed hospitalized"  F=POINT, I=',m,', J=1, K=1'
         do i=1,nrobs
            if (cobs(i)=='h') write(10,'(2i5,2f12.4)')i,tobs(i), dobs(i), sqrt(Rprt(i))
            if (cobs(i)=='h') write(12,'(2i5,2f12.4)')i,tobs(i), dobs(i), sqrt(Rprt(i))
         enddo
      endif

      close(10)
      close(11)
      close(12)
  
! Parameters   
   open(10,file='par'//tag//'.dat')
      write(10,*)'TITLE = "Parameters_'//tag//'"'
      write(10,*)'VARIABLES = "iens" "pri" ',parnames%I0,parnames%Tinf,parnames%Tinc,parnames%Trecm,parnames%Trecs,parnames%Thosp,&
                                             parnames%Tdead,parnames%p_sev,parnames%CFR
      write(10,'(a,i5,a,i5,a)')' ZONE T="Parameters_'//tag//'"  F=POINT, I=',nrens,', J=1, K=1'
      do j=1,nrens
         write(10,'(2i5,200f13.6)')j,pri,enspar(j)%I0,      &
                                         enspar(j)%Tinf,    &
                                         enspar(j)%Tinc,    &
                                         enspar(j)%Trecm,   &
                                         enspar(j)%Trecs,   &
                                         enspar(j)%Thosp,   &
                                         enspar(j)%Tdead,   &
                                         enspar(j)%p_sev,   &
                                         enspar(j)%CFR 
      enddo 
   close(10)
   
   aveR=0.0
   stdR=0.0
   do i=0,nt
      do j=1,nrens
         aveR(i)=aveR(i) + enspar(j)%R(i)
      enddo
      aveR(i)=aveR(i)/real(nrens)

      do j=1,nrens
         stdR(i)=stdR(i) + (enspar(j)%R(i)-aveR(i))**2
      enddo
      stdR(i)=stdR(i)/real(nrens-1) 
      stdR(i)=sqrt(stdR(i))
   enddo

   open(10,file='Rens_'//tag//'.dat')
   write(10,*)'TITLE = "Rens_'//tag//'"'
      write(10,'(a)')'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,min(nrens,1000))
      write(10,*)'ZONE T="Rens_'//tag//'"  F=POINT, I=',min(nt+1,rdim+1),', J=1, K=1'
      do i=0,min(rdim,nt)
         write(10,'(2000f10.5)')real(i)*dt,aveR(i),stdR(i),enspar(1:min(nrens,1000))%R(i)
      enddo
   close(10)
  
end subroutine
end module 
