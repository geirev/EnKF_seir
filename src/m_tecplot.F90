module m_tecplot
contains

subroutine tecplot(ens,enspar,pri)
   use mod_dimensions
   use mod_states
   use mod_diag
   use mod_params
   use mod_parameters
   use m_readagegroups
   use m_ensave
   use m_ensstd
   use m_enkfini
   use m_readinfile
   implicit none
   integer, intent(in) :: pri

   type(params), intent(in) :: enspar(nrens)

   type(states), intent(in) :: ens(0:nt,nrens)

   type(states) ave(0:nt)
   type(states) std(0:nt)

   type(diag) ensd(0:nt,nrens)
   type(diag) aved(0:nt)
   type(diag) stdd(0:nt)

   integer ic,i,l
   real dt,t
   character(len=1) tag
   character(len=3) tag3
   character(len=100) :: cmd='mkdir -p '

   print '(a)','Dumping tecplot files.'

   write(tag,'(i1.1)')pri
   dt= time/real(nt-1)

   do ic=1,nc
      write(tag3,'(i3.3)')ic
      call ensemblediagnostics(ensd,aved,stdd,ens,ic)

      call saveresults('susc'   ,'Susceptible'  ,aved(:)%S,stdd(:)%S,ensd(:,:)%S,tag,tag3,dt,outdir)
      call saveresults('expos'  ,'Exposed'      ,aved(:)%E,stdd(:)%E,ensd(:,:)%E,tag,tag3,dt,outdir)
      call saveresults('infec'  ,'Infecteus'    ,aved(:)%I,stdd(:)%I,ensd(:,:)%I,tag,tag3,dt,outdir)
      call saveresults('recov'  ,'Recovered'    ,aved(:)%R,stdd(:)%R,ensd(:,:)%R,tag,tag3,dt,outdir)
      call saveresults('hosp'   ,'Hospitalized' ,aved(:)%H,stdd(:)%H,ensd(:,:)%H,tag,tag3,dt,outdir)
      call saveresults('dead'   ,'Dead'         ,aved(:)%D,stdd(:)%D,ensd(:,:)%D,tag,tag3,dt,outdir)
      call saveresults('case'   ,'Cases'        ,aved(:)%C,stdd(:)%C,ensd(:,:)%C,tag,tag3,dt,outdir)
      call saveresults('active' ,'Active'       ,aved(:)%A,stdd(:)%A,ensd(:,:)%A,tag,tag3,dt,outdir)
    
      call saveobservations(ic,'D',outdir)
      call saveobservations(ic,'H',outdir)
      call saveobservations(ic,'C',outdir)
     
      call saveparameters(enspar,ic,tag3,pri,outdir)

      call saveR(enspar,ic,tag3,tag,dt,outdir)


      open(10,file=trim(outdir)//'/'//'Xsusc'//tag3//'_'//tag//'.dat')
         write(10,'(5a)')'TITLE = "'//'Xsusc'//tag3//'_'//tag//'"'
         write(10,'(a)')'VARIABLES = "time" '
         write(10,'(20(a,i4,a))')(' "',l,'"',l=1,na)
         write(10,*)'ZONE T="'//'Xsusc'//tag3//'_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
         do i=0,nt-1
            t=0.0+real(i)*dt
            write(10,'(2000E13.5)',advance='no')t
            do l=1,na
               write(10,'(2000E13.5)',advance='no')sum(ens(i,:)%group(ic)%S(l))/real(nrens)
            enddo
            write(10,'(a)')' '
         enddo
      close(10)

      open(10,file=trim(outdir)//'/'//'Xvacc'//tag3//'_'//tag//'.dat')
         write(10,'(5a)')'TITLE = "'//'Xvacc'//tag3//'_'//tag//'"'
         write(10,'(a)')'VARIABLES = "time" '
         write(10,'(20(a,i4,a))')(' "',l,'"',l=1,na)
         write(10,*)'ZONE T="'//'Xvacc'//tag3//'_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
         do i=0,nt-1
            t=0.0+real(i)*dt
            write(10,'(2000E13.5)',advance='no')t
            do l=1,na
               write(10,'(2000E13.5)',advance='no')sum(ens(i,:)%group(ic)%V(l))/real(nrens)
            enddo
            write(10,'(a)')' '
         enddo
      close(10)

   enddo 
   print '(a)','Tecplot dump done'
end subroutine

subroutine ensemblediagnostics(ensd,aved,stdd,ens,ic)
   use mod_dimensions
   use mod_states
   use mod_diag
   use m_readagegroups
   integer, intent(in) :: ic
   type(states), intent(in) :: ens(0:nt,nrens)
   type(diag), intent(out)  :: ensd(0:nt,nrens)
   type(diag), intent(out) :: aved(0:nt)
   type(diag), intent(out) :: stdd(0:nt)
   integer j,i
   do j=1,nrens
      do i=0,nt
         ensd(i,j)%S=sum(ens(i,j)%group(ic)%S(:))
         ensd(i,j)%E=sum(ens(i,j)%group(ic)%E(:))
         ensd(i,j)%I=sum(ens(i,j)%group(ic)%I(:))
         ensd(i,j)%H=ens(i,j)%group(ic)%Hs + ens(i,j)%group(ic)%Hf
         ensd(i,j)%R=ens(i,j)%group(ic)%Rm + ens(i,j)%group(ic)%Rs
         ensd(i,j)%D=ens(i,j)%group(ic)%D
         ensd(i,j)%C=&  !!!!!!! Dont include exposed in cases. ensd(i,j)%E  &
                    + ensd(i,j)%I  &
                     + ens(i,j)%group(ic)%Qm &
                     + ens(i,j)%group(ic)%Qs &
                     + ens(i,j)%group(ic)%Qf &
                     + ens(i,j)%group(ic)%Hs &
                     + ens(i,j)%group(ic)%Hf &
                     + ens(i,j)%group(ic)%Rm &
                     + ens(i,j)%group(ic)%Rs &
                     + ens(i,j)%group(ic)%D 
         ensd(i,j)%A=ensd(i,j)%C + ensd(i,j)%E - ensd(i,j)%R - ensd(i,j)%D
         ensd(i,j)=Ntot(ic)*ensd(i,j)
      enddo
   enddo

   do i=0,nt-1

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
end subroutine

subroutine saveresults(fname,varname,aved,stdd,ensd,tag,tag3,dt,outdir)
   use mod_dimensions
   implicit none
   real, intent(in)    :: dt
   real, intent(in) :: aved(0:nt)
   real, intent(inout) :: stdd(0:nt)
   real, intent(in) :: ensd(0:nt,nrens)
   character(len=1), intent(in) :: tag
   character(len=3), intent(in) :: tag3
   character(len=*), intent(in) :: fname
   character(len=*), intent(in) :: varname
   character(len=*), intent(in) :: outdir
   integer i,j
   real t
   

   open(10,file=trim(outdir)//'/'//fname//tag3//'_'//tag//'.dat')
      write(10,'(5a)')'TITLE = "'//varname//tag3//'_'//tag//'"'
      write(10,'(a)')'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,min(nrens,1000))
      write(10,*)'ZONE T="'//varname//tag3//'_'//tag//'"  F=POINT, I=',nt,', J=1, K=1'
      if (stdd(0) < 1.0E-30) stdd(0)=0.0
      do i=0,nt-1
         t=0.0+real(i)*dt
         write(10,'(2000E13.5)')t, aved(i), stdd(i), (ensd(i,j),j=1,min(nrens,1000))
      enddo
   close(10)

end subroutine

subroutine saveobservations(ic,obstype,outdir)
   use mod_dimensions
   use m_enkfini
   implicit none
   integer, intent(in) :: ic
   character(len=1), intent(in) :: obstype
   character(len=3) tag3
   character(len=1) c
   character(len=100) zonetitle
   character(len=*), intent(in) :: outdir
   integer m,i

   zonetitle=' '
   write(tag3,'(i3.3)')ic

   select case (obstype)
   case('D')
      c='d'
      zonetitle='Observed deaths '//tag3
   case('H')
      c='h'
      zonetitle='Observed Hospitalized '//tag3
   case('C')
      c='c'
      zonetitle='Observed Cases '//tag3
   case default
      print '(a)','invalid obstype:',obstype
      stop
   end select

   open(10,file=trim(outdir)//'/'//'obs'//obstype//tag3//'.dat')
      write(10,*)'TITLE = "Observations"'
      write(10,*)'VARIABLES = "i" "time" "ave" "std" '

      m=0
      do i=1,nrobs
         if (obs(i)%c==c .and. obs(i)%ic==ic) m=m+1
      enddo

      if (m==0) then
         write(10,'(a,i5,a)')' ZONE T="'//trim(zonetitle)//'"  F=POINT, I=',1,', J=1, K=1'
         write(10,'(2000g13.5)')(0.0,i=1,nrens+3)
      else
         write(10,'(a,i5,a)')' ZONE T="'//trim(zonetitle)//'"  F=POINT, I=',m,', J=1, K=1'
         do i=1,nrobs
            if (obs(i)%c==c .and. obs(i)%ic==ic) write(10,'(2i5,2f14.4)')i,obs(i)%t, obs(i)%d, sqrt(Rprt(i))
         enddo
      endif
   close(10)

end subroutine

subroutine saveparameters(enspar,ic,tag3,pri,outdir)
   use mod_dimensions
   use mod_parameters
   use mod_params
   implicit none
   type(params), intent(in) :: enspar(nrens)
   integer, intent(in) :: ic
   integer, intent(in) :: pri
   character(len=3), intent(in) :: tag3
   character(len=*), intent(in) :: outdir
   character(len=1) tag
   integer j

   write(tag,'(i1.1)')pri

   open(10,file=trim(outdir)//'/'//'par'//tag3//'_'//tag//'.dat')
      write(10,*)'TITLE = "Parameters_'//tag3//tag//'"'
      write(10,*)'VARIABLES = "iens" "pri" ',parnames%E0,parnames%I0,parnames%Tinf,parnames%Tinc,parnames%Trecm,&
                                             parnames%Trecs,parnames%Thosp, parnames%Tdead,parnames%sev,parnames%CFR
      write(10,'(a,i5,a,i5,a)')' ZONE T="Parameters_'//tag//'"  F=POINT, I=',nrens,', J=1, K=1'
      do j=1,nrens
         write(10,'(2i5,200f13.6)')j,pri,enspar(j)%E0(ic),  &
                                         enspar(j)%I0(ic),  &
                                         enspar(j)%Tinf,    &
                                         enspar(j)%Tinc,    &
                                         enspar(j)%Trecm,   &
                                         enspar(j)%Trecs,   &
                                         enspar(j)%Thosp,   &
                                         enspar(j)%Tdead,   &
                                         enspar(j)%sev(ic), &
                                         enspar(j)%CFR(ic) 
      enddo 
   close(10)
end subroutine

subroutine saveR(enspar,ic,tag3,tag,dt,outdir)
   use mod_dimensions
   use mod_parameters
   use mod_params
   implicit none
   integer, intent(in) :: ic
   character(len=1) tag
   character(len=3) tag3
   real, intent(in) :: dt
   integer i,j
   real aveR(0:nt),stdR(0:nt)
   type(params), intent(in) :: enspar(nrens)
   character(len=*), intent(in) :: outdir
      
   aveR=0.0
   stdR=0.0
   do i=0,nt
      do j=1,nrens
         aveR(i)=aveR(i) + enspar(j)%R(i,ic)
      enddo
      aveR(i)=aveR(i)/real(nrens)

      do j=1,nrens
         stdR(i)=stdR(i) + (enspar(j)%R(i,ic)-aveR(i))**2
      enddo
      stdR(i)=stdR(i)/real(nrens-1) 
      stdR(i)=sqrt(stdR(i))
   enddo

   open(10,file=trim(outdir)//'/'//'Rens'//tag3//'_'//tag//'.dat')
   write(10,*)'TITLE = "Rens'//tag3//'_'//tag//'"'
      write(10,'(a)')'VARIABLES = "time" "ave" "std" '
      write(10,'(20(a,i4,a))')(' "',i,'"',i=1,min(nrens,1000))
      write(10,*)'ZONE T="Rens'//tag3//'_'//tag//'"  F=POINT, I=',min(nt+1,rdim+1),', J=1, K=1'
      do i=0,min(rdim,nt)
         write(10,'(2000f10.5)')real(i)*dt,aveR(i),stdR(i),enspar(1:min(nrens,1000))%R(i,ic)
      enddo
   close(10)
end subroutine

!subroutine bigdump(fname,varname,aved,stdd,ensd,tag,dt)
! ensemble average and std as a function of time
!   do i=0,nt-1
!      ave(i)=0.0
!      std(i)=0.0
!      do j=1,nrens
!         ave(i)=ave(i) + ens(i,j)
!      enddo
!      ave(i)=ave(i)*(1.0/real(nrens))
!      do j=1,nrens
!         std(i)=std(i) + (ens(i,j)-ave(i))*(ens(i,j)-ave(i))
!      enddo
!      std(i)=std(i)*(1.0/real(nrens-1))
!      std(i)=sqrt(std(i))
!   enddo

! Big tecplot dump
!   character(len=30) fm
!   write(fm,'(a,i2.2,a)')'(a,3(',na,'(a,i2.2,a)),a)'   
!   open(10,file='bigdump'//tag//'.dat')
!      write(10,*)'TITLE = "Bigdump"'
!      write(10,fm)'VARIABLES = "i" "time" ', &
!                          & ('"S',i,'" ',i=1,na),  &
!                          & ('"E',i,'" ',i=1,na),  &
!                          & ('"I',i,'" ',i=1,na),  &
!                          &' "Qm" "Qs" "Qf" "Hs" "Hf" "C" "Rm" "Rs" "D"'
!      write(10,'(a,i5,a,i5,a)')' ZONE T="ave"  F=POINT, I=',nt+1,', J=1, K=1'
!      do i=0,nt
!         t=0+real(i)*dt
!         write(10,'(i5,f10.2,50g13.5)')i,t,N*ave(i)
!      enddo
! 
!      write(10,'(a,i5,a,i5,a)')' ZONE T="std"  F=POINT, I=',nt+1,', J=1, K=1'
!      do i=0,nt
!         t=0+real(i)*dt
!         write(10,'(i5,f10.2,50g13.5)')i,t,N*std(i)
!      enddo
! 
!      do j=1,min(nrens,100)
!         write(ttag3,'(i3.3)')j
!         write(10,'(a,i5,a,i5,a)')' ZONE T="mem'//ttag3//'"  F=POINT, I=',nt+1,', J=1, K=1'
!         do i=0,nt
!            t=0+real(i)*dt
!            write(10,'(i5,f10.2,50g13.5)')i,t,N*ens(i,j)
!         enddo
!      enddo
!   close(10)
! end subroutine 
end module 
