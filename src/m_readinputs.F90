module m_readinputs
contains
subroutine readinputs(parstd,nrpar,nrens,nt)
   use mod_parameters 
   use m_enkfini 
   use m_getday
   implicit none
   integer, intent(in)  :: nrpar
   real,    intent(out) :: parstd(nrpar)
   integer, intent(out) :: nrens
   integer, intent(out) :: nt
   logical ex
   character(len=2) ca
   integer id,im,iy,k,day

   inquire(file='infile.in',exist=ex)
   if (.not.ex) then
      print '(a)','Did not find inputfile infile.in...'
      stop
   endif
   open(10,file='infile.in')
      read(10,*)nrens               ;    print '(a,i4)',       'number  of samples         :',nrens
      read(10,*)nt                  ;    print '(a,i4)',       'nt                         :',nt
      read(10,*)time                ;    print '(a,f10.3)',    'Length of integration      :',time
      read(10,'(a)')ca      
      if (ca /= '#1') then
         print *,'#1: error in infile.in'
         stop
      endif
      read(10,*)lenkf               ;    print '(a,l1)',       'Run enkf update            :',lenkf
      read(10,*)nesmda              ;    print '(a,i2)',       'Number of ESMDA steps      :',nesmda
      read(10,*)relobserr           ;    print '(a,f10.4)',    'Realative obs error        :',relobserr
      read(10,*)minobserr           ;    print '(a,f10.4)',    'Minimum   obs error        :',minobserr
      read(10,*)maxobserr           ;    print '(a,f10.4)',    'Maximum   obs error        :',maxobserr
      parstd(2)=0.0

      read(10,'(a)')ca      
      if (ca /= '#2') then
         print *,'#2: error in infile.in'
         stop
      endif

! Read startday of simulation
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      startday=getday(id,im,iy)
      print '(a,i3,i3,i5,i5)',       'Start date of simulation        :',id,im,iy
      print '(a,2i5)',               'Relative start day              :',startday,365+31+28+1



! Read startday of 1 intervention
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv=real(getday(id,im,iy))
      print '(a,i3,i3,i5,f10.2,i5)',    'Start date of first intervention:',id,im,iy,Tinterv

! Read endday of intervention
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv2=real(getday(id,im,iy))
      print '(a,i3,i3,i5,f10.2,i5)',    'End   date of first intervention:',id,im,iy,Tinterv2


      read(10,'(a)')ca      
      if (ca /= '#3') then
         print *,'#3: error in infile.in'
         stop
      endif

! MODEL PARAMETERS (Set first guess (ensemble mean) of parameters (decleared in mod_parameters.F90) and their stddev 
      read(10,*)T2death  , parstd(1);    print '(a,2f10.3)',   'Time to death            and std dev :',T2death  ,parstd(1)
      read(10,*)I0       , parstd(3);    print '(a,2f10.3)',   'Initial infected I0      and std dev :',I0       ,parstd(3)
      read(10,*)R0       , parstd(4);    print '(a,2f10.3)',   'Initial R                and std dev :',R0       ,parstd(4)
      read(10,*)Tinc     , parstd(5);    print '(a,2f10.3)',   'Incubation time          and std dev :',Tinc     ,parstd(5)
      read(10,*)Tinf     , parstd(6);    print '(a,2f10.3)',   'Infection time           and std dev :',Tinf     ,parstd(6)
      read(10,*)Trecm    , parstd(7);    print '(a,2f10.3)',   'Recovery time mild       and std dev :',Trecm    ,parstd(7)
      read(10,*)Trecs    , parstd(8);    print '(a,2f10.3)',   'Recovery time severe     and std dev :',Trecs    ,parstd(8)
      read(10,*)Thosp    , parstd(9);    print '(a,2f10.3)',   'Hospitalization time     and std dev :',Thosp    ,parstd(9)
      read(10,*)CFR      , parstd(10);   print '(a,2f10.3)',   'Critical fatality ratio  and std dev :',CFR      ,parstd(10)
      read(10,*)p_severe , parstd(11);   print '(a,2f10.3)',   'Fraction of severe cases and std dev :',p_severe ,parstd(11)
      read(10,*)Rt       , parstd(12);   print '(a,2f10.3)',   'R during interventions   and std dev :',Rt       ,parstd(12)
!      read(10,*)Tinterv  , parstd(13);   print '(a,2f10.3)',   'Intervention time        and std dev :',Tinterv  ,parstd(13)

      read(10,'(a)')ca      
      if (ca /= '#4') then
         print *,'#4: error in infile.in'
         stop
      endif

      read(10,*)minpar              ;    print '(a,f10.3)',    'Lower bound on all paramters         :',minpar
      read(10,*)rtmax               ;    print '(a,f10.3)',    'Maximum value of Rt during intervent :',rtmax

      read(10,'(a)')ca      
      if (ca /= '#5') then
         print *,'#5: error in infile.in'
         stop
      endif


   close(10)
   print *
end subroutine
end module

