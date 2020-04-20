module m_readinputs
contains
subroutine readinputs(nrens,nt)
   use mod_params
   use mod_parameters 
   use m_enkfini 
   use m_getday
   implicit none
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
      read(10,*)mode_analysis       ;    print '(a,i2)',       'Analysis mode of EnKF      :',mode_analysis
      read(10,*)nesmda              ;    print '(a,i2)',       'Number of ESMDA steps      :',nesmda
      read(10,*)relobserr           ;    print '(a,f10.4)',    'Realative obs error        :',relobserr
      read(10,*)minobserr           ;    print '(a,f10.4)',    'Minimum   obs error        :',minobserr
      read(10,*)maxobserr           ;    print '(a,f10.4)',    'Maximum   obs error        :',maxobserr
      read(10,*)lmeascorr           ;    print '(a,l1)',       'Activate correated obs err :',lmeascorr
      read(10,*)rh                  ;    print '(a,f10.4)',    'Obs error correation       :',rh
      read(10,*)truncation          ;    print '(a,f10.4)',    'EnKF SVD truncation (0.99) :',truncation

      read(10,'(a)')ca      
      if (ca /= '#2') then
         print *,'#2: error in infile.in'
         stop
      endif

! Read startday of simulation - Running with Rmat(:,:,1)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      startday=getday(id,im,iy)
      print '(a,i3,i3,i5,i5)',       'Start date of simulation        :',id,im,iy
      print '(a,2i5)',               'Relative start day              :',startday,365+31+28+1



! Read startday of 1st intervention - switching to Rmat(:,:,2)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv(1)=real(getday(id,im,iy))
      print '(a,i3,i3,i5,f10.2,i5)',    'Start date of first  intervention:',id,im,iy,Tinterv(1)

! Read startdate of 2nd intervention- switching to Rmat(:,:,3)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv(2)=real(getday(id,im,iy))
      print '(a,i3,i3,i5,f10.2,i5)',    'Start date of second intervention:',id,im,iy,Tinterv(2)


      read(10,'(a)')ca      
      if (ca /= '#3') then
         print *,'#3: error in infile.in'
         stop
      endif

! MODEL PARAMETERS (Set first guess (ensemble mean) of parameters (decleared in mod_parameters.F90) and their stddev 
      read(10,*)p%R(1) , parstd%R(1) ; print '(a,2f10.3)', 'R until 1st intervention and std dev :',p%R(1)   ,parstd%R(1)   
      read(10,*)p%R(2) , parstd%R(2) ; print '(a,2f10.3)', 'R 1st-2nd intervention   and std dev :',p%R(2)   ,parstd%R(2)   
      read(10,*)p%R(3) , parstd%R(3) ; print '(a,2f10.3)', 'R 2nd-3rd intervention   and std dev :',p%R(3)   ,parstd%R(3)   
      read(10,*)p%I0   , parstd%I0   ; print '(a,2f10.3)', 'Initial infected I0      and std dev :',p%I0     ,parstd%I0   
      read(10,*)p%Tinc , parstd%Tinc ; print '(a,2f10.3)', 'Incubation time          and std dev :',p%Tinc   ,parstd%Tinc 
      read(10,*)p%Tinf , parstd%Tinf ; print '(a,2f10.3)', 'Infection time           and std dev :',p%Tinf   ,parstd%Tinf 
      read(10,*)p%Trecm, parstd%Trecm; print '(a,2f10.3)', 'Recovery time mild       and std dev :',p%Trecm  ,parstd%Trecm
      read(10,*)p%Trecs, parstd%Trecs; print '(a,2f10.3)', 'Recovery time severe     and std dev :',p%Trecs  ,parstd%Trecs
      read(10,*)p%Thosp, parstd%Thosp; print '(a,2f10.3)', 'Hospitalization time     and std dev :',p%Thosp  ,parstd%Thosp
      read(10,*)p%Tdead, parstd%Tdead; print '(a,2f10.3)', 'Time to death            and std dev :',p%Tdead  ,parstd%Tdead
      read(10,*)p%CFR  , parstd%CFR  ; print '(a,2f10.3)', 'Critical fatality ratio  and std dev :',p%CFR    ,parstd%CFR  
      read(10,*)p%p_sev, parstd%p_sev; print '(a,2f10.3)', 'Fraction of severe cases and std dev :',p%p_sev  ,parstd%p_sev

      pfg=p ! store first guess of parameters

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

