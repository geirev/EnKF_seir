module m_readinfile
use mod_dimensions
logical lrtime    ! Continuous R(t) 
real rdcorr       ! decorrelation time for R(t)
real qminf        ! If set to 0.0 < qminf <= 1.0 then qminf fraction of Qm contributes to spreading virus
real hos          ! The fraction of fatally ill that actually goes to the hospital.
logical lRrescale ! Rescales the Rmatrix_0? such that the effective R(t) is not dependent on Rmatrix
character(len=25) outdir

contains
subroutine readinfile()
   use mod_params
   use mod_parameters 
   use m_enkfini 
   use m_getday
   implicit none
   logical ex
   character(len=2) ca
   character(len=10) string
   integer id,im,iy,k,day
   integer ir,i,j,ii,nttmp
   real dt,t
   character(len=9) :: cmd='mkdir -p '

   inquire(file='infile.in',exist=ex)
   if (.not.ex) then
      print '(a)','Did not find inputfile infile.in...'
      stop
   endif
   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Reading infile.in (m_readinfile.F90):'
   open(10,file='infile.in')
      read(10,*)nrens          ;    print '(tr3,a,tr3,i4)',       'Number  of samples (nrens)           :',nrens
      read(10,*)time           ;    print '(tr3,a,f10.3)',        'Length of integration                :',time
      nt=nint(time)+1          ;    print '(tr3,a,tr3,i4)',       'nt (computed in seir)                :',nt
      read(10,'(a)')ca      
      if (ca /= '#1') then
         print *,'#1: error in infile.in'
         stop
      endif
      read(10,*)lenkf          ;    print '(tr3,a,tr3,l1)',       'Run enkf update (lenkf)              :',lenkf
      read(10,*)mode_analysis  ;    print '(tr3,a,tr3,i2)',       'Analysis mode of EnKF                :',mode_analysis
      read(10,*)nesmda         ;    print '(tr3,a,tr3,i2)',       'Number of ESMDA steps                :',nesmda
      read(10,*)ld, relerrd, minerrd, maxerrd 
               print '(tr3,a,tr3,l1,3f10.2)','D cond, relerr, minerr, maxerr       :',ld, relerrd, minerrd, maxerrd
      read(10,*)lh, relerrh, minerrh, maxerrh 
               print '(tr3,a,tr3,l1,3f10.2)','H cond, relerr, minerr, maxerr       :',lh, relerrh, minerrh, maxerrh
      read(10,*)lc, relerrc, minerrc, maxerrc , cfrac
               print '(tr3,a,tr3,l1,4f10.2)','C cond, relerr, minerr, maxerr, cfrac:',lc, relerrd, minerrd, maxerrd, cfrac
      read(10,*)lmeascorr      ;    print '(tr3,a,tr3,l1)',       'Activate corr. obs. err.             :',lmeascorr
      read(10,*)rh             ;    print '(tr3,a,f10.4)',        'Obs error decorrelation              :',rh
      read(10,*)truncation     ;    print '(tr3,a,f10.4)',        'EnKF SVD truncation (0.99)           :',truncation

      read(10,'(a)')ca      
      if (ca /= '#2') then
         print *,'#2: error in infile.in'
         stop
      endif

! Read startday of simulation - Running with Rmat(:,:,1)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      startday=getday(id,im,iy)-1        ! Subtract 1 to get to 00:00am in the morning
      print '(tr3,a,tr2,i3,i3,i5)',          'Start of simulation (00:00am)        :',id,im,iy
      print '(tr3,a,tr2,2i5)',               'Relative start day                   :',startday,365+31+29

! Read startday of 1st intervention - switching to Rmat(:,:,2)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv(1)=real(getday(id,im,iy))-1  ! Subtract 1 to get to 00:00am in the morning
      print '(tr3,a,tr3,i3,i3,i5,f10.2)',    'Start date of first  intervention    :',id,im,iy,Tinterv(1)

! Read startdate of 2nd intervention- switching to Rmat(:,:,3)
      read(10,'(tr1,i2,tr1,i2,tr1,i4)')id,im,iy
      Tinterv(2)=real(getday(id,im,iy))-1  ! Subtract 1 to get to 00:00am in the morning
      print '(tr3,a,tr3,i3,i3,i5,f10.2)',    'Start date of second intervention    :',id,im,iy,Tinterv(2)

      read(10,'(a)')ca      
      if (ca /= '#3') then
         print *,'#3: error in infile.in'
         stop
      endif

! MODEL PARAMETERS (Set first guess (ensemble mean) of parameters (decleared in mod_parameters.F90) and their stddev 
      read(10,*)lRrescale            ; print '(tr3,a,tr9,l1,f10.3)','Rescaling of Rmatrix_0?              :',lRrescale
      read(10,*)lrtime , rdcorr      ; print '(tr3,a,tr9,l1,f10.3)','R(t) (TF) and decorrelation legnth   :',lrtime   ,rdcorr
      read(10,*)p%Tinc , parstd%Tinc ; print '(tr3,a,2f10.3)',      'Incubation time          and std dev :',p%Tinc   ,parstd%Tinc
      read(10,*)p%Tinf , parstd%Tinf ; print '(tr3,a,2f10.3)',      'Infection time           and std dev :',p%Tinf   ,parstd%Tinf
      read(10,*)p%Trecm, parstd%Trecm; print '(tr3,a,2f10.3)',      'Recovery time mild       and std dev :',p%Trecm  ,parstd%Trecm
      read(10,*)p%Trecs, parstd%Trecs; print '(tr3,a,2f10.3)',      'Recovery time severe     and std dev :',p%Trecs  ,parstd%Trecs
      read(10,*)p%Thosp, parstd%Thosp; print '(tr3,a,2f10.3)',      'Hospitalization time     and std dev :',p%Thosp  ,parstd%Thosp
      read(10,*)p%Tdead, parstd%Tdead; print '(tr3,a,2f10.3)',      'Time to death            and std dev :',p%Tdead  ,parstd%Tdead
!      read(10,*)p%CFR(1) , parstd%CFR(1) 
!                  print '(tr3,a,2f10.3)',  'Critical fatality ratio  and std dev :',p%CFR(1) ,parstd%CFR(1)
!      read(10,*)p%sev(1),parstd%sev(1)
!                  print '(tr3,a,2f10.3)','Fraction of severe cases and std dev :',p%sev(1),parstd%sev(1)

! Some other model parameter that are not estimated
      read(10,*)hos                  ; print '(tr3,a,2f10.3)',  'Fraction of Qf that go to hospital   :',hos
      read(10,*)qminf                ; print '(tr3,a,2f10.3)',  'Fraction of Qm that is infecteous    :',qminf

      read(10,'(a)')ca      
      if (ca /= '#4') then
         print *,'#4: error in infile.in'
         stop
      endif

      read(10,*)minpar              ;    print '(tr3,a,f10.3)',    'Lower bound on all paramters         :',minpar
      read(10,*)rtmax               ;    print '(tr3,a,f10.3)',    'Maximum value of Rt during intervent :',rtmax
      read(10,*)outdir              ;    print '(tr3,a,a)',        'Output directory for storing results :',trim(outdir)
      call execute_command_line (cmd//outdir, exitstat=i)

      read(10,'(a)')ca      
      if (ca /= '#5') then
         print *,'#5: error in infile.in'
         stop
      endif

   close(10)
   print '(a)','Finished reading infile.in (m_readinfile.F90):'

   open(10,file='day_to_date.out')
      do i=0,nt
         string=getdate(real(i)+startday)
         read(string(1:2),'(i2.2)')id
         read(string(4:5),'(i2.2)')im
         read(string(7:10),'(i4.4)')iy
         write(10,*)'day to date =',i,string !,id,im,iy,getday(id,im,iy)
      enddo
   close(10)


end subroutine
end module

