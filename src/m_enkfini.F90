module m_enkfini  
use m_random
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF variables read from infile.in
   integer nesmda                ! Number of esmda steps
   integer nrobs                 ! Number of observations
   integer, parameter :: ne=1    ! extended E ensemble
   logical lenkf                 ! True if running analyses
   logical lmeascorr             ! include correlated measurement errors
   real    rh                    ! measurement error decorrelation time
   logical ld,lh,lc              ! True for conditioning on deaths, hopsitalized and total number of cases
   real relerrd, minerrd, maxerrd 
   real relerrh, minerrh, maxerrh 
   real relerrc, minerrc, maxerrc, cfrac      
   integer  mode_analysis        ! Run with 13
   real :: truncation            ! singular value truncation

! some defaults 
   logical :: lrandrot=.true.    ! Random rotation (only used in sqrt scheme)
   logical :: lupdate_randrot=.true. ! Update random (only used in sqrt scheme with local analysis)
   logical :: lsymsqrt=.true.    ! Symmetric square rood (only used in sqrt scheme)
   integer :: inflate=0          ! Don't inflate
   integer :: infmult=1.0        ! Inflation multiplier


!observations class
   type observation
      real             d      ! Observation value
      integer          i      ! Observation time index
      integer          t      ! Observation time (days from 1. March)
      integer          ic     ! Observation corresponding to county ic
      character(len=1) c      ! Observation type (d,h,c)
   end type
   type(observation), allocatable :: obs(:)

! matrices used to compute analysis
   real, allocatable    :: innovation(:) ! Observation innovation dobs-y
   real, allocatable    :: D(:,:)        ! dobs+eps - y
   real, allocatable    :: S(:,:)        ! Ensemble of predicted observation anomalies
   real, allocatable    :: E(:,:)        ! Ensemble of measurement perturbations
   real, allocatable    :: R(:,:)        ! Measurement error covariance matrix
   real, allocatable    :: Rprt(:)       ! Measurement error covariance matrix

contains
subroutine enkfini(time)
   use mod_dimensions
   use m_getday
   implicit none
   real,    intent (in) :: time
   real dt,tmptime
   integer i,j,k,m,ic
   integer iyear,imonth,iday,ideath,ihosp,icase,iobst
   integer nrlines(nc)  ! Number of lines in observations files
   logical ex
   character(len=3) tag3
   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Read observations and define EnKF matrices (m_enkfini.F90)'
   if (.not.lenkf) return

!  Observations
   nrobs=0
   do ic=1,nc
      nrlines(ic)=0
      write(tag3,'(i3.3)')ic
      inquire(file='corona'//tag3//'.in',exist=ex)
      if (ex) then
         print '(tr3,a)','Counting number of lines and observations in corona'//tag3//'.in'
         open(10,file='corona'//tag3//'.in')
! Counting number of lines and observations in coronaxxx.in
            do i=1,10000
               read(10,'(i2,tr1,i2,tr1,i4)',advance='no',end=200)iday,imonth,iyear
                  tmptime=real(getday(iday,imonth,iyear))
                  if (tmptime > real(nt)) then
                     print '(a,i5,a)','Warning: you have set the end of simulation nt=',nt,' earlier than the last measurements'
                     print *,'Warning: I will skip these measurements in the assimilation'
                     exit
                  endif
               read(10,*)ideath,ihosp,icase
               if (ld .and. ideath > 0) then
                  nrobs=nrobs+1
               endif
               if (lh .and. ihosp  > 0) then
                  nrobs=nrobs+1
               endif
               if (lc .and. icase  > 0) then
                  nrobs=nrobs+1
               endif
            enddo
            200 nrlines(ic)=i-1
         close(10)
      endif
      print '(tr3,a,a,tr3,a,i5,a,i5)','Country:',tag3,'nrlines=',nrlines(ic),'  nrobs=',nrobs
   enddo

   if (nrobs>0) then
      allocate(obs(nrobs))
      m=0                                     ! Observation counter
      dt= time/real(nt-1)                     ! Timestep of outputs

      do ic=1,nc
         write(tag3,'(i3.3)')ic
         inquire(file='corona'//tag3//'.in',exist=ex)
         if (ex) then
            print '(tr3,a)','Reading observations from corona'//tag3//'.in'
            open(10,file='corona'//tag3//'.in')
               do i=1,nrlines(ic)
                  read(10,'(i2,tr1,i2,tr1,i4)',advance='no')iday,imonth,iyear
                  tmptime=real(getday(iday,imonth,iyear))
                  if (tmptime > real(nt)) exit
                  read(10,*)ideath,ihosp,icase
                  if (ld .and. ideath > 0) then
                     m=m+1
                     obs(m)%t=real(getday(iday,imonth,iyear))
                     obs(m)%i=nint(real(obs(m)%t)/dt)
                     obs(m)%c='d'
                     obs(m)%ic=ic
                     obs(m)%d=real(ideath)
                  endif
                  if (lh .and. ihosp > 0) then
                     m=m+1
                     obs(m)%t=real(getday(iday,imonth,iyear))
                     obs(m)%i=nint(real(obs(m)%t)/dt)
                     obs(m)%c='h'
                     obs(m)%ic=ic
                     obs(m)%d=real(ihosp)
                  endif
                  if (lc .and. icase > 0) then
                     m=m+1
                     obs(m)%t=real(getday(iday,imonth,iyear))
                     obs(m)%i=nint(real(obs(m)%t)/dt)
                     obs(m)%c='c'
                     obs(m)%ic=ic
                     obs(m)%d=real(icase)
                  endif
               enddo
            close(10)
         endif
      enddo
   else
      print '(tr3,a)','WARNING: For EnKF, you must supply the file corona'//tag3//'.in'
      print '(tr3,a)','WARNING: Running only prior ensemble prediction'
      lenkf=.false.
      return
   endif



   print '(tr3,a)','List of observations:'
   print '(tr3,a)','  number   gridp obstime obstype  country      obsval'
   do m=1,nrobs
      write(tag3,'(i3.3)')obs(m)%ic
      print '(tr3,3i8,a8,tr6,a3,f12.3)',m,obs(m)%i,obs(m)%t,obs(m)%c,tag3,obs(m)%d 
   enddo


! Allocating variables used in the analysis scheme
   allocate(innovation(nrobs))
   allocate(D(nrobs,nrens))
   allocate(S(nrobs,nrens))
   allocate(E(nrobs,nrens))
   allocate(R(nrobs,nrobs))
   allocate(Rprt(nrobs))

! For printing perturbed observations
   do m=1,nrobs
      select case (obs(m)%c)
      case ('d')
         Rprt(m)=min(maxerrd,max(relerrd*obs(m)%d,minerrd))**2 
      case ('h')
         Rprt(m)=min(maxerrh,max(relerrh*obs(m)%d,minerrh))**2 
      case ('c')
         Rprt(m)=min(maxerrc,max(relerrc*obs(m)%d,minerrc))**2 
      end select
   enddo
   print '(a)','Done reading observations and defining EnKF matrices (m_enkfini.F90)'
end subroutine
end module
