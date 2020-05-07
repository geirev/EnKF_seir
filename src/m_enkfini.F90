module m_enkfini  
use m_random
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF variables
   integer :: nesmda             ! Number of observations
   integer :: nrobs              ! Number of observations
   integer nrlines               ! Number of lines in observations file
   integer, parameter :: ne=1    ! extended E ensemble
   logical    lenkf
   logical lmeascorr
   real    rh
   integer  mode_analysis
   logical :: lrandrot=.true.
   logical :: lupdate_randrot=.true.
   logical :: lsymsqrt=.true.
   integer :: inflate=0
   integer :: infmult=1.0
   real :: relobserr
   real :: minobserr
   real :: maxobserr
   real :: truncation                    ! singular value truncation
   real, allocatable    :: innovation(:) ! Observation innovation dobs-y
   integer, allocatable :: iobs(:)      ! Observation time index
   real, allocatable    :: dobs(:)       ! Observation values
   integer, allocatable :: tobs(:)       ! Observation times (days from 1. March)
   character(len=1), allocatable :: cobs(:)      ! Observation times (days from 1. March)
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
   real dt
   integer i,j,k,m
   integer iyear,imonth,iday,ideath,ihosp,iobst
   logical ex
   if (.not.lenkf) return

!  Observations
   inquire(file='corona.in',exist=ex)
   if (.not.ex) then
      print '(a)','WARNING: For EnKF, you must supply the file corona.in'
      print '(a)','WARNING: Running only prior ensemble prediction'
      lenkf=.false.
      return
   endif
   print *
   print '(a)','Reading corona.in'
   open(10,file='corona.in')

! Counting number of lines in corona.in
   nrobs=0
   do i=1,10000
      read(10,'(i2,tr1,i2,tr1,i4)',advance='no',end=200)iday,imonth,iyear
      read(10,*)ideath,ihosp
      if (ideath > 0) then
         nrobs=nrobs+1
      endif
      if (ihosp  > 0) then
         nrobs=nrobs+1
      endif
   enddo
   200 nrlines=i-1; print '(a,i5)','nrlines=',nrlines
   rewind(10)

   print '(a,i5)','nrobs=',nrobs
   allocate(dobs(nrobs))
   allocate(tobs(nrobs))
   allocate(iobs(nrobs))
   allocate(cobs(nrobs))
   m=0
   dt= time/real(nt-1)                     ! Timestep of outputs

   do i=1,nrlines
      read(10,'(i2,tr1,i2,tr1,i4)',advance='no')iday,imonth,iyear
      read(10,*)ideath,ihosp
      if (ideath > 0) then
         m=m+1
         tobs(m)=real(getday(iday,imonth,iyear))
         iobs(m)=nint(real(tobs(m))/dt)
         cobs(m)='d'
         dobs(m)=real(ideath)
      endif
   enddo
   rewind(10)
   do i=1,nrlines
      read(10,'(i2,tr1,i2,tr1,i4)',advance='no')iday,imonth,iyear
      read(10,*)ideath,ihosp
      if (ihosp > 0) then
         m=m+1
         tobs(m)=real(getday(iday,imonth,iyear))
         iobs(m)=nint(real(tobs(m))/dt)
         cobs(m)='h'
         dobs(m)=real(ihosp)
      endif
   enddo
   close(10)

   print '(a)','List of observations:'
   print '(a)','  number   gridp obstime obstype  obsval  stddev'
   do m=1,nrobs
      print '(3i8,a8,2f12.3)',m,iobs(m),tobs(m),cobs(m),dobs(m),min(maxobserr,max(relobserr*dobs(m),minobserr)) 
   enddo
   print *

   allocate(innovation(nrobs))
   allocate(D(nrobs,nrens))
   allocate(S(nrobs,nrens))
   allocate(E(nrobs,nrens))
   allocate(R(nrobs,nrobs))
   allocate(Rprt(nrobs))

! For printing perturbed observations
   do m=1,nrobs
      Rprt(m)=min(maxobserr,max(relobserr*dobs(m),minobserr))**2 
   enddo
end subroutine
end module
