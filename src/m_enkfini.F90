module m_enkfini  
use m_random
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF variables
   integer, save :: nrobs                 ! Number of observations
   integer nrlines               ! Number of lines in observations file
   integer, parameter :: ne=1    ! extended E ensemble
   logical    lenkf
   integer :: mode_analysis=11
   logical :: lrandrot=.false.
   logical :: lupdate_randrot=.false.
   logical :: lsymsqrt=.false.
   integer :: inflate=0
   integer :: infmult=1.0
   real :: truncation=0.999      ! singular value truncation
   real, allocatable    :: innovation(:) ! Observation innovation dobs-y
   integer, allocatable :: iobs(:)      ! Observation time index
   real, allocatable    :: dobs(:)       ! Observation values
   integer, allocatable :: tobs(:)       ! Observation times (days from 1. March)
   character(len=1), allocatable :: cobs(:)      ! Observation times (days from 1. March)
   real, allocatable    :: Dprt(:,:)     ! dobs+eps
   real, allocatable    :: D(:,:)        ! dobs+eps - y
   real, allocatable    :: S(:,:)        ! Ensemble of predicted observation anomalies
   real, allocatable    :: E(:,:)        ! Ensemble of measurement perturbations
   real, allocatable    :: R(:,:)        ! Measurement error covariance matrix
   character(len=5) date
   integer, parameter :: days_in_month(12) =(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
   integer imonth,iday,ideath,ihosp,iobst

contains
subroutine enkfini(nrens,nt,time)
   implicit none
   integer, intent (in) :: nrens
   integer, intent (in) :: nt
   real,    intent (in) :: time
   real dt
   integer i,j,k,m
   logical ex

!  Observations
   inquire(file='corona.dat',exist=ex)
   if (.not.ex) then
      print '(a)','You must supply the fole corona.dat (example in src dir)'
      stop
   endif
   open(10,file='corona.dat')
   do i=1,1000
      read(10,'(tr1,a5)',end=200)date; print '(i4,a)',i,date
   enddo
   200 nrlines=i-1; print '(a,i5)','nrlines=',nrlines
   rewind(10)
   nrobs=2*nrlines; print '(a,i5)','nrobs=',nrobs
   allocate(dobs(nrobs))
   allocate(tobs(nrobs))
   allocate(iobs(nrobs))
   allocate(cobs(nrobs))
   m=0
   dt= time/real(nt)                     ! Timestep of outputs
   do i=1,nrlines
      read(10,'(i2,tr1,i2,2i6)')iday,imonth,ideath,ihosp
      m=m+1
      tobs(m)=0
      do k=3,imonth-1
         tobs(m)=tobs(m)+days_in_month(k)
      enddo
      tobs(m)=tobs(m)+iday
      iobs(m)=nint(tobs(m)/dt)
      cobs(m)='d'
      dobs(m)=real(ideath)
      m=m+1
      tobs(m)=0
      do k=3,imonth-1
         tobs(m)=tobs(m)+days_in_month(k)
      enddo
      tobs(m)=tobs(m)+iday
      iobs(m)=nint(tobs(m)/dt)
      cobs(m)='h'
      dobs(m)=real(ihosp)
   enddo
   print '(a)','List of observations:'
   do m=1,nrobs
      print *,m,iobs(m),tobs(m),cobs(m),dobs(m)
   enddo


   allocate(innovation(nrobs))
   allocate(D(nrobs,nrens))
   allocate(Dprt(nrobs,nrens))
   allocate(S(nrobs,nrens))
   allocate(E(nrobs,nrens))
   allocate(R(nrobs,nrobs))

   lenkf=.true.     ! True to run EnKF
   call random(E,nrobs*nrens)
   R=0.0
   do m=1,nrobs
      R(m,m)=(0.05*dobs(m))**2
      E(m,:)=sqrt(R(m,m))*E(m,:)          
      D(m,:)=dobs(m)+E(m,:)
      Dprt(m,:)=dobs(m)+E(m,:)
   enddo
end subroutine
end module
