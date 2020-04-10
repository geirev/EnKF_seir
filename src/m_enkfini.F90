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
   logical :: lrandrot=.false.
   logical :: lupdate_randrot=.false.
   logical :: lsymsqrt=.false.
   integer :: inflate=0
   integer :: infmult=1.0
   real :: relobserr
   real :: minobserr
   real :: maxobserr
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

contains
subroutine enkfini(nrens,nt,time)
   use m_getday
   implicit none
   integer, intent (in) :: nrens
   integer, intent (in) :: nt
   real,    intent (in) :: time
   real dt
   integer i,j,k,m
   integer iyear,imonth,iday,ideath,ihosp,iobst
   logical ex
   if (.not.lenkf) return

!  Observations
   inquire(file='corona.dat',exist=ex)
   if (.not.ex) then
      print '(a)','For EnKF, you must supply the fole corona.dat (example in src dir)'
      print '(a)','Running prior ensemble prediction'
      lenkf=.false.
      return
   endif
   print *
   print '(a)','Reading corona.dat'
   open(10,file='corona.dat')

! Counting number of lines in corona.dat
   do i=1,1000
      read(10,'(tr1,a5)',end=200)date
   enddo
   200 nrlines=i-1; print '(a,i5)','nrlines=',nrlines
   rewind(10)

! Allocate and read
   nrobs=2*nrlines; print '(a,i5)','nrobs=',nrobs
   allocate(dobs(nrobs))
   allocate(tobs(nrobs))
   allocate(iobs(nrobs))
   allocate(cobs(nrobs))
   m=0
   dt= time/real(nt)                     ! Timestep of outputs
   do i=1,nrlines
      read(10,'(i2,tr1,i2,tr1,i4,2i6)')iday,imonth,iyear,ideath,ihosp
      m=m+1
      tobs(m)=real(getday(iday,imonth,iyear))
      iobs(m)=nint(real(tobs(m))/dt)
      cobs(m)='d'
      dobs(m)=real(ideath)

      tobs(nrlines+m)=tobs(m)
      iobs(nrlines+m)=iobs(m)
      cobs(nrlines+m)='h'
      dobs(nrlines+m)=real(ihosp)
   enddo

   print '(a)','List of observations:'
   print '(a)','  number   gridp obstime obstype  obsval  stddev'
   do m=1,nrobs
      print '(3i8,a8,2f8.3)',m,iobs(m),tobs(m),cobs(m),dobs(m),min(maxobserr,max(relobserr*dobs(m),minobserr)) 
   enddo
   print *

   allocate(innovation(nrobs))
   allocate(D(nrobs,nrens))
   allocate(Dprt(nrobs,nrens))
   allocate(S(nrobs,nrens))
   allocate(E(nrobs,nrens))
   allocate(R(nrobs,nrobs))

!   if (lmeascorr) then
!      call pseudo1D(E,nrobs,nrens,cor1,dx,n1)
!      call fixsample1D(B,nx,nrens)
!   else 
      call random(E,nrobs*nrens)
!   endif

   R=0.0
   do m=1,nrobs
      R(m,m)=min(maxobserr,max(relobserr*dobs(m),minobserr))**2 
      E(m,:)=sqrt(R(m,m))*E(m,:)          
      Dprt(m,:)=dobs(m)+E(m,:)
   enddo

end subroutine
end module
