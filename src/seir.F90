program seir
   use mod_parameters
   use m_ens2mod
   use m_mod2ens
   use m_tecplot
   use m_random
   use m_agegroups
   use m_Rmatrix
   use m_pfactors

   implicit none
   external f,jac
   integer, parameter :: neq=40 ! Number of equations
   real y(0:neq-1)              ! Solution S, E, I R
   logical ex

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! slsode parameters and workspace
   integer :: itol=1
   real    :: rtol=1.0E-5
   real    :: atol=1.0E-7
   integer :: itask=1
   integer :: istate=1
   integer :: iopt=0

   integer mf
   integer lrw,liw

   real,    allocatable  :: rwork(:) ! (lrw)
   integer, allocatable  :: iwork(:)  !(liw)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Time stepping
   integer, parameter :: nt=365           ! Number of outputs
   real tout,t,dt
   integer i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Ensemble variables
   integer, parameter :: nrens=99
   integer, parameter :: nrpar=13

   real enspar(1:nrpar+neq,nrens) ! Ensemble of state variables ( parameters + initial conditions)
   real parstd(1:nrpar)           ! Standard deviations of parameters
   integer j

   real ens(0:neq-1,0:nt,nrens)  ! storage of the ensemble of solutions for printing

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF variables
   integer, parameter :: nrobs=2 ! Number of observations
   integer, parameter :: ne=1    ! extended E ensemble
   logical    lenkf
   integer :: mode_analysis=11
   logical :: lrandrot=.false.
   logical :: lupdate_randrot=.false.
   logical :: lsymsqrt=.false.
   integer :: inflate=0
   integer :: infmult=1.0
   real :: truncation=0.999      ! singular value truncation
   real dobs(nrobs)              ! Observation values
   integer iobst                 ! time of measurement
   real innovation(nrobs)        ! Observation innovation dobs-y
   real D(nrobs,nrens)           ! dobs+eps - y
   real S(nrobs,nrens)           ! Ensemble of predicted observation anomalies
   real E(nrobs,nrens)           ! Ensemble of measurement perturbations
   real R(nrobs,nrobs)           ! Measurement error covariance matrix
   integer m
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   call agegroups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! SLSODE parameters
   mf=10  ! (10,21,22)
   lrw =max(20+16*neq, 22+9*neq+neq**2)
   liw = 20 + neq
   allocate(rwork(lrw))
   allocate(iwork(liw))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF initialization
!  Observations
   lenkf=.true.     ! True to run EnKF
   iobst=36         ! 31/3 day of measurment since first death 12th March
   dobs(1) =71.0    ! total deaths at day 31/3
   dobs(2) =311.0   ! total hospitalized at day 31/3
   R(1,1)=0.1; R(1,2)=0.0
   R(2,1)=0.0;  R(2,2)=0.01
   call random(E,nrobs*nrens)
   print *,'D: perturbed data'
   do m=1,nrobs
      E(m,:)=sqrt(R(m,m))*E(m,:)          
      D(m,:)=dobs(m)+E(m,:)
      print '(i3,100f10.2)',m,D(m,1:10)
   enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODEL PARAMETERS (Set first guess (ensemble mean) of parameters (decleared in mod_parameters.F90) and their stddev 
   Time_to_death     = 32.0                         ; parstd(1)=0.0    ! 1  Days to death
   N                 = sum(agegroup(:))             ; parstd(2)=0.0    ! 2  Initial population
   print *,'N=',N
   I0                = 51.0                         ; parstd(3)=6.0    ! 3  Initial infectious (19 cases 1st march)
   R0                = 3.8                          ; parstd(4)=0.5    ! 4  Basic Reproduction Number
   Tinc              = 5.2                          ; parstd(5)=0.0    ! 5  Incubation period (Tinc)
   Tinf              = 2.9                          ; parstd(6)=0.0    ! 6  Duration patient is infectious (Tinf)
   Trecm             = 14.0 - Tinf                  ; parstd(7)=0.0    ! 7  Recovery time mild cases (11.1)
   Trecs             = 31.5 - Tinf                  ; parstd(8)=0.0    ! 8  Recovery time severe cases Length of hospital stay
   Thosp             = 5.0                          ; parstd(9)=0.0    ! 10 Time to hospitalization.
   CFR               = 0.010                        ; parstd(10)=0.0020  ! 11 Case fatality rate 
   p_severe          = 0.020                        ; parstd(11)=0.0030  ! 12 Hospitalization rate % for severe cases
   Rt                = 0.9                          ; parstd(12)=0.100  ! 13 Basic Reproduction Number during intervention
   InterventionTime  = 15.0                         ; parstd(13)=0.0   ! 14 Interventions start here (15th march)

   duration= 30                             ! Duration of measures
   time=365.0                            ! Length of simulation
   dt= time/real(nt)                     ! Timestep of outputs

   call Rmatrix
   call pfactors


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! perturb first guess values of parameters and initial conditions
   call random(enspar(1:nrpar,1:nrens),nrpar*nrens)
   do j=1,nrens
      enspar(1:nrpar,j)=parstd(1:nrpar)*enspar(1:nrpar,j)
      call mod2ens(nrpar,nrens,neq,enspar,j)
   enddo
   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),0.0001) ! Ensure positive parameters
   enspar(12,:)=min(enspar(12,:),0.999)            ! Ensure Rt < 1.0

   print '(a)','Simple initialization'
   do j=1,nrens
      I0=enspar(3,j)     
      ens(0    :   na-1, 0, j) = agegroup(0:na-1)  ! Susceptible na agegroups                 S_i
      ens(na   : 2*na-1, 0, j) = 4*I0/real(na)     ! Exposed     na agegroups                 E_i
      ens(2*na : 3*na-1, 0, j) =   I0/real(na)     ! Infected    na agegroups                 I_i
      ens(3*na         , 0, j) = 0.0               ! Sick Mild                                Q_m
      ens(3*na+1       , 0, j) = 0.0               ! Sick (Severe at home)                    Q_s
      ens(3*na+2       , 0, j) = 0.0               ! Sick (Severe at hospital)                Q_h
      ens(3*na+3       , 0, j) = 0.0               ! Sick (Severe at hospital that will die)  Q_f
      ens(3*na+4       , 0, j) = 0.0               ! Removed_mild   (recovered)               R_m
      ens(3*na+5       , 0, j) = 0.0               ! Removed_severe (recovered)               R_s
      ens(3*na+6       , 0, j) = 0.0               ! Removed_fatal (dead)                     D
      ens(0:na-1 ,0, j) = ens(0:na-1, 0, j) - ens(na:2*na-1, 0, j) - ens(2*na : 3*na-1, 0, j)
   enddo
   ens(:,0,:)=ens(:,0,:)/N

! Copy initial ensemble to enspar for later updating
   enspar(nrpar+1:nrpar+neq,:)=ens(0:neq-1,0,:) 

   print '(a)','enspar INI'
   print '(a,100g11.4)','1', enspar(1:nrpar,1)
   print '(a,100g11.4)','2', enspar(1:nrpar,2)
   print '(a,100g11.4)','3', enspar(1:nrpar,3)
!   print '(a)','ens INI'
!   print '(a,100g13.4)','1', N*ens(:,0,1)
!   print '(a,100g13.4)','2', N*ens(:,0,2)
!   print '(a,100g13.4)','3', N*ens(:,0,3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   do j=1,nrens
      y(:)=ens(:,0,j) 
      call ens2mod(nrpar, nrens, neq, enspar , j)
      call pfactors 
      istate=1
      do i=1,nt 
         t=0+real(i)*dt
         tout=t+dt
         call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
         ens(:,i,j)=y(:)
!         print '(a,4f12.2)','SUM=',sum(y(0:neq-1))
         if (istate < 0) then
            print '(a,i3,a,i4)','negative istate, exiting: ',istate,',  it=0,  iens=',j
            stop
         endif
      enddo
   enddo
   call tecplot(ens,enspar,nt,nrens,neq,nrpar,0)
   if (.not.lenkf) stop

   print '(a)','Prior ensemble parameters:'
   do i=1,4
      print '(i2,100g12.3)',i, enspar(1:nrpar,i)
   enddo
   print '(a)','Prior ensemble initial conditions:'
   do i=1,4
      print '(i1,a)',i,':'
      print '(10g12.3)',N*ens(:,0,i)
   enddo 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF update
   innovation(:)=0.0                                  ! only for sqrt filters
   D(1,:) = D(1,:)-N*ens(3*na+6,iobst,:)                   ! Dead
   D(2,:) = D(2,:)-N*(ens(3*na+2,iobst,:)+ens(3*na+3,iobst,:))  ! Hospitelized

   print *,'Predicted measurements (10 realizations):'
   print '(a,100f10.2)','d(1)',N*ens(3*na+6,iobst,1:10)
   print '(a,100f10.2)','d(2)',N*(ens(3*na+2,iobst,1:10)+ens(3*na+3,iobst,1:10)) 

   print *,'D innovation'
   do m=1,nrobs
      print '(i3,100f10.2)',m,D(m,1:10)
   enddo

   S(1,:) = N*( ens(3*na+6,iobst,:) - sum(ens(3*na+6,iobst,:))/real(nrens) )           
   S(2,:) = N*( ens(3*na+2,iobst,:) - sum(ens(3*na+2,iobst,:))/real(nrens) &
              &+ens(3*na+3,iobst,:) - sum(ens(3*na+3,iobst,:))/real(nrens) )

   call analysis(enspar, R, E, S, D, innovation, nrpar+neq, nrens, nrobs, .true., truncation, mode_analysis, &
                 lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)

   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),0.0001)
   ens(0:neq-1,0,:)=max(enspar(nrpar+1:nrpar+neq,:),0.0)

   print '(a)','Posterior ensemble parameters:'
   do i=1,4
      print '(i2,100g12.3)',i, enspar(1:nrpar,i)
   enddo
   print '(a)','Posterior ensemble initial conditions:'
   do i=1,4
      print '(i1,a)',i,':'
      print '(10g13.4)', N*ens(:,0,i)
   enddo 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Posterior ensemble prediction
   do j=1,nrens
      y(:)=ens(:,0,j) 
      call ens2mod(nrpar, nrens, neq, enspar , j)
      call pfactors 
      istate=1
      do i=1,nt 
         t=0+real(i)*dt
         tout=t+dt
         call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
         ens(:,i,j)=y(:)
         if (istate < 0) then
            print '(a,i3,a,i4)','negative istate, exiting: ',istate,',  it=0,  iens=',j
            stop
         endif
      enddo
   enddo
   
   call tecplot(ens,enspar,nt,nrens,neq,nrpar,1)

end program

subroutine f(neq, t, y, ydot)
   use mod_parameters
   use m_agegroups
   use m_Rmatrix
   use m_pfactors
   use m_random

   implicit none
   integer i,k,ii,kk
   integer neq
   real t,beta(1)
   real y(0:neq-1)
   real ydot(0:neq-1)

   real R(0:na-1,0:na-1)

   if (t <= interventiontime) then
      R=R0
   elseif (interventiontime < t .and. t <= interventiontime + duration ) then
      R=Rt 
   elseif (t > interventiontime + duration) then
      call random(beta,1)
      R=Rmat  !*(1.0+0.1*beta(1))
   endif

!   S_i        = y(0    : na-1   ) ! Susectable
!   E_i        = y(na   : 2*na-1 ) ! Exposed
!   I_i        = y(2*na : 3*na-1 ) ! Infected
!   Q_m        = y(3*na) ! Recovering (Mild)
!   Q_s        = y(3*na+1) ! Recovering (Severe at home)
!   Q_h        = y(3*na+2) ! Recovering (Severe in hospital)
!   Q_f        = y(3*na+3) ! fatally sick to die
!   R_m        = y(3*na+4) ! Recovered
!   R_s        = y(3*na+5) ! Recovered 
!   D          = y(3*na+6) ! Dead
   
!   pf(:) = CFR
!   ps(:) = P_SEVERE
!   pm(:) = 1.0 - P_SEVERE - CFR
   Tdead  = Time_to_death-Tinf 

!   print '(100f8.4)',pm(:)
!   print '(100f8.4)',ps(:)
!   print '(100f8.4)',pf(:)
!   print '(a,100f8.4)','Tinf=',Tinf,Tinc,Trecm,Trecs,Thosp,Tdead  
!   print '(11f8.2)',R
!    stop 
! S
   do i=0,na-1
      ii=i
      ydot(ii) = 0.0
      do k=0,na-1
         kk=2*na+k
         ydot(ii) = ydot(ii) - (R(i,k)/Tinf)*y(kk)*y(ii)
      enddo
      !print '(a,i3,6g13.5)','S1',ii,y(ii),ydot(ii)
   enddo

! E
   do i=0,na-1
      ii=na+i       ! E
      ydot(ii) = - (1.0/Tinc)*y(ii)
      do k=0,na-1
         kk=2*na+k  ! I
         ydot(ii) = ydot(ii) + (R(i,k)/Tinf)*y(kk)*y(i)
      enddo
      !print '(a,i3,6g13.5)','E ',ii,y(ii),ydot(ii)
   enddo

! I
   do i=0,na-1
      ii=2*na+i     ! I
      kk=na+i       ! E
      ydot(ii) =   (1.0/Tinc)*y(kk) - (1.0/Tinf)*y(ii)
      !print '(a,i3,2g13.5)','I ',ii,y(ii),ydot(ii)
   enddo

! Qm
   ii=3*na          ! Qm
   ydot(ii) =  - (1.0/Trecm)*y(ii) 
   do k=0,na-1
      kk=2*na+k     ! I
      ydot(ii) = ydot(ii) + (pm(k)/Tinf)*y(kk)
      !print '(a,2i3,2g13.5)','Qm',ii,kk,y(ii),ydot(ii)
   enddo

! Qs
   ii=3*na+1        ! Qs
   ydot(ii) =  - (1.0/Thosp)*y(ii) 
   do k=0,na-1
      kk=2*na+k     ! I
      ydot(ii) = ydot(ii) + (ps(k)/Tinf)*y(kk)
      !print '(a,2i3,2g13.5)','Qs',ii,kk,y(ii),ydot(ii)
   enddo

! Qh
   ii=3*na+2        ! Qh
   kk=3*na+1        ! Qs
   ydot(ii) =  + (1.0/Thosp)*y(kk)  - (1.0/Trecs)*y(ii)
   !print '(a,2i3,2g13.5)','Qh',ii,kk,y(ii),ydot(ii)
   
! Qf
   ii=3*na+3        ! Qf
   ydot(ii) =  - (1.0/Tdead)*y(ii) 
   do k=0,na-1
      kk=2*na+k     ! I
      ydot(ii) = ydot(ii) + (pf(k)/Tinf)*y(kk)
      !print '(a,2i3,2g13.5)','Qf',ii,kk,y(ii),ydot(ii)
   enddo

! Rm
   ii=3*na+4        ! Rm
   kk=3*na
   ydot(ii) =   (1.0/Trecm)*y(kk) 
   !print '(a,i3,2g13.5)','Rm',ii,y(ii),ydot(ii)

! Rs
   ii=3*na+5        ! Rs
   kk=3*na+2        ! Qh
   ydot(ii) =   (1.0/Trecs)*y(kk) 
   !print '(a,i3,2g13.5)','Rs',ii,y(ii),ydot(ii)

! D
   ii=3*na+6        ! D
   kk=3*na+3        ! Qf
   ydot(ii) =   (1.0/Tdead)*y(kk) 
   !print '(a,i3,2g13.5)','D ',ii,y(ii),ydot(ii)

   !print *,'sumydot',sum(ydot(0:neq-1))

end subroutine
 
subroutine jac(neq, t, y, ml, mu, pd, nrowpd)
   use mod_parameters
   implicit none
   integer neq, ml, mu, nrowpd
   real t, y(0:neq-1), pd(0:neq-1,0:neq-1)
   real p_fatal
   real p_mild
   real beta,a,b

!   if ((t > interventiontime).and.(t < interventiontime + duration)) then
!      beta=Rt/Tinf
!   elseif (t > interventiontime + duration) then
!      beta=0.3/Tinf
!   else
!      beta=R0/Tinf
!   endif
!
!   p_fatal  = CFR
!   p_mild   = 1 - P_SEVERE - CFR
!   Tdead  = Time_to_death-Tinf 
!
!   pd=0.0
!   pd(0,0) = -beta*y(2)
!   pd(0,2) = -beta*y(0)
!   pd(1,0) =  beta*y(2)
!   pd(1,1) = -1.0/Tinc
!   pd(1,2) =  beta*y(0)
!   pd(2,1) =  1.0/Tinc
!   pd(2,2) = -1.0/Tinf
!   pd(3,2) =  p_mild*1.0/Tinf
!   pd(3,3) = -1.0/Trecm
!   pd(4,2) =  p_severe*1.0/Tinf
!   pd(4,4) = -1.0/Thosp
!   pd(5,4) =  1.0/Thosp
!   pd(5,5) = -1.0/Trecs
!   pd(6,2) =  p_fatal*1.0/Tinf
!   pd(6,6) = -1.0/Tdead
!   pd(7,3) =  1.0/Trecm
!   pd(8,5) =  1.0/Trecs
!   pd(9,6) =  1.0/Tdead
!
end subroutine

