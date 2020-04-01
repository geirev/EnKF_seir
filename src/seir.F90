program seir
   use mod_parameters
   use m_ens2mod
   use m_mod2ens
   use m_tecplot
   use m_random
   implicit none
   external f,jac
   integer, parameter :: neq=10 ! Number of equations
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
   integer, parameter :: nrens=999
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
! SLSODE parameters
   mf=21  ! (10,21,22)
   lrw =max(20+16*neq, 22+9*neq+neq**2)
   liw = 20 + neq
   allocate(rwork(lrw))
   allocate(iwork(liw))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF initialization
!  Observations
   lenkf=.true.     ! True to run EnKF
   iobst=31         ! 31/3 day of measurment since first death 12th March
   dobs(1) =34.0    ! total deaths at day 31/3
   dobs(2) =319.0   ! total hospitalized at day 31/3
   R(1,1)=0.01; R(1,2)=0.0
   R(2,1)=0.0; R(2,2)=0.01
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
   N                 = 5000000.0                    ; parstd(2)=0.0    ! 2  Initial population
   I0                = 40.0                         ; parstd(3)=5.0    ! 3  Initial infectious (19 cases 1st march)
   R0                = 2.2                          ; parstd(4)=0.25   ! 4  Basic Reproduction Number
   D_incbation       = 5.2                          ; parstd(5)=0.0    ! 5  Incubation period (Tinc)
   D_infectious      = 2.9                          ; parstd(6)=0.0    ! 6  Duration patient is infectious (Tinf)
   D_recovery_mild   = 14.0 - D_infectious          ; parstd(7)=0.0    ! 7  Recovery time mild cases (11.1)
   D_recovery_severe = 31.5 - D_infectious          ; parstd(8)=0.0    ! 8  Recovery time severe cases Length of hospital stay
   D_hospital_lag    = 5.0                          ; parstd(9)=0.0    ! 10 Time to hospitalization.
   CFR               = 0.04                         ; parstd(10)=0.002 ! 11 Case fatality rate 
   p_severe          = 0.15                         ; parstd(11)=0.0   ! 12 Hospitalization rate % for severe cases
   Rt                = 0.9                          ; parstd(12)=0.05  ! 13 Basic Reproduction Number during intervention
   InterventionTime  = 15.0                         ; parstd(13)=0.0   ! 14 Interventions start here (15th march)

   duration= 500                         ! Duration of measures
   time=365.0                            ! Length of simulation
   dt= time/real(nt-1)                   ! Timestep of outputs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! perturb first guess values of parameters and initial conditions
   call random(enspar(1:nrpar,1:nrens),nrpar*nrens)
   do j=1,nrens
      enspar(1:nrpar,j)=parstd(1:nrpar)*enspar(1:nrpar,j)
      call mod2ens(nrpar,nrens,neq,enspar,j)
   enddo
   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),0.01) ! Ensure positive parameters
   enspar(12,:)=min(enspar(12,:),0.999)          ! Ensure Rt < 1.0

   print '(a)','Simple initialization'
   do j=1,nrens
      I0=enspar(3,j)     
      ens(0,0,j)=(N-I0)   ! Susceptible (Normalized initial nonimmune population)   (S       )    S
      ens(1,0,j)=4*I0     ! Exposed                                                 (E       )    E
      ens(2,0,j)=I0       ! Infected                                                (I       )    I
      ens(3,0,j)=0.0      ! Sick Mild                                               (Mild    )    S_mild
      ens(4,0,j)=0.0      ! Sick (Severe at home)                                   (Severe  )    S_home
      ens(5,0,j)=0.0      ! Sick (Severe at hospital)                               (Severe_H)    S_hosp
      ens(6,0,j)=0.0      ! Sick (Severe at hospital that will die)                 (Fatal   )    S_mort
      ens(7,0,j)=0.0      ! Removed_mild   (recovered)                              (R_Mild  )    R_mild
      ens(8,0,j)=0.0      ! Removed_severe (recovered)                              (R_Severe)    R_seve
      ens(9,0,j)=0.0      ! Removed_fatal (dead)                                    (R_Fatal )    R_dead
   enddo
   ens(:,0,:)=ens(:,0,:)/N

! Copy initial ensemble to enspar for later updating
   enspar(nrpar+1:nrpar+neq,:)=ens(0:neq-1,0,:) 

   print '(a)','enspar INI'
   print '(a,100g13.4)','1', enspar(1:nrpar,1)
   print '(a,100g13.4)','2', enspar(1:nrpar,2)
   print '(a,100g13.4)','3', enspar(1:nrpar,3)
   print '(a)','ens INI'
   print '(a,100g13.4)','1', N*ens(:,0,1)
   print '(a,100g13.4)','2', N*ens(:,0,2)
   print '(a,100g13.4)','3', N*ens(:,0,3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   do j=1,nrens
      y(:)=ens(:,0,j) 
      call ens2mod(nrpar, nrens, neq, enspar , j)
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
   call tecplot(ens,enspar,nt,nrens,neq,nrpar,0)

   if (.not.lenkf) stop

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF update
   innovation(:)=0.0                                  ! only for sqrt filters
   D(1,:) = D(1,:)-N*ens(9,iobst,:)                   ! Dead
   D(2,:) = D(2,:)-N*(ens(5,iobst,:)+ens(6,iobst,:))  ! Hospitelized

   print *,'Predicted measurements (10 realizations):'
   print '(a,100f10.2)','d(1)',N*ens(9,iobst,1:10)
   print '(a,100f10.2)','d(2)',N*(ens(5,iobst,1:10)+ens(6,iobst,1:10)) 

   print *,'D innovation'
   do m=1,nrobs
      print '(i3,100f10.2)',m,D(m,1:10)
   enddo

   S(1,:) = N*( ens(9,iobst,:) - sum(ens(9,iobst,:))/real(nrens) )           
   S(2,:) = N*( ens(5,iobst,:) - sum(ens(5,iobst,:))/real(nrens) &
              &+ens(6,iobst,:) - sum(ens(6,iobst,:))/real(nrens) )

   call analysis(enspar, R, E, S, D, innovation, nrpar+neq, nrens, nrobs, .true., truncation, mode_analysis, &
                 lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)

   enspar(1:nrpar,:)=max(enspar(1:nrpar,:),0.01)
   ens(0:neq-1,0,:)=max(enspar(nrpar+1:nrpar+neq,:),0.0)

   print '(a)','enspar ANA'
   print '(a,100g13.4)','1', enspar(1:nrpar,1)
   print '(a,100g13.4)','2', enspar(1:nrpar,2)
   print '(a,100g13.4)','3', enspar(1:nrpar,3)
   print '(a)','ens FIN'
   print '(a,100g13.4)','1', N*ens(:,0,1)
   print '(a,100g13.4)','2', N*ens(:,0,2)
   print '(a,100g13.4)','3', N*ens(:,0,3)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Posterior ensemble prediction
   do j=1,nrens
      y(:)=ens(:,0,j) 
      call ens2mod(nrpar, nrens, neq, enspar , j)
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
   implicit none
   integer neq
   real t 
   real y(0:neq-1)
   real ydot(0:neq-1)
   real S        
   real E        
   real I        
   real Mild     
   real Severe   
   real Severe_H 
   real Fatal    
   real R_Mild   
   real R_Severe 
   real R_Fatal  

   real p_fatal
   real p_mild
   real beta,a,b

   if ((t > interventiontime).and.(t < interventiontime + duration)) then
      beta=Rt/D_infectious
   elseif (t > interventiontime + duration) then
      beta=1.0/D_infectious
   else
      beta=R0/D_infectious
   endif
   a= 1.0/D_incbation
   b= 1.0/D_infectious

!   S        = y(0) ! Susectable
!   E        = y(1) ! Exposed
!   I        = y(2) ! Infected
!   Mild     = y(3) ! Recovering (Mild)
!   Severe   = y(4) ! Recovering (Severe at home)
!   Severe_H = y(5) ! Recovering (Severe in hospital)
!   Fatal    = y(6) ! Recovering (Fatal)
!   R_Mild   = y(7) ! Rehibilitated
!   R_Severe = y(8) ! Rehibilitated
!   R_Fatal  = y(9) ! Dead

   p_fatal  = CFR
   p_mild   = 1 - P_SEVERE - CFR
   D_death  = Time_to_death-D_infectious   ! 9  Time sick

!   ydot(0) = -beta*I*S
!   ydot(1) =  beta*I*S - a*E
!   ydot(2) =  a*E - b*I
!   ydot(3) =  p_mild*b*I   - (1/D_recovery_mild)*Mild
!   ydot(4) =  p_severe*b*I - (1/D_hospital_lag)*Severe
!   ydot(5) =  (1/D_hospital_lag)*Severe - (1/D_recovery_severe)*Severe_H
!   ydot(6) =  p_fatal*b*I  - (1/D_death)*Fatal
!   ydot(7) =  (1/D_recovery_mild)*Mild
!   ydot(8) =  (1/D_recovery_severe)*Severe_H
!   ydot(9) =  (1/D_death)*Fatal

   ydot(0) = -beta*y(2)*y(0)
   ydot(1) =  beta*y(2)*y(0) - a*y(1)
   ydot(2) =  a*y(1) - b*y(2)
   ydot(3) =  p_mild*b*y(2)   - (1.0/D_recovery_mild)*y(3)
   ydot(4) =  p_severe*b*y(2) - (1.0/D_hospital_lag)*y(4)
   ydot(5) =  (1.0/D_hospital_lag)*y(4) - (1.0/D_recovery_severe)*y(5)
   ydot(6) =  p_fatal*b*y(2)  - (1.0/D_death)*y(6)
   ydot(7) =  (1.0/D_recovery_mild)  *y(3)
   ydot(8) =  (1.0/D_recovery_severe)*y(5)
   ydot(9) =  (1.0/D_death)          *y(6)
end subroutine
 
subroutine jac(neq, t, y, ml, mu, pd, nrowpd)
   use mod_parameters
   implicit none
   integer neq, ml, mu, nrowpd
   real t, y(0:neq-1), pd(0:neq-1,0:neq-1)
   real p_fatal
   real p_mild
   real beta,a,b

   if ((t > interventiontime).and.(t < interventiontime + duration)) then
      beta=Rt/D_infectious
   elseif (t > interventiontime + duration) then
      beta=1.0/D_infectious
   else
      beta=R0/D_infectious
   endif

   a= 1.0/D_incbation
   b= 1.0/D_infectious

   p_fatal  = CFR
   p_mild   = 1 - P_SEVERE - CFR
   D_death  = Time_to_death-D_infectious   ! 9  Time sick

   pd=0.0
   pd(0,0) = -beta*y(2)
   pd(0,2) = -beta*y(0)
   pd(1,0) =  beta*y(2)
   pd(1,1) = -a
   pd(1,2) =  beta*y(0)
   pd(2,1) =  a
   pd(2,2) = -b
   pd(3,2) =  p_mild*b
   pd(3,3) = -1.0/D_recovery_mild
   pd(4,2) =  p_severe*b
   pd(4,4) = -1.0/D_hospital_lag
   pd(5,4) =  1.0/D_hospital_lag
   pd(5,5) = -1.0/D_recovery_severe
   pd(6,2) =  p_fatal*b
   pd(6,6) = -1.0/D_death
   pd(7,3) =  1.0/D_recovery_mild
   pd(8,5) =  1.0/D_recovery_severe
   pd(9,6) =  1.0/D_death

end subroutine

