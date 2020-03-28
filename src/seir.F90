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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! slsode parameters and workspace
   integer :: itol=1
   real    :: rtol=1.0E-6
   real    :: atol=1.0e-8
   integer :: itask=1
   integer :: istate=1
   integer :: iopt=0
   integer, parameter :: lrw=20 + 16*neq     ! for mf=10
   real    :: rwork(lrw)
   integer, parameter :: liw=20              ! for mf=10
   integer :: iwork(liw)
   integer :: mf=10                          ! Nostiff Adams (no Jacobian)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Time stepping
   integer, parameter :: nt=200
   real tout,t,dt
   integer i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Ensemble variables
   integer, parameter :: nrens=100
   integer, parameter :: nrpar=16
   real ens(0:neq-1,0:nt,nrens)
   real enspar(1:nrpar,nrens)
   real parstd(1:nrpar)
   integer j


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Set parameters (decleared in mod_parameters.F90)
   Time_to_death     = 32.0                         ; parstd(1)=3.0   ! 1  Days to death
   N                 = 5000000.0                    ; parstd(2)=0.0   ! 2  Initial population
   I0                = 195.0                        ; parstd(3)=20.0  ! 3  Initial infectious
   R0                = 2.2                          ; parstd(4)=0.2   ! 4  Basic Reproduction Number
   D_incbation       = 5.2                          ; parstd(5)=1.0   ! 5  Incubation period (Tinc)
   D_infectious      = 2.9                          ; parstd(6)=1.0   ! 6  Duration patient is infectious (Tinf)
   D_recovery_mild   = 14.0 - D_infectious          ; parstd(7)=2.0   ! 7  Recovery time mild cases (11.1)
   D_recovery_severe = 31.5 - D_infectious          ; parstd(8)=2.0   ! 8  Recovery time severe cases (28.6) Lengt of hospital stay
   D_death           = Time_to_death-D_infectious   ; parstd(9)=2.0   ! 9  Time sick
   D_hospital_lag    = 5.0                          ; parstd(10)=2.0  ! 10 Time to hospitalization.
   CFR               = 0.02                         ; parstd(11)=0.002! 11 Case fatality rate 
   Time              = 365.0                        ; parstd(12)=0.0  ! 12 Length of simulation
   p_severe          = 0.2                          ; parstd(13)=0.1  ! 13 Hospitalization rate % for severe cases
   Rt                = 0.9                          ; parstd(14)=0.1  ! 14 Basic Reproduction Number during intervention
   InterventionTime  = 30.0                         ; parstd(15)=2.0  ! 15 Interventions start here
   duration          = 500                          ; parstd(16)=0.0  ! 16 Duration of measures

   call random(enspar,nrpar*nrens)
   do j=1,nrens
      enspar(:,j)=parstd(:)*enspar(:,j)
      call mod2ens(nrpar,nrens,enspar,j)
   enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initial conditions
   y(0) = (N-I0)/N    ! Susceptible (Normalized initial nonimmune population)
   y(1) = 0.0         ! Exposed 
   y(2) = I0/N        ! Infected
   y(3) = 0.0         ! Mild
   y(4) = 0.0         ! Severe   (severe at home)
   y(5) = 0.0         ! Severe_H (severe at hospital)
   y(6) = 0.0         ! Severe at hospital that will die   
   y(7) = 0.0         ! R_mild   (recovered)
   y(8) = 0.0         ! R_severe (recovered)
   y(9) = 0.0         ! R_fatal (dead)

! Randomizing number of initially infected
   call random(ens(2,0,:),nrens)
   ens(2,0,:)=parstd(3)*ens(2,0,:) / N

   do j=1,nrens
      ens(3:9,0,j)=y(3:9)
      ens(2,0,j)= ens(2,0,j)+y(2)
      ens(0,0,j)=1.0-ens(2,0,j)
   enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   dt= time/real(nt-1)

   do j=1,nrens        ! ensemble loop
      y(:)=ens(:,0,j) 
      call ens2mod(nrpar, nrens, enspar , j)

      istate=1
      do i=1,nt 
         t=0+real(i)*dt
         tout=t+dt
         call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
         if (istate < 0) then
            print '(a,i3)','negative istate, exiting: ',istate
            stop
         endif
         ens(:,i,j)=y(:)
      enddo

   enddo

   call tecplot(ens,nt,nrens,neq)

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

   S        = y(0) ! Susectable
   E        = y(1) ! Exposed
   I        = y(2) ! Infected
   Mild     = y(3) ! Recovering (Mild)
   Severe   = y(4) ! Recovering (Severe at home)
   Severe_H = y(5) ! Recovering (Severe in hospital)
   Fatal    = y(6) ! Recovering (Fatal)
   R_Mild   = y(7) ! Rehibilitated
   R_Severe = y(8) ! Rehibilitated
   R_Fatal  = y(9) ! Dead

   p_fatal  = CFR
   p_mild   = 1 - P_SEVERE - CFR

   ydot(0) = -beta*I*S
   ydot(1) =  beta*I*S - a*E
   ydot(2) =  a*E - b*I
   ydot(3) =  p_mild*b*I   - (1/D_recovery_mild)*Mild
   ydot(4) =  p_severe*b*I - (1/D_hospital_lag)*Severe
   ydot(5) =  (1/D_hospital_lag)*Severe - (1/D_recovery_severe)*Severe_H
   ydot(6) =  p_fatal*b*I  - (1/D_death)*Fatal
   ydot(7) =  (1/D_recovery_mild)*Mild
   ydot(8) =  (1/D_recovery_severe)*Severe_H
   ydot(9) =  (1/D_death)*Fatal
end subroutine
 
subroutine jac(neq, t, y, ml, mu, pd, nrowpd)
   use mod_parameters
   implicit none
   integer neq, ml, mu, nrowpd
   real t, y(neq), pd(nrowpd,neq)
   pd=0.0
!   N=sum(y(1:4))
!   pd(1,1) = -Rt*y(3)/(Tinf*N)
!   pd(1,3) = -Rt*y(1)/(Tinf*N)
!   pd(2,1) =  Rt*y(3)/(Tinf*N)
!   pd(2,2) = -1.0/Tinc
!   pd(2,3) =  Rt*y(1)/(Tinf*N)
!   pd(3,2) =  1.0/Tinc 
!   pd(3,3) = -1.0/Tinf        
!   pd(4,3) =  1.0/Tinf        
end subroutine

