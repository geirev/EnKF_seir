program seir
   use mod_parameters
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
   integer, parameter :: nt=1000
   real tout,t,dt
   integer i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Set parameters (decleared in mod_parameters.F90)
   Time_to_death     = 32.0                  ! Days to death
   N                 = 5178365.0             ! Initial population
   I0                = 195.0                 ! Initial infectious
   R0                = 2.2                   ! Basic Reproduction Number
   D_incbation       = 5.2                   ! Incubation period (Tinc)
   D_infectious      = 2.9                   ! Duration patient is infectious (Tinf)
   D_recovery_mild   = (14.0 - 2.9)          ! Recovery time mild cases (11.1)
   D_recovery_severe = (31.5 - 2.9)          ! Recovery time severe cases (28.6) Lengt of hospital stay
   D_hospital_lag    = 5.0                   ! Time to hospitalization.
   D_death           = Time_to_death - D_infectious
   CFR               = 0.02                  ! Case fatality rate 
   Time              = 365.0                 ! Length of simulation
   p_severe          = 0.2                   ! Hospitalization rate % for severe cases
   Rt                = 0.9                   ! Basic Reproduction Number during intervention
   InterventionTime  = 60.0                  ! Interventions start here
   duration          = 500                   ! Duration of measures

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

   open(10,file='seir.dat')
   write(10,*)'TITLE = "Solutions"'
   write(10,*)'VARIABLES = "time" &
!             &"S" "E" "I" "Mild" "Severe" "Hospital" "Fatal" "R_Mild" "R_Severe" "R_fatal" &
             &"Susceptible" "Dead" "Hospitalized" "Recovered" "Infected" "Exposed"' 
   write(10,'(a,i5,a,i5,a)')' ZONE T="Average"  F=POINT, I=',nt,', J=1, K=1'
   write(10,'(20g13.4)')t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1) 
   print '(20f12.2)',t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1), y(0)+y(9)+y(7)+y(8) 

   dt= time/real(nt-1)
   do i=1,nt
      t=0+real(i)*dt
      tout=t+dt


      call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
      if (istate < 0) then
         print '(a,i3)','negative istate, exiting: ',istate
         stop
      endif
      write(10,'(20g13.4)')t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1) 
      print '(20f12.2)',t,N*y(0),N*y(9),N*(y(5)+y(6)),N*(y(7) + y(8)), N*y(2), N*y(1), y(0)+y(9)+y(7)+y(8) 
   enddo
   close(10)

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
