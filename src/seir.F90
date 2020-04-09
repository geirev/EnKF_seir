program seir
   use mod_parameters
   use m_ens2mod
   use m_mod2ens
   use m_tecplot
   use m_random
   use m_agegroups
   use m_Rmatrix
   use m_pfactors
   use m_enkfini
   use m_enkfprep
   use m_enkfpost
   use m_solve
   use m_readinputs
   use m_inipar
   use m_iniens

   implicit none
   integer, parameter :: neq=40                ! Number of equations
   integer, parameter :: nrpar=13              ! Number of uncertain model parameters
   integer  nrens                              ! Ensemble size (from infile.in)
   integer  nt                                 ! Number of output times (from infile.in)

   integer i,k,j,m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real, allocatable :: ens(:,:,:)             ! storage of the ensemble of solutions for printing
   real, allocatable :: enspar(:,:)            ! Ensemble of state variables ( parameters + initial conditions)
   real parstd(nrpar)                          ! Standard deviations of parameters

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization 
   call readinputs(parstd,nrpar,nrens,nt)      ! reads infile.in
   allocate( ens(0:neq-1,0:nt,nrens)   )       ! allocate ensemble for the model solutions
   allocate( enspar(1:nrpar+neq,nrens) )       ! allocate ensemble of model parameters
   call agegroups                              ! define agegroups and population numbers
   call Rmatrix                                ! define R infection rates between agegroups used in phase 3
   call pfactors                               ! define fraction of mild, severe, or fatal, for each agegroup
   if (lenkf) call enkfini(nrens,nt,time)      ! Initialize EnKF (E, D, and R) reading data from corona.dat
   call inipar(enspar,parstd,nrpar,nrens,neq)  ! initialize ensemble of parameters
   call iniens(ens,enspar,neq,nrens,nt,nrpar)  ! initialize ensemble of models

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   print '(a)','Computing prior ensemble prediction'
   do j=1,nrens
      call ens2mod(nrpar,nrens,neq,enspar,j)   ! copy ensemble member j to model parameters
      call pfactors                            ! copy member j of pfactors to model 
      call solve(ens,neq,nrens,nt,j)           ! solve ODEs for member j
   enddo
   call tecplot(ens,enspar,nt,nrens,neq,nrpar,0) ! Dump prior solution to files
   if (.not.lenkf) stop                        ! If not doing assimilation stop here


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! EnKF update
   call enkfprep(ens,enspar,nrpar,nrens,nt,neq) ! Compute S and D matrices
   call analysis(enspar, R, E, S, D, innovation, nrpar+neq, nrens, nrobs, .true., truncation, mode_analysis, &
                 lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)
   call enkfpost(ens,enspar,nrpar,nrens,nt,neq) ! Check parameters
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Posterior ensemble prediction
   print '(a)','Computing posterior ensemble prediction'
   do j=1,nrens
      call ens2mod(nrpar, nrens, neq, enspar , j)
      call pfactors 
      call solve(ens,neq,nrens,nt,j)
   enddo
   call tecplot(ens,enspar,nt,nrens,neq,nrpar,1)
   print '(a)','Done..'

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

   if (t <= Tinterv) then
      R=R0
   elseif (Tinterv < t .and. t <= Tinterv2 ) then
      R=Rt 
   elseif (t > Tinterv2) then
      R=Rmat
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
   
   Tdead  = T2death-Tinf 

! S_i
   do i=0,na-1
      ii=i
      ydot(ii) = 0.0
      do k=0,na-1
         kk=2*na+k
         ydot(ii) = ydot(ii) - (R(i,k)/Tinf)*y(kk)*y(ii)
      enddo
      !print '(a,i3,6g13.5)','S1',ii,y(ii),ydot(ii)
   enddo

! E_i
   do i=0,na-1
      ii=na+i       ! E
      ydot(ii) = - (1.0/Tinc)*y(ii)
      do k=0,na-1
         kk=2*na+k  ! I
         ydot(ii) = ydot(ii) + (R(i,k)/Tinf)*y(kk)*y(i)
      enddo
      !print '(a,i3,6g13.5)','E ',ii,y(ii),ydot(ii)
   enddo

! I_i
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

!   if ((t > Tinterv).and.(t < Tinterv + duration)) then
!      beta=Rt/Tinf
!   elseif (t > Tinterv + duration) then
!      beta=0.3/Tinf
!   else
!      beta=R0/Tinf
!   endif
!
!   p_fatal  = CFR
!   p_mild   = 1 - P_SEVERE - CFR
!   Tdead  = T2death-Tinf 
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

