program seir
   use mod_dimensions
   use mod_states
   use mod_parameters
   use m_ens2mod
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
   use m_inienspar
   use m_iniens

   implicit none
   integer, parameter :: nrpar=12              ! Number of uncertain model parameters
   integer  nrens                              ! Ensemble size (from infile.in)
   integer  nt                                 ! Number of output times (from infile.in)

   integer i,k,j,m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(states), allocatable :: ens(:,:)       ! Storage of the ensemble of solutions for printing
   real, allocatable :: enspar(:,:)            ! Ensemble of state variables ( parameters + initial conditions)
   real parstd(nrpar)                          ! Standard deviations of parameters

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization 
   call readinputs(parstd,nrpar,nrens,nt)      ! Reads infile.in
   allocate( ens(0:nt,nrens)   )               ! Allocate ensemble for the model solutions
   allocate( enspar(1:nrpar,nrens) )       ! Allocate ensemble of model parameters
   call agegroups                              ! Define agegroups and population numbers
   call Rmatrix                                ! Define R infection rates between agegroups used in phase 3
   call pfactors                               ! Define fraction of mild, severe, or fatal, for each agegroup
   call enkfini(nrens,nt,time)                 ! Reading data from corona.dat
   call inienspar(enspar,parstd,nrpar,nrens)   ! Initialize ensemble of parameters

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   print '(a)','Computing prior ensemble prediction'
   call iniens(ens,enspar,nrens,nt,nrpar)      ! Initialize ensemble of models
   do j=1,nrens
      call ens2mod(nrpar,nrens,enspar,j)   ! copy ensemble member j to model parameters
      call pfactors                            ! copy member j of pfactors to model 
      call solve(ens,nrens,nt,j)               ! solve ODEs for member j
   enddo
   print '(a)','Dumping tecplot files.'
   call tecplot(ens,enspar,nt,nrens,nrpar,0) ! Dump prior solution to files
   if (.not.lenkf) stop                        ! If not doing assimilation stop here


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ESMDA update
   do i=1,nesmda
      print '(a,2I3)','ESMDA step:',i,nesmda
! Analysis step
      call enkfprep(ens,enspar,nrpar,nrens,nt) ! Compute S and D matrices
      call analysis(enspar, R, E, S, D, innovation, nrpar, nrens, nrobs, .true., truncation, mode_analysis, &
                    lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)
      call enkfpost(ens,enspar,nrpar,nrens,nt) ! Check parameters
  
! Posterior ensemble prediction
      call iniens(ens,enspar,nrens,nt,nrpar)  ! Initialize ensemble of models from updated parameters
      do j=1,nrens
         call ens2mod(nrpar, nrens, enspar , j)
         call pfactors 
         call solve(ens,nrens,nt,j)
      enddo
   print '(a)','Done..'
   enddo

   print '(a)','Dumping tecplot files.'
   call tecplot(ens,enspar,nt,nrens,nrpar,1)
   print '(a)','Done..'


end program

subroutine f(neqq, t, y, ydot)
   use mod_parameters
   use m_agegroups
   use m_Rmatrix
   use m_pfactors
   use m_random
   use mod_dimensions
   use mod_states
   implicit none
   integer i,k,ii,kk
   integer neqq
   real t
   type(states) y
   type(states) ydot

   real R(0:na-1,0:na-1)

   if (t <= Tinterv) then
      R=R0
   elseif (Tinterv < t .and. t <= Tinterv2 ) then
      R=Rt 
   elseif (t > Tinterv2) then
      R=Rmat
   endif

   
   ydot=0.0

   ydot%S  =                        - (1.0/Tinf) * matmul(R,y%I) * y%S
   ydot%E  =  - (1.0/Tinc ) * y%E   + (1.0/Tinf) * matmul(R,y%I) * y%S
   ydot%I  =    (1.0/Tinc ) * y%E   - (1.0/Tinf) * y%I
   ydot%Qm =  - (1.0/Trecm) * y%Qm  + (1.0/Tinf) * dot_product(pm,y%I)
   ydot%Qs =  - (1.0/Thosp) * y%Qs  + (1.0/Tinf) * dot_product(ps,y%I)
   ydot%Qf =  - (1.0/Thosp) * y%Qf  + (1.0/Tinf) * dot_product(pf,y%I)
   ydot%Hs =    (1.0/Thosp) * y%Qs  - (1.0/Trecs) * y%Hs
   ydot%Hf =    (1.0/Thosp) * y%Qf  - (1.0/Tdead) * y%Hf
   ydot%Rm =    (1.0/Trecm) * y%Qm
   ydot%Rs =    (1.0/Trecs) * y%Hs
   ydot%D  =    (1.0/Tdead) * y%Hf

!   print *,'sum ydot and y (0.0 and 1.0)',sum(ydot),sum(y)

end subroutine
 
subroutine jac(neq, t, y, ml, mu, pd, nrowpd)
   use mod_parameters
   implicit none
   integer neq, ml, mu, nrowpd
   real t, y(0:neq-1), pd(0:neq-1,0:neq-1)

end subroutine
