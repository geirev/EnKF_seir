program seir
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
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
   integer  nrens                              ! Ensemble size (from infile.in)
   integer  nt                                 ! Number of output times (from infile.in)

   integer i,k,j,m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(states), allocatable :: ens(:,:)       ! Storage of the ensemble of solutions for printing
   type(params), allocatable :: enspar(:)      ! Ensemble of state variables ( parameters + initial conditions)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization 
   call readinputs(nrens,nt)                   ! Reads infile.in
   allocate( ens(0:nt,nrens)   )               ! Allocate ensemble for the model solutions
   allocate( enspar(nrens) )                   ! Allocate ensemble of model parameters
   call agegroups                              ! Define agegroups and population numbers
   call Rmatrix                                ! Define R infection rates between agegroups used in phase 3
   call pfactors                               ! Define fraction of mild, severe, or fatal, for each agegroup
   call enkfini(nrens,nt,time)                 ! Reading data from corona.dat
   call inienspar(enspar,nrens)   ! Initialize ensemble of parameters

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   print '(a)','Computing prior ensemble prediction'
   call iniens(ens,enspar,nrens,nt)                   ! Initialize ensemble of models
   do j=1,nrens
      call solve(ens,enspar,nrens,nt,j)               ! solve ODEs for member j
   enddo
   print '(a)','Dumping tecplot files.'
   call tecplot(ens,enspar,nt,nrens,0) ! Dump prior solution to files
   print *,'A'
   if (.not.lenkf) stop                        ! If not doing assimilation stop here

   print *,'B'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ESMDA update
   do i=1,nesmda
      print '(a,2I3)','ESMDA step:',i,nesmda
! Analysis step
      call enkfprep(ens,enspar,nrens,nt) ! Compute S and D matrices
      call analysis(enspar, R, E, S, D, innovation, nrpar, nrens, nrobs, .true., truncation, mode_analysis, &
                    lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)
      call enkfpost(ens,enspar,nrens,nt) ! Check parameters
  
! Posterior ensemble prediction
      call iniens(ens,enspar,nrens,nt)                ! Initialize ensemble of models from updated parameters
      do j=1,nrens
         call solve(ens,enspar,nrens,nt,j)
      enddo
   print '(a)','Done..'
   enddo

   print '(a)','Dumping tecplot files.'
   call tecplot(ens,enspar,nt,nrens,1)
   print '(a)','Done..'


end program

subroutine f(neq, t, y, ydot)
   use mod_params
   use mod_parameters
   use m_agegroups
   use m_Rmatrix
   use m_pfactors
   use m_random
   use mod_dimensions
   use mod_states
   implicit none
   integer i,k,ii,kk
   integer neq
   real t
   type(states) y
   type(states) ydot

   real R(na,na)

   if (t <= Tinterv(1)) then
      R=p%R(1)*Rmat(:,:,1)
   elseif (Tinterv(1) < t .and. t <= Tinterv(2) ) then
      R=p%R(2)*Rmat(:,:,2)
   elseif (t > Tinterv(2)) then
      R=p%R(3)*Rmat(:,:,3)
   endif
   

   ydot%S  =                          - (1.0/p%Tinf) * matmul(R,y%I) * y%S
   ydot%E  =  - (1.0/p%Tinc ) * y%E   + (1.0/p%Tinf) * matmul(R,y%I) * y%S
   ydot%I  =    (1.0/p%Tinc ) * y%E   - (1.0/p%Tinf) * y%I
   ydot%Qm =  - (1.0/p%Trecm) * y%Qm  + (1.0/p%Tinf) * dot_product(pm,y%I)
   ydot%Qs =  - (1.0/p%Thosp) * y%Qs  + (1.0/p%Tinf) * dot_product(ps,y%I)
   ydot%Qf =  - (1.0/p%Thosp) * y%Qf  + (1.0/p%Tinf) * dot_product(pf,y%I)
   ydot%Hs =    (1.0/p%Thosp) * y%Qs  - (1.0/p%Trecs) * y%Hs
   ydot%Hf =    (1.0/p%Thosp) * y%Qf  - (1.0/p%Tdead) * y%Hf
   ydot%Rm =    (1.0/p%Trecm) * y%Qm
   ydot%Rs =    (1.0/p%Trecs) * y%Hs
   ydot%D  =    (1.0/p%Tdead) * y%Hf

!   print *,'sum ydot and y (0.0 and 1.0)',sum(ydot),sum(y)

end subroutine
 
subroutine jac(neq, t, y, ml, mu, pd, nrowpd)
   use mod_params
   use mod_parameters
   use m_agegroups
   use m_Rmatrix
   use m_pfactors
   use m_random
   use mod_dimensions
   use mod_states
   implicit none
   integer neq, ml, mu, nrowpd
   type(states) y
   real pd(1,1)
   real t

end subroutine
