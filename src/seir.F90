program seir
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_readinfile
   use m_Rprior
   use m_readagegroups
   use m_readinicond
   use m_pfactors
   use m_Rmatrix
   use m_random
   use m_set_random_seed2
   use m_enkfini
   use m_inienspar
   use m_iniens
   use m_solve
   use m_tecplot
   use m_enkfprep
   use m_enkfpost
   use m_chisquared

   implicit none
   integer i,ic,k,j,m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   type(states), allocatable :: ens(:,:)       ! Storage of the ensemble of solutions for printing
   type(params), allocatable :: enspar(:)      ! Ensemble of state variables ( parameters + initial conditions)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization 
   call set_random_seed2
   call readinfile()                           ! Reads infile.in
   call Rprior()                               ! Sets the prior for R
   call readagegroups()                        ! Reads agegroups and population numbers from agegroupsxxx.in or populationxxx.in
   call readinicond()                          ! Reads initial conditions from inicondxxx.in
   pfg=p          ! store first guess of parameters
   call pfactors()                             ! Define fraction of mild, severe, or fatal, for each agegroup
   call Rmatrix()                              ! Define R infection rates between agegroups
   call enkfini(time)                          ! Reading data from corona.dat
   allocate( ens(0:nt,nrens) )                 ! Allocate ensemble for the model solutions
   allocate( enspar(nrens) )                   ! Allocate ensemble of model parameters
   call inienspar(enspar)                      ! Initialize ensemble of parameters
   call iniens(ens,enspar)                     ! Initialize ensemble of models

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Prior ensemble prediction
   print '(a)','Computing prior ensemble prediction'
   do j=1,nrens
      call solve(ens,enspar,j)                 ! solve ODEs for member j
   enddo
   print '(a)','Dumping tecplot files.'
   call tecplot(ens,enspar,0)                  ! Dump prior solution to files
   if (.not.lenkf) stop                        ! If not doing assimilation stop here


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ESMDA update
! Prepare for first analysis computation
   call enkfprep(ens,enspar)

   do i=1,nesmda
      print '(a)','--------------------------------------------------------------------------------'
      print '(a,2I3)','ESMDA step:',i,nesmda

! Analysis step
      nrpar=sizeof(enspar(1))/8
      call analysis(enspar, R, E, S, D, innovation, nrpar, nrens, nrobs, .true., truncation, mode_analysis, &
                    lrandrot, lupdate_randrot, lsymsqrt, inflate, infmult, ne)

! Check and print updated parameters
      call enkfpost(ens,enspar)
 
! Posterior ensemble prediction
      call iniens(ens,enspar)                  ! Initialize ensemble of models from updated parameters
      do j=1,nrens
         call solve(ens,enspar,j)
      enddo

! Prepare for next analysis computation
      call enkfprep(ens,enspar)

      call chisquared()

   enddo

   call tecplot(ens,enspar,1)

   lpost_pfactors=.true.
   do ic=1,nc
      p%sev(ic)=sum(enspar(1:nrens)%sev(ic))/real(nrens)
      p%cfr(ic)=sum(enspar(1:nrens)%cfr(ic))/real(nrens)
   enddo
   call pfactors 

end program

subroutine f(neqq, t, y, ydot)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_readagegroups
   use m_Rmatrix
   use m_pfactors
   use m_random
   use m_readinfile
   implicit none
   integer neqq
   real t
   type(states) y
   type(states) ydot

   real R(na,na)
   real RCI(nc,nc)
   real dt
   integer i,ic,jc,ir


   dt= time/real(nt-1)
   i= min(int(t/dt), rdim)

   if (t < Tinterv(1)) then
      ir=1
   elseif (Tinterv(1) <= t .and. t < Tinterv(2) ) then
      ir=2
   elseif (t >= Tinterv(2)) then
      ir=3
   endif
!   print '(a,2f8.2,i5,f10.2,i2)','t: ',t,dt,i,p%R(i,ic),ir


   do ic=1,nc
      RC(ic,ic,ir)=1.0 ! Diagonal is always one for contry-country interaction
      R(:,:)= p%R(i,ic) * Rmat(:,:,ir,ic)  

      ydot%group(ic)%S  = 0.0 !- qminf*(1.0/p%Tinf) * p%R(ir)*y%group(ic)%Qm * y%group(ic)%S
      do jc=1,nc
         ydot%group(ic)%S  = ydot%group(ic)%S  &
                           - (1.0/p%Tinf) * (Ntot(jc)/Ntot(ic)) * RC(ic,jc,ir) * matmul(R,y%group(jc)%I) * y%group(ic)%S
      enddo

      ydot%group(ic)%E  =  - (1.0/p%Tinc ) * y%group(ic)%E  !+ qminf*(1.0/p%Tinf) * p%R(ir)*y%group(ic)%Qm * y%group(ic)%S
      do jc=1,nc
         ydot%group(ic)%E  = ydot%group(ic)%E  &
                           + (1.0/p%Tinf) * (Ntot(jc)/Ntot(ic)) * RC(ic,jc,ir) * matmul(R,y%group(jc)%I) * y%group(ic)%S
      enddo
      ydot%group(ic)%I  =    (1.0/p%Tinc ) * y%group(ic)%E   - (1.0/p%Tinf) * y%group(ic)%I
      ydot%group(ic)%Qm =  - (1.0/p%Trecm) * y%group(ic)%Qm  + (1.0/p%Tinf) * dot_product(pm(:,ic),y%group(ic)%I)
      ydot%group(ic)%Qs =  - (1.0/p%Thosp) * y%group(ic)%Qs  + (1.0/p%Tinf) * dot_product(ps(:,ic),y%group(ic)%I)
      ydot%group(ic)%Qf =  - (1.0/p%Thosp) * y%group(ic)%Qf  + (1.0/p%Tinf) * dot_product(pf(:,ic),y%group(ic)%I)
      ydot%group(ic)%Hs =    (1.0/p%Thosp) * y%group(ic)%Qs  - (1.0/p%Trecs) * y%group(ic)%Hs
      ydot%group(ic)%Hf =    (hos/p%Thosp) * y%group(ic)%Qf  - (1.0/p%Tdead) * y%group(ic)%Hf
      ydot%group(ic)%C  =    ((1.0-hos)/p%Thosp) * y%group(ic)%Qf - (1.0/p%Tdead) * y%group(ic)%C
      ydot%group(ic)%Rm =    (1.0/p%Trecm) * y%group(ic)%Qm
      ydot%group(ic)%Rs =    (1.0/p%Trecs) * y%group(ic)%Hs
      ydot%group(ic)%D  =    (1.0/p%Tdead) * y%group(ic)%Hf  + (1.0/p%Tdead) * y%group(ic)%C
   enddo

!  print *,'sum ydot and y (0.0 and 1.0)',sum(ydot),sum(y)/real(nc)

end subroutine
 
subroutine jac(neqq, t, y, ml, mu, pd, nrowpd)
   use mod_dimensions
   use mod_states
   implicit none
   integer neqq, ml, mu, nrowpd
   type(states) y
   real pd(1,1)
   real t

end subroutine
