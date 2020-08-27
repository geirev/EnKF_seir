module m_enkfpost
contains 
subroutine enkfpost(ens,enspar)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   implicit none
   type(states), intent(inout) :: ens(0:nt,nrens)
   type(params), intent(inout) :: enspar(nrens)
   type(params) avepar,nordif,parvar,chisq
   real chisqsum
   integer j,ic

! Ensure all parameters are larger than minpar (from infile.in)
   avepar=0.0
   do j=1,nrens
      enspar(j)=max(enspar(j),minpar)
      avepar=avepar+enspar(j)*(1.0/real(nrens))
   enddo

   print '(a)','Posterior ensemble mean of parameters including chisquare value:'
   print '(100a10)','group ',parnames
   do ic=1,nc
      print '(i8,100f10.4)',ic,    pfg%E0(ic),    pfg%I0(ic),    pfg%Tinf,     pfg%Tinc,       pfg%Trecm,   &
                                   pfg%Trecs,     pfg%Thosp,     pfg%Tdead,    pfg%sev(ic),    pfg%CFR(ic) 
      print '(i8,100f10.4)',ic, avepar%E0(ic), avepar%I0(ic), avepar%Tinf,  avepar%Tinc,    avepar%Trecm,   &
                                avepar%Trecs,  avepar%Thosp,  avepar%Tdead, avepar%sev(ic), avepar%CFR(ic)
   enddo

!   nordif = pfg - avepar
!   nordif = nordif * nordif
!   parvar = parstd * parstd
!   chisq%E0    = nordif%E0/parvar%E0
!   chisq%I0    = nordif%I0/parvar%I0
!   chisq%Tinf  = nordif%Tinf/parvar%Tinf
!   chisq%Tinc  = nordif%Tinc/parvar%Tinc
!   chisq%Trecm = nordif%Trecm/parvar%Trecm
!   chisq%Trecs = nordif%Trecs/parvar%Trecs
!   chisq%Thosp = nordif%Thosp/parvar%Thosp
!   chisq%Tdead = nordif%Tdead/parvar%Tdead
!   chisq%sev   = nordif%sev/parvar%sev
!   chisq%CFR   = nordif%CFR/parvar%CFR
     
!   print '(100f10.4)', chisq%E0, &
!                       chisq%I0, &
!                       chisq%Tinf, &
!                       chisq%Tinc, &
!                       chisq%Trecm, &
!                       chisq%Trecs, &
!                       chisq%Thosp, &
!                       chisq%Tdead, &
!                       chisq%sev, &
!                       chisq%CFR
!   chisqsum = chisq%E0 + chisq%I0 + chisq%Tinf + chisq%Tinc + chisq%Trecm + &
!              chisq%Trecs + chisq%Thosp + chisq%Tdead + chisq%sev + chisq%CFR
!   write(*,'(a,f10.4)') 'chisq summed over all parameters: ',chisqsum      
!   print *
end subroutine
end module
