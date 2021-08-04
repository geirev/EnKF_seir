module m_solve
contains
subroutine solve(ens,enspar,j)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_pfactors
   use m_readvariantcond
   use m_readvariant
   use m_readvaccines
   use m_readvaccov
   implicit none
   external f,jac
   integer, intent(in) :: j
   type(states), intent(inout) :: ens(0:nt,nrens)
   type(params), intent(inout) :: enspar(nrens)
   type(states) y

   real t,dt,tout

   integer :: itol=1
   real    :: rtol=1.0E-5
   real    :: atol=1.0E-7
   integer :: itask=1
   integer :: istate=1
   integer :: iopt=0

   integer :: mf=10
   integer lrw,liw
   integer i
   integer ic

   real,    allocatable  :: rwork(:) 
   integer, allocatable  :: iwork(:)  
   integer neq
   real fac_s
   real fac_f
   real g
   integer a
   neq=sizeof(y)/8

   dt= time/real(nt-1)

   lrw =max(20+16*neq, 22+9*neq+neq**2)
   liw = 20 + neq
   allocate(rwork(lrw))
   allocate(iwork(liw))

   p=enspar(j)
   y=ens(0,j) 
!   print *,'sum y (1.0)',sum(y)
   call pfactors

   istate=1
   do i=1,nt 
      t=0+real(i-1)*dt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Time dependence (linear decline) of CFR
      do ic=1,nc

         a = minloc(vaccine(:,ic)%start_day, DIM=1)
         if (variant(ic)%start_day <= t) then
            g = (vaccov(ic)%coef)*(10**(-7.0))*(t - vaccine(a,ic)%start_day)**(vaccov(ic)%power)
            fac_s = min(g,varcond(ic)%vaccinated) * varcond(ic)%V_qs + (1.0 - min(g,varcond(ic)%vaccinated))*1.0
            fac_f = min(g,varcond(ic)%vaccinated) * varcond(ic)%V_qf + (1.0 - min(g,varcond(ic)%vaccinated))*1.0
         else
            fac_s = 1.0
            fac_f = 1.0
         endif
!         print *, fac_s, fac_f 
         p%sev(ic)=fac_s*enspar(j)%sev(ic)
         p%CFR(ic)=fac_f*enspar(j)%CFR(ic)
!         p%sev(ic)=max(enspar(j)%sev(ic)*(1.0-t/2000.0),0.0001)
!         p%CFR(ic)=max(enspar(j)%CFR(ic)*(1.0-t/2000.0),0.0001)
!         if (j == 1 .and. mod(i,10) == 0)  print *,'cfr: ',t,p%CFR(ic)

        
      enddo
      call pfactors
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      tout=t+dt
      call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
      ens(i,j)=y
      if (istate < 0) then
         print '(a,i3,a,i4)','negative istate, exiting: ',istate,',  it=0,  iens=',j
         stop
      endif
   enddo

   deallocate(rwork)
   deallocate(iwork)
end subroutine
end module
