module m_solve
contains
subroutine solve(ens,enspar,nrens,nt,j)
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_pfactors
   implicit none
   external f,jac
   integer, intent(in) :: nrens
   integer, intent(in) :: nt
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
   integer :: neq=3*na+8

   real,    allocatable  :: rwork(:) 
   integer, allocatable  :: iwork(:)  

   dt= time/real(nt-1)

   lrw =max(20+16*neq, 22+9*neq+neq**2)
   liw = 20 + neq
   allocate(rwork(lrw))
   allocate(iwork(liw))

   p=enspar(j)
   y=ens(0,j) 
   call pfactors 

   istate=1
   do i=1,nt 
      t=0+real(i)*dt
      tout=t+dt
      call slsode(f,neq,y,t,tout,itol,rtol,atol,itask,istate,iopt,rwork,lrw,iwork,liw,jac,mf)
      ens(i,j)=y
!        print '(a,4f12.2)','SUM=',sum(y)
      if (istate < 0) then
         print '(a,i3,a,i4)','negative istate, exiting: ',istate,',  it=0,  iens=',j
         stop
      endif
   enddo

   deallocate(rwork)
   deallocate(iwork)
end subroutine
end module
