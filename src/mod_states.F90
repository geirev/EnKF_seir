module mod_states
   use mod_dimensions
   use mod_substate
   private
   public :: states, operator(+), operator(-), operator(*), assignment(=), sum, sqrt

! A full model state is an array of groups/countries/regions represented by a substate
   type states
      type(substate) group(nc)
   end type states
!  type(states) fullstate
!  type(states) ensemble(nrens)
!  fullstate%group(i)
!  ensemble(j)%group(i)%S(k)

! Overloaded and generic operators
   interface operator(+)
      module procedure add_states
   end interface

   interface operator(-)
      module procedure subtract_states
   end interface

   interface operator(*)
      module procedure states_real_mult,&
                       real_states_mult,&
                       states_states_mult
   end interface

   interface assignment(=)
      module procedure assign_states
   end interface

   interface sqrt
      module procedure sqrt_states
   end interface

   interface sum
      module procedure sum_states
   end interface


contains

  real function sum_states(A)
      implicit none
      integer i
      type(states), intent(in) :: A
      sum_states=0.0
      do i=1,nc
         sum_states = sum_states + sum(A%group(i))
      enddo
   end function sum_states

   type(states) function sqrt_states(A)
      implicit none
      type(states), intent(in) :: A
      integer i
      do i=1,nc
         sqrt_states%group(i)   = sqrt(A%group(i))
      enddo
   end function sqrt_states

   type(states) function add_states(A,B)
      implicit none
      integer i
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      do i=1,nc
         add_states%group(i) = A%group(i)  + B%group(i) 
      enddo
         
   end function add_states

   type(states) function subtract_states(A,B)
      implicit none
      integer i
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      do i=1,nc
         subtract_states%group(i) = A%group(i)  - B%group(i) 
      enddo
   end function subtract_states

   type(states) function states_real_mult(A,B)
      implicit none
      integer i
      type(states), intent(in) :: A
      real, intent(in) :: B
      do i=1,nc
         states_real_mult%group(i)       = B*A%group(i) 
      enddo
   end function states_real_mult

   type(states) function real_states_mult(B,A)
      implicit none
      integer i
      type(states), intent(in) :: A
      real, intent(in) :: B
      do i=1,nc
         real_states_mult%group(i)       = B*A%group(i) 
      enddo
   end function real_states_mult

   type(states) function states_states_mult(A,B)
      implicit none
      integer i
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      do i=1,nc
         states_states_mult%group(i)       = A%group(i)  * B%group(i) 
      enddo
   end function states_states_mult


   subroutine assign_states(A,r)
      implicit none
      integer i
      type(states), intent(out) :: A
      real, intent(in) :: r
      do i=1,nc
         A%group(i)       = r
      enddo
   end subroutine assign_states


end module mod_states

