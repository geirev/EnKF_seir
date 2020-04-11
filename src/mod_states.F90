module mod_states
! Modelstate definition for ECLIPSE
   use mod_dimensions

   type states
! Solution data
      real S(na)        ! Susceptible age groups
      real E(na)        ! Exposed age groups
      real I(na)        ! Infectious age groups
      real Qm           ! Quarantened mild sickness
      real Qs           ! Quarantened severe sickness (will be hospitalized)
      real Qf           ! Quarantened fatal sickness (will die)
      real Hs           ! Hospitalized with severe sickness
!      real Hf           ! Hospitalized with fatal sickness
      real Rm           ! Recovered from mild sickness
      real Rs           ! Recovered from severe sickness (from hospital)
      real D            ! Dead
   end type states


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


contains

   function sqrt_states(A)
      type(states) sqrt_states
      type(states), intent(in) :: A
      real :: eps=0.1E-14
      sqrt_states%S       = sqrt(A%S+eps)
      sqrt_states%E       = sqrt(A%E+eps)
      sqrt_states%I       = sqrt(A%I+eps)
      sqrt_states%Qm      = sqrt(A%Qm+eps)
      sqrt_states%Qs      = sqrt(A%Qs+eps)
      sqrt_states%Qf      = sqrt(A%Qf+eps)
      sqrt_states%Hs      = sqrt(A%Hs+eps)
!      sqrt_states%Hf      = sqrt(A%Hf+eps)
      sqrt_states%Rm      = sqrt(A%Rm+eps)
      sqrt_states%Rs      = sqrt(A%Rs+eps)
      sqrt_states%D       = sqrt(A%D+eps)
   end function sqrt_states

   function add_states(A,B)
      type(states) add_states
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      add_states%S       = A%S  + B%S 
      add_states%E       = A%E  + B%E
      add_states%I       = A%I  + B%I
      add_states%Qm      = A%Qm + B%Qm
      add_states%Qs      = A%Qs + B%Qs
      add_states%Qf      = A%Qf + B%Qf
      add_states%Hs      = A%Hs + B%Hs
!      add_states%Hf      = A%Hf + B%Hf
      add_states%Rm      = A%Rm + B%Rm
      add_states%Rs      = A%Rs + B%Rs
      add_states%D       = A%D  + B%D
   end function add_states

   function subtract_states(A,B)
      type(states) subtract_states
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      subtract_states%S       = A%S  - B%S 
      subtract_states%E       = A%E  - B%E
      subtract_states%I       = A%I  - B%I
      subtract_states%Qm      = A%Qm - B%Qm
      subtract_states%Qs      = A%Qs - B%Qs
      subtract_states%Qf      = A%Qf - B%Qf
      subtract_states%Hs      = A%Hs - B%Hs
!      subtract_states%Hf      = A%Hf - B%Hf
      subtract_states%Rm      = A%Rm - B%Rm
      subtract_states%Rs      = A%Rs - B%Rs
      subtract_states%D       = A%D  - B%D
   end function subtract_states

   function states_real_mult(A,B)
      type(states) states_real_mult
      type(states), intent(in) :: A
      real, intent(in) :: B
      states_real_mult%S       = B*A%S 
      states_real_mult%E       = B*A%E 
      states_real_mult%I       = B*A%I 
      states_real_mult%Qm      = B*A%Qm
      states_real_mult%Qs      = B*A%Qs
      states_real_mult%Qf      = B*A%Qf
      states_real_mult%Hs      = B*A%Hs
!      states_real_mult%Hf      = B*A%Hf
      states_real_mult%Rm      = B*A%Rm
      states_real_mult%Rs      = B*A%Rs
      states_real_mult%D       = B*A%D 
   end function states_real_mult

   function real_states_mult(B,A)
      type(states) real_states_mult
      type(states), intent(in) :: A
      real, intent(in) :: B
      real_states_mult%S       = B*A%S 
      real_states_mult%E       = B*A%E 
      real_states_mult%I       = B*A%I 
      real_states_mult%Qm      = B*A%Qm
      real_states_mult%Qs      = B*A%Qs
      real_states_mult%Qf      = B*A%Qf
      real_states_mult%Hs      = B*A%Hs
!      real_states_mult%Hf      = B*A%Hf
      real_states_mult%Rm      = B*A%Rm
      real_states_mult%Rs      = B*A%Rs
      real_states_mult%D       = B*A%D 
   end function real_states_mult

   function states_states_mult(A,B)
      type(states) states_states_mult
      type(states), intent(in) :: A
      type(states), intent(in) :: B
      states_states_mult%S       = A%S  * B%S 
      states_states_mult%E       = A%E  * B%E 
      states_states_mult%I       = A%I  * B%I 
      states_states_mult%Qm      = A%Qm * B%Qm
      states_states_mult%Qs      = A%Qs * B%Qs
      states_states_mult%Qf      = A%Qf * B%Qf
      states_states_mult%Hs      = A%Hs * B%Hs
!      states_states_mult%Hf      = A%Hf * B%Hf
      states_states_mult%Rm      = A%Rm * B%Rm
      states_states_mult%Rs      = A%Rs * B%Rs
      states_states_mult%D       = A%D  * B%D 
   end function states_states_mult


   subroutine assign_states(A,r)
      type(states), intent(out) :: A
      real, intent(in) :: r
      A%S       = r
      A%E       = r
      A%I       = r
      A%Qm      = r
      A%Qs      = r
      A%Qf      = r
      A%Hs      = r
!      A%Hf      = r
      A%Rm      = r
      A%Rs      = r
      A%D       = r
   end subroutine assign_states


end module mod_states

