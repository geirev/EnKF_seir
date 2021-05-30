module mod_substate
! Model state definition for a country or group.
   use mod_dimensions
   private
   public :: substate, operator(+), operator(-), operator(*), assignment(=), sum, sqrt

   type substate
! Solution data
      real V(na)        ! Vaccinated age groups
      real S(na)        ! Susceptible age groups
      real E(na)        ! Exposed age groups
      real I(na)        ! Infectious age groups
      real Qm           ! Quarantened mild sickness
      real Qs           ! Quarantened severe sickness (will be hospitalized)
      real Qf           ! Quarantened fatal sickness (will die)
      real Hs           ! Hospitalized with severe sickness
      real Hf           ! Hospitalized with fatal sickness
      real C            ! In Care home with fatal sickness
      real Rm           ! Recovered from mild sickness
      real Rs           ! Recovered from severe sickness (from hospital)
      real D            ! Dead
   end type substate

! Overloaded and generic operators
   interface operator(+)
      module procedure add_substate
   end interface

   interface operator(-)
      module procedure subtract_substate
   end interface

   interface operator(*)
      module procedure substate_real_mult,&
                       real_substate_mult,&
                       substate_substate_mult
   end interface

   interface assignment(=)
      module procedure assign_substate
   end interface

   interface sqrt
      module procedure sqrt_substate
   end interface

   interface sum
      module procedure sum_substate
   end interface


contains

   function sum_substate(A)
      real sum_substate
      type(substate), intent(in) :: A
      sum_substate =&
                   sum(A%V) &
                 + sum(A%S) &
                 + sum(A%E) &
                 + sum(A%I) &
                 + A%Qm     &
                 + A%Qs     &
                 + A%Qf     &
                 + A%Hs     &
                 + A%Hf     &
                 + A%C      &
                 + A%Rm     &
                 + A%Rs     &
                 + A%D 
   end function sum_substate

   function sqrt_substate(A)
      type(substate) sqrt_substate
      type(substate), intent(in) :: A
      real :: eps=0.1E-14
      sqrt_substate%V       = sqrt(A%V+eps)
      sqrt_substate%S       = sqrt(A%S+eps)
      sqrt_substate%E       = sqrt(A%E+eps)
      sqrt_substate%I       = sqrt(A%I+eps)
      sqrt_substate%Qm      = sqrt(A%Qm+eps)
      sqrt_substate%Qs      = sqrt(A%Qs+eps)
      sqrt_substate%Qf      = sqrt(A%Qf+eps)
      sqrt_substate%Hs      = sqrt(A%Hs+eps)
      sqrt_substate%Hf      = sqrt(A%Hf+eps)
      sqrt_substate%C       = sqrt(A%C+eps)
      sqrt_substate%Rm      = sqrt(A%Rm+eps)
      sqrt_substate%Rs      = sqrt(A%Rs+eps)
      sqrt_substate%D       = sqrt(A%D+eps)
   end function sqrt_substate

   function add_substate(A,B)
      type(substate) add_substate
      type(substate), intent(in) :: A
      type(substate), intent(in) :: B
      add_substate%V       = A%V  + B%V 
      add_substate%S       = A%S  + B%S 
      add_substate%E       = A%E  + B%E
      add_substate%I       = A%I  + B%I
      add_substate%Qm      = A%Qm + B%Qm
      add_substate%Qs      = A%Qs + B%Qs
      add_substate%Qf      = A%Qf + B%Qf
      add_substate%Hs      = A%Hs + B%Hs
      add_substate%Hf      = A%Hf + B%Hf
      add_substate%C       = A%C  + B%C 
      add_substate%Rm      = A%Rm + B%Rm
      add_substate%Rs      = A%Rs + B%Rs
      add_substate%D       = A%D  + B%D
   end function add_substate

   function subtract_substate(A,B)
      type(substate) subtract_substate
      type(substate), intent(in) :: A
      type(substate), intent(in) :: B
      subtract_substate%V       = A%V  - B%V 
      subtract_substate%S       = A%S  - B%S 
      subtract_substate%E       = A%E  - B%E
      subtract_substate%I       = A%I  - B%I
      subtract_substate%Qm      = A%Qm - B%Qm
      subtract_substate%Qs      = A%Qs - B%Qs
      subtract_substate%Qf      = A%Qf - B%Qf
      subtract_substate%Hs      = A%Hs - B%Hs
      subtract_substate%Hf      = A%Hf - B%Hf
      subtract_substate%C       = A%C  - B%C 
      subtract_substate%Rm      = A%Rm - B%Rm
      subtract_substate%Rs      = A%Rs - B%Rs
      subtract_substate%D       = A%D  - B%D
   end function subtract_substate

   function substate_real_mult(A,B)
      type(substate) substate_real_mult
      type(substate), intent(in) :: A
      real, intent(in) :: B
      substate_real_mult%V       = B*A%V 
      substate_real_mult%S       = B*A%S 
      substate_real_mult%E       = B*A%E 
      substate_real_mult%I       = B*A%I 
      substate_real_mult%Qm      = B*A%Qm
      substate_real_mult%Qs      = B*A%Qs
      substate_real_mult%Qf      = B*A%Qf
      substate_real_mult%Hs      = B*A%Hs
      substate_real_mult%Hf      = B*A%Hf
      substate_real_mult%C       = B*A%C
      substate_real_mult%Rm      = B*A%Rm
      substate_real_mult%Rs      = B*A%Rs
      substate_real_mult%D       = B*A%D 
   end function substate_real_mult

   function real_substate_mult(B,A)
      type(substate) real_substate_mult
      type(substate), intent(in) :: A
      real, intent(in) :: B
      real_substate_mult%V       = B*A%V 
      real_substate_mult%S       = B*A%S 
      real_substate_mult%E       = B*A%E 
      real_substate_mult%I       = B*A%I 
      real_substate_mult%Qm      = B*A%Qm
      real_substate_mult%Qs      = B*A%Qs
      real_substate_mult%Qf      = B*A%Qf
      real_substate_mult%Hs      = B*A%Hs
      real_substate_mult%Hf      = B*A%Hf
      real_substate_mult%C       = B*A%C
      real_substate_mult%Rm      = B*A%Rm
      real_substate_mult%Rs      = B*A%Rs
      real_substate_mult%D       = B*A%D 
   end function real_substate_mult

   function substate_substate_mult(A,B)
      type(substate) substate_substate_mult
      type(substate), intent(in) :: A
      type(substate), intent(in) :: B
      substate_substate_mult%V       = A%V  * B%V 
      substate_substate_mult%S       = A%S  * B%S 
      substate_substate_mult%E       = A%E  * B%E 
      substate_substate_mult%I       = A%I  * B%I 
      substate_substate_mult%Qm      = A%Qm * B%Qm
      substate_substate_mult%Qs      = A%Qs * B%Qs
      substate_substate_mult%Qf      = A%Qf * B%Qf
      substate_substate_mult%Hs      = A%Hs * B%Hs
      substate_substate_mult%Hf      = A%Hf * B%Hf
      substate_substate_mult%C       = A%C  * B%C
      substate_substate_mult%Rm      = A%Rm * B%Rm
      substate_substate_mult%Rs      = A%Rs * B%Rs
      substate_substate_mult%D       = A%D  * B%D 
   end function substate_substate_mult


   subroutine assign_substate(A,r)
      type(substate), intent(out) :: A
      real, intent(in) :: r
      A%V       = r
      A%S       = r
      A%E       = r
      A%I       = r
      A%Qm      = r
      A%Qs      = r
      A%Qf      = r
      A%Hs      = r
      A%Hf      = r
      A%C       = r
      A%Rm      = r
      A%Rs      = r
      A%D       = r
   end subroutine assign_substate


end module mod_substate

