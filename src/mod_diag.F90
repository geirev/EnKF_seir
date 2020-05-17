module mod_diag
! Diagnostic variables
   type diag
      real S        ! Susceptible summed over age groups
      real E        ! Exposed summed over age groups
      real I        ! Infectious summed age groups
      real H        ! Hs + Hf 
      real C        ! Total number of cases
      real R        ! Total Recovered
      real D        ! Deaths
      real A        ! Active cases 
   end type diag

! Overloaded and generic operators
   interface operator(+)
      module procedure add_diag
   end interface

   interface operator(-)
      module procedure subtract_diag
   end interface

   interface operator(*)
      module procedure diag_real_mult,&
                       real_diag_mult,&
                       diag_diag_mult
   end interface

   interface assignment(=)
      module procedure assign_diag
   end interface

   interface sqrt
      module procedure sqrt_diag
   end interface

contains

   function sqrt_diag(A)
      type(diag) sqrt_diag
      type(diag), intent(in) :: A
      real eps
      real x
      eps=tiny(x)
      sqrt_diag%S       = sqrt(A%S+eps)
      sqrt_diag%E       = sqrt(A%E+eps)
      sqrt_diag%I       = sqrt(A%I+eps)
      sqrt_diag%H       = sqrt(A%H+eps)
      sqrt_diag%R       = sqrt(A%R+eps)
      sqrt_diag%C       = sqrt(A%C+eps)
      sqrt_diag%D       = sqrt(A%D+eps)
      sqrt_diag%A       = sqrt(A%A+eps)
   end function sqrt_diag

   function add_diag(A,B)
      type(diag) add_diag
      type(diag), intent(in) :: A
      type(diag), intent(in) :: B
      add_diag%S       = A%S  + B%S 
      add_diag%E       = A%E  + B%E
      add_diag%I       = A%I  + B%I
      add_diag%H       = A%H + B%H
      add_diag%R       = A%R + B%R
      add_diag%C       = A%C  + B%C
      add_diag%D       = A%D  + B%D
      add_diag%A       = A%A  + B%A
   end function add_diag

   function subtract_diag(A,B)
      type(diag) subtract_diag
      type(diag), intent(in) :: A
      type(diag), intent(in) :: B
      subtract_diag%S       = A%S  - B%S 
      subtract_diag%E       = A%E  - B%E
      subtract_diag%I       = A%I  - B%I
      subtract_diag%H       = A%H  - B%H
      subtract_diag%R       = A%R  - B%R
      subtract_diag%C       = A%C  - B%C
      subtract_diag%D       = A%D  - B%D
      subtract_diag%A       = A%A  - B%A
   end function subtract_diag

   function diag_real_mult(A,B)
      type(diag) diag_real_mult
      type(diag), intent(in) :: A
      real, intent(in) :: B
      diag_real_mult%S       = B*A%S 
      diag_real_mult%E       = B*A%E 
      diag_real_mult%I       = B*A%I 
      diag_real_mult%H       = B*A%H
      diag_real_mult%R       = B*A%R
      diag_real_mult%C       = B*A%C 
      diag_real_mult%D       = B*A%D 
      diag_real_mult%A       = B*A%A 
   end function diag_real_mult

   function real_diag_mult(B,A)
      type(diag) real_diag_mult
      type(diag), intent(in) :: A
      real, intent(in) :: B
      real_diag_mult%S       = B*A%S 
      real_diag_mult%E       = B*A%E 
      real_diag_mult%I       = B*A%I 
      real_diag_mult%H       = B*A%H
      real_diag_mult%R       = B*A%R
      real_diag_mult%C       = B*A%C 
      real_diag_mult%D       = B*A%D 
      real_diag_mult%A       = B*A%A 
   end function real_diag_mult

   function diag_diag_mult(A,B)
      type(diag) diag_diag_mult
      type(diag), intent(in) :: A
      type(diag), intent(in) :: B
      diag_diag_mult%S       = A%S  * B%S 
      diag_diag_mult%E       = A%E  * B%E 
      diag_diag_mult%I       = A%I  * B%I 
      diag_diag_mult%H       = A%H *  B%H
      diag_diag_mult%R       = A%R *  B%R
      diag_diag_mult%C       = A%C  * B%C 
      diag_diag_mult%D       = A%D  * B%D 
      diag_diag_mult%A       = A%A  * B%A 
   end function diag_diag_mult


   subroutine assign_diag(A,r)
      type(diag), intent(out) :: A
      real, intent(in) :: r
      A%S       = r
      A%E       = r
      A%I       = r
      A%H       = r
      A%R       = r
      A%C       = r
      A%D       = r
      A%A       = r
   end subroutine assign_diag


end module mod_diag

