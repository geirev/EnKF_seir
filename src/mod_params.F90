module mod_params
! model parameters to be tuned by EnKF
   type paramnames
      character(len=9) ::   I0    =' "    I0"'   
      character(len=9) ::   R(3)  =(/' "    R1"',' "    R2"',' "    R3"'/)   
      character(len=9) ::   Tinf  =' "  Tinf"'
      character(len=9) ::   Tinc  =' "  Tinc"'
      character(len=9) ::   Trecm =' " Trecm"'
      character(len=9) ::   Trecs =' " Trecs"'
      character(len=9) ::   Thosp =' " Thosp"'
      character(len=9) ::   Tdead =' " Tdead"'
      character(len=9) ::   p_sev =' " p_sev"'
      character(len=9) ::   CFR   =' "   CFR"'
   end type paramnames

   type params
      real I0   
      real R(3)
      real Tinf 
      real Tinc 
      real Trecm
      real Trecs
      real Thosp
      real Tdead
      real p_sev
      real CFR 
   end type params

! Overloaded and generic operators
   interface operator(+)
      module procedure add_params
   end interface

   interface operator(-)
      module procedure subtract_params
   end interface

   interface operator(*)
      module procedure params_real_mult,&
                       real_params_mult,&
                       params_params_mult
   end interface

   interface assignment(=)
      module procedure assign_params
   end interface

   interface max
      module procedure max_params
   end interface

contains

   function max_params(A,B)
      type(params) max_params
      type(params), intent(in) :: A
      real,         intent(in) :: B
      max_params%I0          = max(A%I0   ,B)
      max_params%R           = max(A%R    ,B)
      max_params%Tinf        = max(A%Tinf ,B)
      max_params%Tinc        = max(A%Tinc ,B)
      max_params%Trecm       = max(A%Trecm,B)
      max_params%Trecs       = max(A%Trecs,B)
      max_params%Thosp       = max(A%Thosp,B)
      max_params%Tdead       = max(A%Tdead,B)
      max_params%p_sev       = max(A%p_sev,B)
      max_params%CFR         = max(A%CFR  ,B)
   end function max_params

   function add_params(A,B)
      type(params) add_params
      type(params), intent(in) :: A
      type(params), intent(in) :: B
      add_params%I0          = A%I0     + B%I0    
      add_params%R           = A%R      + B%R   
      add_params%Tinf        = A%Tinf   + B%Tinf 
      add_params%Tinc        = A%Tinc   + B%Tinc 
      add_params%Trecm       = A%Trecm  + B%Trecm
      add_params%Trecs       = A%Trecs  + B%Trecs
      add_params%Thosp       = A%Thosp  + B%Thosp
      add_params%Tdead       = A%Tdead  + B%Tdead
      add_params%p_sev       = A%p_sev  + B%p_sev
      add_params%CFR         = A%CFR    + B%CFR 
   end function add_params

   function subtract_params(A,B)
      type(params) subtract_params
      type(params), intent(in) :: A
      type(params), intent(in) :: B
      subtract_params%I0          = A%I0     - B%I0    
      subtract_params%R           = A%R      - B%R    
      subtract_params%Tinf        = A%Tinf   - B%Tinf  
      subtract_params%Tinc        = A%Tinc   - B%Tinc  
      subtract_params%Trecm       = A%Trecm  - B%Trecm
      subtract_params%Trecs       = A%Trecs  - B%Trecs
      subtract_params%Thosp       = A%Thosp  - B%Thosp
      subtract_params%Tdead       = A%Tdead  - B%Tdead
      subtract_params%p_sev       = A%p_sev  - B%p_sev
      subtract_params%CFR         = A%CFR    - B%CFR 
   end function subtract_params

   function params_real_mult(A,B)
      type(params) params_real_mult
      type(params), intent(in) :: A
      real, intent(in) :: B
      params_real_mult%I0          = B*A%I0    
      params_real_mult%R           = B*A%R    
      params_real_mult%Tinf        = B*A%Tinf  
      params_real_mult%Tinc        = B*A%Tinc  
      params_real_mult%Trecm       = B*A%Trecm 
      params_real_mult%Trecs       = B*A%Trecs 
      params_real_mult%Thosp       = B*A%Thosp
      params_real_mult%Tdead       = B*A%Tdead
      params_real_mult%p_sev       = B*A%p_sev 
      params_real_mult%CFR         = B*A%CFR   
   end function params_real_mult

   function real_params_mult(B,A)
      type(params) real_params_mult
      type(params), intent(in) :: A
      real, intent(in) :: B
      real_params_mult%I0          = B*A%I0    
      real_params_mult%R           = B*A%R    
      real_params_mult%Tinf        = B*A%Tinf  
      real_params_mult%Tinc        = B*A%Tinc  
      real_params_mult%Trecm       = B*A%Trecm 
      real_params_mult%Trecs       = B*A%Trecs 
      real_params_mult%Thosp       = B*A%Thosp
      real_params_mult%Tdead       = B*A%Tdead
      real_params_mult%p_sev       = B*A%p_sev 
      real_params_mult%CFR         = B*A%CFR   
   end function real_params_mult

   function params_params_mult(A,B)
      type(params) params_params_mult
      type(params), intent(in) :: A
      type(params), intent(in) :: B
      params_params_mult%I0          = A%I0     * B%I0    
      params_params_mult%R           = A%R      * B%R    
      params_params_mult%Tinf        = A%Tinf   * B%Tinf 
      params_params_mult%Tinc        = A%Tinc   * B%Tinc 
      params_params_mult%Trecm       = A%Trecm  * B%Trecm 
      params_params_mult%Trecs       = A%Trecs  * B%Trecs 
      params_params_mult%Thosp       = A%Thosp  * B%Thosp 
      params_params_mult%Tdead       = A%Tdead  * B%Tdead 
      params_params_mult%p_sev       = A%p_sev  * B%p_sev 
      params_params_mult%CFR         = A%CFR    * B%CFR   
   end function params_params_mult


   subroutine assign_params(A,r)
      type(params), intent(out) :: A
      real, intent(in) :: r
      A%I0          = r
      A%R           = r
      A%Tinf        = r
      A%Tinc        = r
      A%Trecm       = r
      A%Trecs       = r
      A%Thosp       = r
      A%Tdead       = r
      A%p_sev       = r
      A%CFR         = r
   end subroutine assign_params


end module mod_params

