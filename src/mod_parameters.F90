module mod_parameters
   use mod_params
   type (params) p
   type (params) pfg
   type (params) parstd
   type (paramnames) parnames
  
   integer, parameter :: nrpar=11
   real :: N
   real :: Tinterv
   real :: Tinterv2
   real :: Xmax              
   real :: time
   real :: minpar
   real :: rtmax
end module 
