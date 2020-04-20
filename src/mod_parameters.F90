module mod_parameters
   use mod_dimensions
   use mod_params
   type (params) p
   type (params) pfg
   type (params) parstd
   type (paramnames) parnames
  
   real :: N
   real :: Tinterv(nrint-1)
   real :: Xmax              
   real :: time
   real :: minpar
   real :: rtmax
end module 
