module mod_parameters
   real, save :: Time_to_death
   real, save :: N
   real, save :: I0                
   real, save :: R0                
   real, save :: Rt                
   real, save :: D_incbation       
   real, save :: D_infectious      
   real, save :: D_recovery_mild   
   real, save :: D_recovery_severe 
   real, save :: D_hospital_lag    
   real, save :: D_death           
   real, save :: CFR               
   real, save :: InterventionTime  
   real, save :: Xmax              
   real, save :: p_severe

   real, save :: duration
   real, save :: time
end module 
