module m_pfactors
use mod_dimensions
use m_agegroups

real, save         :: pm(na)
real, save         :: ps(na)
real, save         :: pf(na)

contains
subroutine pfactors
   use mod_parameters
   implicit none
   real dead, seve
   integer i
   integer, save :: iprt=0
   logical :: lprt=.false.

   iprt=iprt+1
   if (iprt < 3) lprt=.true.
   if (iprt >= 3) lprt=.false.



! Defining initial fractions of mild, severe, and fatal cases for different agegroups
!            1    2    3    4    5    6    7    8    9    10   11
   pm(:)= (/ 1.000, 1.000, 0.998, 0.998, 0.998, 0.900, 0.800, 0.700, 0.600, 0.500, 0.400 /)
   pf(:)= (/ 0.000, 0.000, 0.002, 0.002, 0.002, 0.004, 0.013, 0.036, 0.080, 0.140, 0.140 /)
   ps=1.0-pf-pm

!  Initial ratios:
   if (iprt==1) print '(a,11f12.4)','pm0: ',pm(:) 
   if (iprt==1) print '(a,11f12.4)','ps0: ',ps(:) 
   if (iprt==1) print '(a,11f12.4)','pf0: ',pf(:) 
   if (iprt==1) print *

   dead=0.0
   seve=0.0
   do i=1,na
      dead=dead+pf(i)*agegroup(i)/sum(agegroup(:))
      seve=seve+ps(i)*agegroup(i)/sum(agegroup(:))
   enddo
!   print '(a,f10.4)','dead ratio=',dead
!   print '(a,f10.4)','seve ratio=',seve
   
   pf(:)=(CFR/dead)*pf(:)
   ps(:)=(P_SEVERE/seve)*ps(:)
   pm(:)=1.0-ps(:)-pf(:)
   if (lprt) print '(a,11f12.4)','pm1: ',pm(:) 
   if (lprt) print '(a,11f12.4)','ps1: ',ps(:) 
   if (lprt) print '(a,11f12.4)','pf1: ',pf(:) 

   dead=0.0
   seve=0.0
   do i=1,na
      dead=dead+pf(i)*agegroup(i)/sum(agegroup(:))
      seve=seve+ps(i)*agegroup(i)/sum(agegroup(:))
   enddo
   if (lprt) print '(a,f10.4)','dead ratio2=',dead
   if (lprt) print '(a,f10.4)','seve ratio2=',seve
   if (iprt==1) print *
end subroutine
end module
