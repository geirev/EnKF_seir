module m_pfactors
use m_agegroups

real, save         :: pm(0:na-1)
real, save         :: ps(0:na-1)
real, save         :: pf(0:na-1)

contains
subroutine pfactors
   use mod_parameters
   real dead, seve
   integer i

! Defining initial fractions of mild, severe, and fatal cases for different agegroups
!            1    2    3    4    5    6    7    8    9    10   11
   pm(:)= (/ 1.0, 1.0, 1.0, 1.0, 1.0, 0.7, 0.5, 0.3, 0.2, 0.1, 0.1 /)
   ps(:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.6, 0.5, 0.6, 0.3, 0.1 /)
   pf(:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.2, 0.4, 0.6, 0.8 /)

!  Initial ratios:
   print '(a,11f12.4)','pm0: ',pm(:) 
   print '(a,11f12.4)','ps0: ',ps(:) 
   print '(a,11f12.4)','pf0: ',pf(:) 

   dead=0.0
   seve=0.0
   do i=0,na-1
      dead=dead+pf(i)*agegroup(i)/sum(agegroup(:))
      seve=seve+ps(i)*agegroup(i)/sum(agegroup(:))
   enddo
!   print '(a,f10.4)','dead ratio=',dead
!   print '(a,f10.4)','seve ratio=',seve
   
   pf(:)=(CFR/dead)*pf(:)
   ps(:)=(P_SEVERE/seve)*ps(:)
   pm(:)=1.0-ps(:)-pf(:)
   print '(a,11f12.4)','pm1: ',pm(:) 
   print '(a,11f12.4)','ps1: ',ps(:) 
   print '(a,11f12.4)','pf1: ',pf(:) 

   dead=0.0
   seve=0.0
   do i=0,na-1
      dead=dead+pf(i)*agegroup(i)/sum(agegroup(:))
      seve=seve+ps(i)*agegroup(i)/sum(agegroup(:))
   enddo
   print '(a,f10.4)','dead ratio2=',dead
   print '(a,f10.4)','seve ratio2=',seve
end subroutine
end module
