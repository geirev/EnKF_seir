module m_pfactors
! The factors pm, ps, and pf determine the fractions of mild, severe, and fatal sick for each agegroup.
! A first guess is defined here and dumped to file the first time.
! Later this file is read and can be edited by the user
use mod_dimensions
use m_agegroups

real pm(na)
real ps(na)
real pf(na)

contains
subroutine pfactors
   use mod_params
   use mod_parameters
   implicit none
   real dead, seve
   integer i
   integer, save :: iprt=0
   logical :: lprt=.true.
   logical ex
   character(len=6) ctmp

   logical :: lpread=.true.
   real, save :: pmorig(na)
   real, save :: psorig(na)
   real, save :: pforig(na)


   iprt=iprt+1
   if (iprt < 3) lprt=.true.
   if (iprt >= 3) lprt=.false.



! Defining initial fractions of mild, severe, and fatal cases for different agegroups
!            1      2      3      4      5      6      7      8      9      10     11
   pmorig(:)= (/ 1.000, 1.000, 0.998, 0.998, 0.998, 0.900, 0.800, 0.700, 0.600, 0.500, 0.400 /)
   pforig(:)= (/ 0.000, 0.000, 0.002, 0.002, 0.002, 0.004, 0.013, 0.036, 0.080, 0.140, 0.140 /)

   if (lpread) then
      inquire(file='pfactors.in',exist=ex)
      open(10,file='pfactors.in')
         if (ex) then
            print '(a)','Reading p-factors from pfactors.in'
            read(10,*)
            read(10,*)ctmp,pmorig(:)
            read(10,*)ctmp,pforig(:)
         else
            write(10,'(a12,100(a,tr3))')'Age ranges: ',(agerange(i),i=1,na)
            write(10,'(a6,100f13.4)')'pm:   ',pmorig(:)
            write(10,'(a6,100f13.4)')'pf:   ',pforig(:)
         endif
      close(10)
      psorig=1.0-pforig-pmorig
      lpread=.false.

!  Initial ratios:
      print '(a,11f12.4)','pm0: ',pmorig(:) 
      print '(a,11f12.4)','ps0: ',psorig(:) 
      print '(a,11f12.4)','pf0: ',pforig(:) 
      print *
   endif

   dead=0.0
   seve=0.0
   do i=1,na
      dead=dead+pforig(i)*agegroup(i)/sum(agegroup(:))
      seve=seve+psorig(i)*agegroup(i)/sum(agegroup(:))
   enddo
   
   pf(:)=(p%CFR/dead)*pforig(:)
   ps(:)=(p%p_sev/seve)*psorig(:)
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
   if (lprt) print '(a,2f10.4)','dead ratio2=',dead,p%CFR
   if (lprt) print '(a,2f10.4)','seve ratio2=',seve,p%P_sev
   if (iprt==1) print *
end subroutine
end module
