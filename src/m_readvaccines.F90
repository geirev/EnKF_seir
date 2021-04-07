module m_readvaccines
! Defines the flow from Succeptible to Vaccinate for each age group.
! Define start date of vaccination and 1/time_scale for vaccination process. 
! Define end date of vaccination allowing for leaving a rest of unvaccinated in S.
! Assumes 100% efficiency of the vaccines.

use mod_dimensions

type vaccine_data
   real start_day
   real end_day
   real time
end type
type(vaccine_data) vaccine(na,nc)

contains
subroutine readvaccines
   use m_getday
   implicit none
   character(len=3) tag3
   logical ex
   integer i,ic
   real tmp1,tmp2,tmp3

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Reading eventual vaccine???.in file (m_readvaccines.F90):'
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='vaccines'//tag3//'.in',exist=ex)
      if (ex) then
         open(10,file='vaccines'//tag3//'.in')
            print '(tr3,a)','Reading vaccine data from vaccine'//tag3//'.in'
            do i=1,na
               read(10,*)vaccine(i,ic)%start_day, vaccine(i,ic)%end_day, vaccine(i,ic)%time
               vaccine(i,ic)%time=max(1.0,vaccine(i,ic)%time)
               write(*,'(2f8.0,tr3,a10,a,a10,f12.5)')vaccine(i,ic)%start_day, vaccine(i,ic)%end_day,&
                                                     getdate(vaccine(i,ic)%start_day+startday),&
                                               '-->',getdate(vaccine(i,ic)%end_day+startday),&
                                                     vaccine(i,ic)%time
            enddo
         close(10)
      else
         vaccine(:,ic)%start_day=0.0
         vaccine(:,ic)%end_day=0.0
         vaccine(:,ic)%time=100.0
         open(10,file='vaccines'//tag3//'.template')
            tmp1=100.0
            tmp2=300.0
            tmp3=20.0
            do i=1,na
               write(10,*)vaccine(i,ic)%start_day, vaccine(i,ic)%end_day, vaccine(i,ic)%time
            enddo
         close(10)
      endif
   enddo
   print '(a)','--------------------------------------------------------------------------------'

end subroutine
end module
