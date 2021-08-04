module m_readvariant
! If a new variant of the virus appars where the vaccine is ineffective we can move
! vaccinated back into succeptible for reinfection.

use mod_dimensions

type variant_data
   real start_day
   real end_day
   real time
end type
type(variant_data) variant(nc)

contains
subroutine readvariant
   use m_getday
   implicit none
   character(len=3) tag3
   logical ex
   integer ic

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Reading eventual variant???.in file (m_readvariant.F90):'
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='variant'//tag3//'.in',exist=ex)
      if (ex) then
         open(10,file='variant'//tag3//'.in')
            print '(tr3,a)','Reading variant data from variant'//tag3//'.in'
            read(10,*)
            read(10,*)
            read(10,*)variant(ic)%start_day, variant(ic)%end_day, variant(ic)%time
            write(*,'(2f8.0,tr3,a10,a,a10,f12.5)')variant(ic)%start_day, variant(ic)%end_day,&
                                                  getdate(variant(ic)%start_day+startday),&
                                            '-->',getdate(variant(ic)%end_day+startday),&
                                                  variant(ic)%time
         close(10)
      else
         variant(ic)%start_day=0.0
         variant(ic)%end_day=0.0
         variant(ic)%time=100.0
         open(10,file='variant'//tag3//'.template')
               write(10,*)'Schedule for moving vaccinated back to susceptible due to new virus variant'
               write(10,*)' startday','    endday',' timescale'
               write(10,'(3f10.1)')variant(ic)%start_day, variant(ic)%end_day, variant(ic)%time
         close(10)
      endif
   enddo
   print '(a)','--------------------------------------------------------------------------------'

end subroutine
end module
