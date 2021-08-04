module m_readvaccov
use mod_dimensions

type vaccov_data
   real power
   real coef
end type
type(vaccov_data) vaccov(nc)

contains
subroutine readvaccov()
   implicit none
   logical ex
   character(len=3) tag3
   integer iread,ic

   print '(a)','--------------------------------------------------------------------------------'
   iread=0
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='vaccov'//tag3//'.in',exist=ex)
      if (.not.ex) then
         print '(tr3,a)','Did not find vaccov'//tag3//'.in, generates random template file'
         print '(tr3,a)','You should set appropriate values in this file!'
         print '(tr3,a)','Template file is will be read next!'
         open(10,file='vaccov'//tag3//'.in')
            vaccov(ic)%power=2.84
            vaccov(ic)%coef=1.20
            write(10,'(2f13.5,a)')vaccov(ic)%power
            write(10,'(2f13.5,a)')vaccov(ic)%coef
         close(10)
      endif
      print '(tr3,a)','Reading vaccov'//tag3//'.in'
      open(10,file='vaccov'//tag3//'.in')
         read(10,*)vaccov(ic)%power
         read(10,*)vaccov(ic)%coef
      close(10)
      iread=iread+1
   enddo

end subroutine
end module

