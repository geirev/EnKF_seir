module m_readvariantcond
use mod_dimensions

type varcond_data
   real vaccinated
   real V_qs
   real V_qf
end type
type(varcond_data) varcond(nc)

contains
subroutine readvariantcond()
   use mod_states
   use mod_params
   use mod_parameters
   use m_readagegroups
   implicit none
   logical ex
   character(len=3) tag3
   integer iread,ic

   print '(a)','--------------------------------------------------------------------------------'
   iread=0
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='variantcond'//tag3//'.in',exist=ex)
      if (.not.ex) then
         print '(tr3,a)','Did not find variantcond'//tag3//'.in, generates random template file'
         print '(tr3,a)','You should set appropriate values in this file!'
         print '(tr3,a)','Template file is will be read next!'
         open(10,file='variantcond'//tag3//'.in')
            varcond(ic)%vaccinated=0.8
            varcond(ic)%V_qs=0.05
            varcond(ic)%V_qf=0.01
            write(10,'(2f13.5,a)')varcond(ic)%vaccinated
            write(10,'(2f13.5,a)')varcond(ic)%V_qs
            write(10,'(2f13.5,a)')varcond(ic)%V_qf
         close(10)
      endif
      print '(tr3,a)','Reading variantcond'//tag3//'.in'
      open(10,file='variantcond'//tag3//'.in')
         read(10,*)varcond(ic)%vaccinated
         read(10,*)varcond(ic)%V_qs
         read(10,*)varcond(ic)%V_qf
      close(10)
      iread=iread+1
   enddo

end subroutine
end module

