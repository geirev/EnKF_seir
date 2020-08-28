module m_readinicond
contains
subroutine readinicond()
   use mod_dimensions
   use mod_states
   use mod_params
   use mod_parameters
   use m_readagegroups
   implicit none
   logical ex
   character(len=2) ca
   character(len=3) tag3
   real :: infected=100.0,dix=10.0
   real :: cfr=0.0090,sev=0.039
   integer iread,ic,k

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Reading initial infected and exposed for all countries'
   iread=0
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='inicond'//tag3//'.in',exist=ex)
      if (.not.ex) then
         print '(tr3,a)','Did not find inicond'//tag3//'.in, generates random template file'
         print '(tr3,a)','You should set appropriate values in this file!'
         open(10,file='inicond'//tag3//'.in')
            write(10,'(2f13.5,a)')infected,0.1*infected,'   #E0 '
            write(10,'(2f13.5,a)')infected,0.1*infected,'   #I0 '
            write(10,'(2f13.5,a)')cfr,0.1*cfr,'   #CFR '
            write(10,'(2f13.5,a)')sev,0.1*sev,'   #sev '
         close(10)
      else
         print '(tr3,a)','Reading inicond'//tag3//'.in'
         open(10,file='inicond'//tag3//'.in')
            read(10,*)p%E0(ic),parstd%E0(ic)
            read(10,*)p%I0(ic),parstd%I0(ic)
            read(10,*)p%cfr(ic),parstd%cfr(ic)
            read(10,*)p%sev(ic),parstd%sev(ic)
         close(10)
         iread=iread+1
      endif
   enddo
   print '(a)','Done reading initial conditons (m_inicond.F90)'

end subroutine
end module

