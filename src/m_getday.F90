module m_getday
   integer :: startday=0
   integer, parameter  :: refyear= 2019
   integer, parameter  :: maxyear= 2022
   integer, parameter  :: refmonth=1

   integer days_in_year(refyear:maxyear)
   integer days_in_month(refyear:maxyear,12)


contains

integer function assign_calendar()
   implicit none
   days_in_year(:)   =(/365, 366, 365, 365 /)   ! 2019, 2020, 2021 2022
   days_in_month(refyear  ,:)=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2019
   days_in_month(refyear+1,:)=(/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2020
   days_in_month(refyear+2,:)=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2021
   days_in_month(refyear+3,:)=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2022
   assign_calendar=1
end function


integer function getday(id,im,iy)
   implicit none
   integer, intent(in) :: id
   integer, intent(in) :: im
   integer, intent(in) :: iy
   integer i

   i=assign_calendar()

   getday=0
! Sum up the days in the previous years since the refyear
   do i=refyear,iy-1
      getday=getday+days_in_year(i)
   enddo

   do i=1,im-1
      getday=getday+days_in_month(iy,i) 
   enddo
   getday=getday+id-startday

end function

character(len=10) function getdate(time)
   implicit none
   real, intent(in) :: time
   integer t
   integer i,iy,im,id

   i=assign_calendar()
   t=nint(time)

! find year
   do i=refyear,maxyear
      if (t > days_in_year(i)) then
         t=t-days_in_year(i)
      else
         iy=i
         exit
      endif
   enddo


! find month
   do i=1,12
      if (t > days_in_month(iy,i)) then
         t=t-days_in_month(iy,i)
      else
         im=i
         exit
      endif
   enddo
   
! find day
  id=t

  write(getdate,'(i4.4,a,i2.2,a,i2.2)')iy,'-',im,'-',id
!  print '(i2.2,a,i2.2,a,i4.4)',id,'_',im,'_',iy 
!  print '(a)',getdate


end function
end module
