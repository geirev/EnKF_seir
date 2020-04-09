module m_getday
integer :: startday=1
contains
integer function getday(id,im,iy)
   integer, intent(in) :: id
   integer, intent(in) :: im
   integer, intent(in) :: iy
   integer, parameter  :: refyear= 2019
   integer, parameter  :: maxyear= 2021
   integer, parameter  :: refmonth=1
   integer, parameter  :: refdat=1

   integer days_in_year(refyear:maxyear)
   integer days_in_month(refyear:maxyear,12)
   integer i

   days_in_year(:)   =(/365, 366, 365 /)   ! 2019, 2020, 2021
   days_in_month(refyear  ,:)=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2019
   days_in_month(refyear+1,:)=(/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2020
   days_in_month(refyear+2,:)=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) ! For 2021

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
end module
