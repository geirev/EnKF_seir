module m_Rmatrix
use m_agegroups
real, save         :: Rmat(na,na)
contains
subroutine Rmatrix
   implicit none
   integer i,j
   logical :: lpread=.true.
   logical ex
   character(len=10) ctmp
! Transmission factors when opening kindergardens after easter.
!                   1    2    3    4    5   6    7    7    9    10   11 
   Rmat( 1,:)= (/ 3.7, 2.0, 2.0, 1.5, 1.5, 1.1, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 2,:)= (/ 0.0, 3.7, 2.0, 1.5, 1.5, 1.5, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 3,:)= (/ 0.0, 0.0, 1.0, 1.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 4,:)= (/ 0.0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 5,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 6,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 7,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 7,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.9, 0.9, 0.9 /)
   Rmat( 9,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.9, 0.9 /)
   Rmat(10,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9, 0.9 /)
   Rmat(11,:)= (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.9 /)
   do j=1,na
   do i=j+1,na
      Rmat(i,j)=Rmat(j,i)
   enddo
   enddo

   if (lpread) then
      inquire(file='Rmatrix.in',exist=ex)
      open(10,file='Rmatrix.in')
         if (ex) then
            print *,'reading Rmat from Rmatrix.in'
            read(10,*)
            do i=1,na
               read(10,*)ctmp,Rmat(i,:)
            enddo
         else
            write(10,'(a12,100(a))')'Age ranges: ',(agerange(i),i=1,na)
            do i=1,na
               write(10,'(a,100f10.2)')agerange(i),Rmat(i,:)
            enddo
         endif
      close(10)
      lpread=.false.
   endif

   do j=1,na
   do i=j+1,na
      Rmat(i,j)=Rmat(j,i)
   enddo
   enddo
   
   print *
   print *,'Rmat:'
   print '(11f10.2)',Rmat
   print *

end subroutine
end module
