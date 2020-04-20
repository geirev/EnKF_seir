module m_Rmatrix
! The R matrices are used to define the reproduction number for and inbetween the different
! age groups. The matrices can be different for different intervention time periods, and their
! uncertainty is modeled using a stochastic factor multiplied with the matrices. See the f function 
! at the end of seir.F90.
use m_agegroups
real Rmat(na,na,nrint)
contains
subroutine Rmatrix
   implicit none
   integer i,j,k
   logical ex
   character(len=10) ctmp
   character(len=2)  tag2

! Set default Rmatrices identical to one meaning same R number for and between all agegroups 
   Rmat(:,:,:) = 1.0

! Reading user defined R matrices if they exist, otherwise save the default one
   do k=1,nrint
      write(tag2,'(i2.2)')k
      inquire(file='Rmatrix_'//tag2//'.in',exist=ex)
      open(10,file='Rmatrix_'//tag2//'.in')
         if (ex) then
            print *,'reading Rmat from Rmatrix.in'
            read(10,*)
            do i=1,na
               read(10,*)ctmp,Rmat(i,:,k)
            enddo
         else
            write(10,'(a12,100(a))')'Age ranges: ',(agerange(i),i=1,na)
            do i=1,na
               write(10,'(a,100f10.2)')agerange(i),Rmat(i,:,k)
            enddo
         endif
      close(10)

! ensure a symmetric R matrix
      do j=1,na
      do i=j+1,na
         Rmat(i,j,k)=Rmat(j,i,k)
      enddo
      enddo
      
      print *
      print *,'Rmat:'
      print '(11f10.2)',Rmat(:,:,k)
      print *
   enddo

end subroutine
end module
