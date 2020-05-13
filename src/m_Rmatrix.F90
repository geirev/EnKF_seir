module m_Rmatrix
! The R matrices are used to define the reproduction number for and inbetween the different
! age groups. The matrices can be different for different intervention time periods, and their
! uncertainty is modeled using a stochastic factor multiplied with the matrices. See the f function 
! at the end of seir.F90.
use m_agegroups
use m_readinputs
real Rmat(na,na,nrint)
contains
subroutine Rmatrix
   implicit none
   integer i,j,k
   logical ex
   character(len=10) ctmp
   character(len=2)  tag2
   real sumR
   real a(na),b,c
! Set default Rmatrices identical to one meaning same R number for and between all agegroups 
   Rmat(:,:,:) = 1.0

! Reading user defined R matrices if they exist, otherwise save the default one
   do k=1,nrint
      write(tag2,'(i2.2)')k
      inquire(file='Rmatrix_'//tag2//'.in',exist=ex)
      open(10,file='Rmatrix_'//tag2//'.in')
         if (ex) then
            print '(a,i1,a,a,a)','reading Rmat(:,:,',k,') from Rmatrix_'//tag2//'.in'
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

      print '(a,i1)','Input Rmatrix_0',k
      do i=1,na
         print '(11f10.3)',Rmat(i,:,k)
      enddo

! Rescaling rows of R matrix by age groups such that the norm of the 
! rescaled a' R a=1.0.  The growt or decline of the epidemic is then
! entirely controlled by the scalar function R(t) 
      print '(a,i1)','Rescaled Rmatrix_0',k
      do i=1,na
         a(i)=agegroup(i)/sum(agegroup(:))
      enddo
      c=dot_product(a,matmul(Rmat(:,:,k),a))
      Rmat(:,:,k)=Rmat(:,:,k)/c
      print '(11f10.3)',Rmat(:,:,k)

   enddo

end subroutine
end module
