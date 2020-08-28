module m_Rmatrix
! The R matrices are used to define the reproduction number for and inbetween the different
! age groups. The matrices can be different for different intervention time periods, and their
! uncertainty is modeled using a stochastic factor multiplied with the matrices. See the f function 
! at the end of seir.F90.
use mod_dimensions
real Rmat(na,na,nrint,nc)
real RC(nc,nc,nrint)
contains
subroutine Rmatrix()
   use m_readagegroups
   implicit none
   integer i,j,k,ic
   logical ex
   character(len=10) ctmp
   character(len=2)  tag2
   character(len=3)  tag3
   real sumR
   real a(na),b,c

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Definition or R matrices for each country (m_Rmatrix.F90)'
! Set default Rmatrices identical to one meaning same R number for and between all agegroups 
   Rmat(:,:,:,:) = 1.0

! Reading user defined R matrices if they exist, otherwise all element are equal to 1.0
   do ic=1,nc
      write(tag3,'(i3.3)')ic
      do k=1,nrint
         write(tag2,'(i2.2)')k
         inquire(file='Rmatrix_'//tag2//'_'//tag3//'.in',exist=ex)
         if (ex) then
            open(10,file='Rmatrix_'//tag2//'_'//tag3//'.in')
               print '(tr3,a,i1,a,i3,a,a,a)','reading Rmat(:,:,',k,',',ic,') from Rmatrix_'//tag2//'_'//tag3//'.in'
               read(10,*)
               do i=1,na
                  read(10,*)ctmp,Rmat(i,:,k,ic)
               enddo
            close(10)

! Enforce a symmetric R matrix (but for which reason?)
            do j=1,na
            do i=j+1,na
               Rmat(i,j,k,ic)=Rmat(j,i,k,ic)
            enddo
            enddo
            print *

            print '(tr3,a)','Input Rmatrix_'//tag2//'_'//tag3
            do i=1,na
               print '(tr3,111f10.3)',Rmat(i,:,k,ic)
            enddo

! Rescaling rows of R matrix for each country by age groups such that the norm of the 
! rescaled a' R a=1.0.  The growth or decline of the epidemic is then
! entirely controlled by the scalar function R(t) for each country 
            do i=1,na
               a(i)=agegroup(i,ic)/Ntot(ic)
            enddo
            c=dot_product(a,matmul(Rmat(:,:,k,ic),a))
            Rmat(:,:,k,ic)=Rmat(:,:,k,ic)/c
            print '(tr3,a)','Rescaled Rmatrix_'//tag2//'_'//tag3
            do i=1,na
               print '(tr3,111f10.3)',Rmat(i,:,k,ic)
            enddo
         else
            print '(tr3,a)','Rmatrix_'//tag2//'_'//tag3//' has all elements equal to 1.0'
         endif
      enddo
   enddo

! Write template file with ones
   ic=1; k=1
   write(tag3,'(i3.3)')ic
   write(tag2,'(i2.2)')k
   print '(tr3,a,i1)','Writing a default template file Rmatrix_'//tag2//'_'//tag3//'.template'
   print '(tr3,a)','Copy template file to Rmatrix_yy_xxx.in to redefine R matrix for a country xxx during intervention period yy'
   open(10,file='Rmatrix_'//tag2//'_'//tag3//'.template')
      write(10,'(a12,100(a))')'Age ranges: ',(agerange(i),i=1,na)
      do i=1,na
         write(10,'(a,100f10.2)')agerange(i),Rmat(i,:,k,ic)
      enddo
   close(10)


! Read RC matrix defining interaction among countries
   print '(tr3,a)','Setting or reading RC matrix of country-country interactions'
   do k=1,nrint
      write(tag2,'(i2.2)')k
      inquire(file='RC_'//tag2//'.in',exist=ex)
      if (ex) then
         open(10,file='RC_'//tag2//'.in')
            do i=1,nc
               read(10,*)RC(i,:,k)
            enddo
         close(10)
      else
         print '(tr3,a,i1)','Writing default template file RC_'//tag2//'.in'
         open(10,file='RC_'//tag2//'.in')
            RC(:,:,k)=0.001
            do i=1,nc
               RC(i,i,k)=1.0
               write(10,*)RC(i,:,k)
            enddo
         close(10)
      endif
   enddo
   print '(a)','Done setting and reading Rmatrices (m_Rmatrix.F90)'

end subroutine
end module
