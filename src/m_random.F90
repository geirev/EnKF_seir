module m_random
contains
subroutine random(work1,n)
!  Returns a vector of random values N(variance=1,mean=0)
   implicit none
   integer, intent(in) :: n
   real,   intent(out) :: work1(n)
   real,   allocatable :: work2(:)
   real, parameter   ::  pi=3.141592653589

   allocate (work2(n))

   call random_number(work1)
   call random_number(work2)
   work1= sqrt(-2.0*log(work1+tiny(1.0)))*cos(2.0*pi*work2)

   deallocate(work2)
end subroutine random
end module m_random
