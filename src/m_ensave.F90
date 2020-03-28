module m_ensave
contains
subroutine ensave(A,ave,nout,nt,nrens)
   implicit none
   integer, intent(in) :: nout
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   real, intent(in)  :: A(nout,0:nt,nrens)
   real, intent(out) :: ave(nout,0:nt)
   integer j

   ave(:,:)=A(:,:,1)
   do j=2,nrens
      ave(:,:)=ave(:,:)+A(:,:,j)
   enddo
   ave=(1.0/real(nrens))*ave

end subroutine
end module
