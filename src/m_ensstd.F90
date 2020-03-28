module m_ensstd
contains
subroutine ensstd(A,std,ave,nout,nt,nrens)
   implicit none
   integer, intent(in) :: nout
   integer, intent(in) :: nt
   integer, intent(in) :: nrens
   real, intent(in)  :: A(nout,0:nt,nrens)
   real, intent(in)  :: ave(nout,0:nt)
   real, intent(out) :: std(nout,0:nt)
   integer j

   std=0.0
   do j=1,nrens
      std(:,:)=std(:,:)+(A(:,:,j)-ave(:,:))*(A(:,:,j)-ave(:,:))
   enddo
   std=(1.0/real(nrens-1))*std
   std=sqrt(std)

end subroutine ensstd
end module m_ensstd
