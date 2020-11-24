module m_Rprior
! This routine sets the prior time series for R. 
! The default is to use a stepwise discontinuous and identical prior R for all countries.
! The default is overrided by defining the Rxxx.in files for each country.
! The Rxxx.in files should be of the format:
!  0    4.000    0.200
!  1    4.000    0.200
! 10    2.000    0.100
! ...
! rdim  1.000    0.100
! Thus, always include the initial time and the dimension of R(t) rdim (defined in mod_dimensions.F90)
! You can specify every time index, or linear interpolation is used to fill in missing values.
! Different R priors can be used for different countries.
contains
subroutine Rprior()
   use mod_dimensions
   use m_readinfile
   use mod_params
   use mod_parameters 
   implicit none
   logical ex
   character(len=3) tag3
   integer ir,i,j,ii,ic,nrlines
   integer i0,i1
   real ar,as
   real dt,t
   real fgR(nrint),stdR(nrint)
   real, allocatable :: Rtmp(:),Stmp(:)
   integer, allocatable :: ind(:)

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Setting priors for R(t) for each country'

   do ic=1,nc
      write(tag3,'(i3.3)')ic
      inquire(file='R'//tag3//'.in',exist=ex)
      if (ex) then
         print '(tr3,a)','Reading prior R and standard deviation from R'//tag3//'.in'
         open(10,file='R'//tag3//'.in')
            do i=0,min(nt,rdim)
               read(10,*,end=200)ii
            enddo
         200 rewind(10)
            nrlines=i-1
            print '(tr3,a,i5)','number of lines in R'//tag3//'.in is',i
            allocate(Rtmp(0:i),Stmp(0:i),ind(0:i))
            do i=0,nrlines
               read(10,*)ind(i),Rtmp(i),Stmp(i)
            enddo
         close(10)

         if (ind(0) /= 0 ) then
            print '(tr3,a)','First line in R'//tag3//'.in should have index=0'
            stop
         endif

! Liner interpolation between points in Rxxx.in
         p%R(0,ic)=Rtmp(0); parstd%R(0,ic)=Stmp(0)
         do i=1,nrlines
            i0=ind(i-1)
            i1=ind(i)
            ar=(Rtmp(i)-Rtmp(i-1))/real(i1-i0)
            as=(Stmp(i)-Stmp(i-1))/real(i1-i0)

            do j=ind(i-1)+1,min(ind(i),rdim)
               p%R(j,ic)     = Rtmp(i-1)+ar*real(j-i0) 
               parstd%R(j,ic)= Stmp(i-1)+as*real(j-i0) 
            enddo
         enddo

! Liner extrapolation to end of simulation if needed 
         if (ind(nrlines) < min(nt,rdim)) then
            do j=nrlines+1,min(nt,rdim)
               p%R(j,ic)=p%R(nrlines,ic)
               parstd%R(j,ic)=parstd%R(nrlines,ic)
            enddo
         endif
         deallocate(ind,Rtmp,Stmp)

! Saving the interpolated Rxxx.dat file for checks
         open(10,file='R'//tag3//'.dat')
            do i=0,min(nt,rdim)
               write(10,'(i6,2f13.3)')i,p%R(i,ic),parstd%R(i,ic)
            enddo
         close(10)

      else 
! Setting defaults and write template files the first time if Rxxx.in doesn't exist
         fgR(1:nrint)=[3.8,0.8,1.0]
         stdR(1:nrint)=[0.2,0.15,0.15]
         dt= time/real(nt-1) 
         do i=0,min(nt,rdim)
            t= 0.0 + real(i)*dt
            if (t < Tinterv(1)) then
               ir=1
            elseif (Tinterv(1) <= t .and. t < Tinterv(2) ) then
               ir=2
            elseif (t >= Tinterv(2)) then
               ir=3
            endif
            p%R(i,ic)=fgR(ir)     
            parstd%R(i,ic) = stdR(ir) 
         enddo

         print '(tr3,a)','Saving default template file for prior R in R'//tag3//'.in'
         open(10,file='R'//tag3//'.in')
            do i=0,min(nt,rdim)
               write(10,'(i6,2f13.3)')i,p%R(i,ic),parstd%R(i,ic)
            enddo
         close(10)
      endif
   enddo
   print '(a)','Done loading R priors (m_Rprior.F90)'


end subroutine
end module

