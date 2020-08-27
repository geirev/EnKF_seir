module m_pfactors
! The factors pm, ps, and pf determine the fractions of mild, severe, and fatal sick for each agegroup and for each country.
! A first guess is defined here and dumped to files pefactorsxxx.in the first time.
! Later these files are read and can be edited by the user
! Default CFR numbers are taken from https://www.worldometers.info/coronavirus/coronavirus-age-sex-demographics/
! Hospitalization numbers comes from https://www.fhi.no/sv/smittsomme-sykdommer/corona/koronavirus-modellering/

use mod_dimensions
use m_readagegroups

real pm(na,nc)
real ps(na,nc)
real pf(na,nc)
logical :: lpost_pfactors=.false.

contains
subroutine pfactors
   use mod_params
   use mod_parameters
   use m_readinfile
   implicit none
   real dead, seve
   integer i,ic
   integer, save :: iprt=0
   character(len=6) ctmp
   character(len=3) tag3

   logical ex
   logical, save :: lprt=.true.
   logical, save :: lpread=.true.

   real, save :: pmorig(na,nc)
   real, save :: psorig(na,nc)
   real, save :: pforig(na,nc)

   if (lprt) print '(a)','--------------------------------------------------------------------------------'
   if (lprt) print '(a)','Reading p-factors (m_pfactors.F90)'

! Only print diagnostics in the first few iterations
   iprt=iprt+1
   if (iprt >= 3) lprt=.false.



   do ic=1,nc
      write(tag3,'(i3.3)')ic
      if (lpread) then
! setting defaults and/or read UNNORMALIZED pfactors first time readpfactors is called
         inquire(file='pfactors'//tag3//'.in',exist=ex)
         open(10,file='pfactors'//tag3//'.in')
            if (ex) then
               print '(tr3,a)','Reading p-factors from pfactors'//tag3//'.in'
               read(10,*)
               read(10,*)ctmp,psorig(:,ic)
               read(10,*)ctmp,pforig(:,ic)
            else
               ! Default p factors (relative values)
               print '(tr3,a)','Using default p-factors'
               psorig(:,ic)= (/ 0.000, 0.000, 0.00013, 0.0037, 0.011, 0.014, 0.027, 0.039, 0.055, 0.055, 0.055 /)
               pforig(:,ic)= (/ 0.000, 0.000, 0.000  , 0.002 , 0.002, 0.004, 0.013, 0.036, 0.080, 0.140, 0.140 /)
               write(10,'(a12,100(a,tr3))')'Age ranges: ',(agerange(i),i=1,na)
               write(10,'(a6,100f13.4)')'ps:   ',psorig(:,ic)
               write(10,'(a6,100f13.4)')'pf:   ',pforig(:,ic)
            endif
         close(10)
         pmorig(:,ic)=1.0-pforig(:,ic)-psorig(:,ic)

         !  Initial relative ratios:
         print '(tr3,a,i3)','Initial relative p-factors for country : ',ic
         print '(tr3,a,11f12.4)','pm0: ',pmorig(:,ic) 
         print '(tr3,a,11f12.4)','ps0: ',psorig(:,ic) 
         print '(tr3,a,11f12.4)','pf0: ',pforig(:,ic) 
         print *
      endif

! Normalize pfactors to match CFR and sev (this is done after each update of p%CFR and p%sev
      dead=0.0
      seve=0.0
      do i=1,na
         dead=dead+pforig(i,ic)*agegroup(i,ic)/Ntot(ic)
         seve=seve+psorig(i,ic)*agegroup(i,ic)/Ntot(ic)
      enddo
      
      pf(:,ic)=(p%CFR(ic)/dead)*pforig(:,ic)
      ps(:,ic)=(p%sev(ic)/seve)*psorig(:,ic)
      pm(:,ic)=1.0-ps(:,ic)-pf(:,ic)

   !  Initial ratios:
      if (lprt) print '(tr3,a)','Scaled p-factors: '
      if (lprt) print '(tr3,a,11f12.4)','pm1: ',pm(:,ic) 
      if (lprt) print '(tr3,a,11f12.4)','ps1: ',ps(:,ic) 
      if (lprt) print '(tr3,a,11f12.4)','pf1: ',pf(:,ic) 

      if (iprt==1) then
         open(10,file=trim(outdir)//'/pfactors'//tag3//'.prior')
            write(10,'(a12,100(a,tr3))')'Age ranges: ',(agerange(i),i=1,na)
            write(10,'(a6,100f13.4)')'pm:   ',pm(:,ic)
            write(10,'(a6,100f13.4)')'ps:   ',ps(:,ic)
            write(10,'(a6,100f13.4)')'pf:   ',pf(:,ic)
            write(10,'(2(a,f12.5))')'ratios corresponds to CFR=',p%CFR(ic),' and PSEV=',p%sev(ic)
         close(10)
      endif
      if (lpost_pfactors) then
         open(10,file=trim(outdir)//'/pfactors'//tag3//'.posterior')
            write(10,'(a12,100(a,tr3))')'Age ranges: ',(agerange(i),i=1,na)
            write(10,'(a6,100f13.4)')'pm:   ',pm(:,ic)
            write(10,'(a6,100f13.4)')'ps:   ',ps(:,ic)
            write(10,'(a6,100f13.4)')'pf:   ',pf(:,ic)
            write(10,'(2(a,f12.5))')'ratios corresponds to CFR=',p%CFR(ic),' and PSEV=',p%sev(ic)
         close(10)
      endif

   !  Checking ratios
      dead=0.0
      seve=0.0
      do i=1,na
         dead=dead+pf(i,ic)*agegroup(i,ic)/Ntot(ic)
         seve=seve+ps(i,ic)*agegroup(i,ic)/Ntot(ic)
      enddo
      if (lprt) print '(tr3,a)','Check on p-factors: '
      if (lprt) print '(tr3,a,2f10.4)','dead ratio2=',dead,p%CFR(ic)
      if (lprt) print '(tr3,a,2f10.4)','seve ratio2=',seve,p%sev(ic)
      if (iprt==1) print *
   enddo
   lpread=.false. ! never read file again
   if (lprt) print '(a)','Done reading p-factors (m_pfactors.F90)'
end subroutine
end module
