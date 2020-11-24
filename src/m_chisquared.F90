module m_chisquared
contains
subroutine chisquared()
   use mod_dimensions
   use m_enkfini
   use m_enkfprep
   real    chid,chih,chic
   integer id,ih,ic
   integer j

   chid = 0.0
   chih = 0.0
   chic = 0.0
   id = 0
   ih = 0
   ic = 0
   do j=1,nrobs
      select case(obs(j)%c)
      case('d')
         id = id + 1     
         chid = chid + innovation(j)**2     
      case('h')
         ih = ih + 1     
         chih = chih + innovation(j)**2     
      case('c')
         ic = ic + 1     
         chic = chic + innovation(j)**2     
      case default
         stop 'Measurement type not found'
      end select
   enddo
   if (id > 0) print '(a,f13.4)', 'chi-square death:        ',chid
   if (ih > 0) print '(a,f13.4)', 'chi-square hospitalized: ',chih
   if (ic > 0) print '(a,f13.4)', 'chi-square cases:        ',chic
   print '(a,i4)','total number of obs: ',nrobs
   print '(a,f13.4)','total chi-square obs: ',chid+chih+chic
   print *
end subroutine
end module
