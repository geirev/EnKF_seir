module m_cfrtimedep
contains
subroutine cfrtimedep(t,fac_s,fac_f)
   use mod_dimensions
   use m_readvaccines
   use m_readvariant
   implicit none

   type varcond_data
      real vaccinated   ! Expected fraction of people that will take the vaccine
      real V_qs         ! Probability for vaccinated to get severe symptoms
      real V_qf         ! Probability for vaccinated to get fatal symptoms
   end type
   type(varcond_data) varcond(nc)

   type vaccov_data
      real power        ! 2.84000
      real coef         ! 1.20000
   end type
   type(vaccov_data) vaccov(nc)

   real, intent(in)  :: t
   real, intent(out) :: fac_s
   real, intent(out) :: fac_f
   integer ic         ! Country counter
   integer ia         ! Ageclass receiving the first vaccine
   real g             ! Number of vaccinated as a function of time

   do ic=1,nc
      varcond(ic)%vaccinated=0.80000
      varcond(ic)%V_qs=      0.05000
      varcond(ic)%V_qf=      0.01000
      vaccov(ic)%power=2.84
      vaccov(ic)%coef=1.2

      ia = minloc(vaccine(:,ic)%start_day, DIM=1)
      if (t > max(variant(ic)%start_day,vaccine(ia,ic)%start_day)) then
         g = (vaccov(ic)%coef)*(1.0E-07) * (t - vaccine(ia,ic)%start_day)**vaccov(ic)%power
         fac_s = min(g,varcond(ic)%vaccinated) * varcond(ic)%V_qs + (1.0 - min(g,varcond(ic)%vaccinated))*1.0
         fac_f = min(g,varcond(ic)%vaccinated) * varcond(ic)%V_qf + (1.0 - min(g,varcond(ic)%vaccinated))*1.0
      else
         fac_s = 1.0
         fac_f = 1.0
      endif
   enddo
end subroutine
end module
