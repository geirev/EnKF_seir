module m_readagegroups
use mod_dimensions
use mod_parameters
! The number of people in each of the na agegroups are for each of the nc
real agegroup(na,nc)
real Ntot(nc)
character(len=10) agerange(na) 

contains
subroutine readagegroups
   use mod_dimensions
   implicit none
   integer, parameter :: nrages=106
   real               :: ages_male(1:nrages)
   real               :: ages_female(1:nrages)
   integer i,ic,im,if
   logical lpop,lage
   character(len=3) tag3

   integer ia(na)
   integer ib(na)

   print '(a)','--------------------------------------------------------------------------------'
   print '(a)','Loading population data for each country'
! Default choices for na=11 agegroups
   agerange(1)= ' 0-5   ';   ia( 1)=0 ; ib( 1)=5
   agerange(2)= ' 6-12  ';   ia( 2)=6 ; ib( 2)=12
   agerange(3)= ' 13-19 ';   ia( 3)=13; ib( 3)=19
   agerange(4)= ' 20-29 ';   ia( 4)=20; ib( 4)=29
   agerange(5)= ' 30-39 ';   ia( 5)=30; ib( 5)=39
   agerange(6)= ' 40-49 ';   ia( 6)=40; ib( 6)=49
   agerange(7)= ' 50-59 ';   ia( 7)=50; ib( 7)=59
   agerange(8)= ' 60-69 ';   ia( 8)=60; ib( 8)=69
   agerange(9)= ' 70-79 ';   ia( 9)=70; ib( 9)=79
   agerange(10)=' 80-89 ';   ia(10)=80; ib(10)=89
   agerange(11)=' 90-105';   ia(11)=90; ib(11)=105


   
! If the file "populationxxx.in" with annual distribution of males and females from country xxx exists it will be read.
! Alternatively the populations per agegroup will be read from agegroupsxxx.in
! If none of the files exist, the Norwegian defaults are used and template files are generated.

   do ic=1,nc
      print '(tr3,a,i3)','processing agegroup data for country:',ic
      write(tag3,'(i3.3)')ic
      inquire(file='population'//tag3//'.in',exist=lpop)
      inquire(file='agegroups'//tag3//'.in',exist=lage)

      if (lpop) then
         ! Read populationxxx.in
         open(10,file='population'//tag3//'.in')
            print '(tr3,a)','Reading annual population numbers of males and females from population'//tag3//'.in'
            ages_male=0.0
            ages_female=0.0
            do i=1,nrages
               read(10,*,end=100)im,if
               ages_male(i)=real(im)
               ages_female(i)=real(if)
            enddo
     100 close(10)

         ! Compute number of people in each agegroup
         do i=1,na
            agegroup(i,ic)=sum(ages_male(ia(i)+1:ib(i)+1))    + sum(ages_female(ia(i)+1:ib(i)+1))
         enddo

         ! write agegroupxxx.out
         open(10,file='agegroups'//tag3//'.out')
            do i=1,na
               write(10,*)agerange(i),agegroup(i,ic)
               write(*,'(tr3,a10,a,f13.4)')agerange(i),': ',agegroup(i,ic)
            enddo
         close(10)
      endif
      
      if (.not.lpop .and. lage) then
         ! reading supplied file with population per agegroup from agegroupxxx.in
         print '(tr3,a)','Reading agegroups and population from agegroups'//tag3//'.in'
         open(10,file='agegroups'//tag3//'.in')
               do i=1,na
                  read(10,*)agerange(i),agegroup(i,ic)
                  write(*,'(tr3,a10,a,f13.4)')agerange(i),': ',agegroup(i,ic)
               enddo
         close(10)
      endif

      if (.not.lpop .and. .not.lage) then
         ! Saving template files using hardcoded defaults for norway
         print '(tr3,a)','Neither population'//tag3//'.in nor  agegroups'//tag3//'.in exists. Running with Norwegian defaults'
         call population_norway(ages_male,ages_female,nrages)

         do i=1,na
            agegroup(i,ic)=sum(ages_male(ia(i)+1:ib(i)+1))    + sum(ages_female(ia(i)+1:ib(i)+1))
         enddo

         ! saving template files with norwegian data
         open(10,file='population'//tag3//'.template')
            do i=1,nrages
               write(10,*)nint(ages_male(i)),nint(ages_female(i))
            enddo
         close(10)

         open(10,file='agegroups'//tag3//'.template')
            do i=1,na
               write(10,*)agerange(i),agegroup(i,ic)
            enddo
         close(10)
      endif

      Ntot(ic)=sum(agegroup(:,ic))
      print '(tr3,a,f13.0)','Total population for Country '//tag3//': ',Ntot(ic)
      print *

   enddo

end subroutine

subroutine population_norway(ages_male,ages_female,nrages)
! Numbers from https://www.ssb.no/statbank/table/07459/
   implicit none
   integer, intent(in):: nrages
   real, intent(out)  :: ages_male(1:nrages)
   real, intent(out)  :: ages_female(1:nrages)
   
   ages_male(1  )= 28208.0
   ages_male(2  )= 28847.0
   ages_male(3  )= 29823.0
   ages_male(4  )= 31261.0
   ages_male(5  )= 31274.0
   ages_male(6  )= 31375.0
   ages_male(7  )= 31274.0
   ages_male(8  )= 32462.0
   ages_male(9  )= 32834.0
   ages_male(10 )= 33634.0
   ages_male(11 )= 34323.0
   ages_male(12 )= 33903.0
   ages_male(13 )= 32987.0
   ages_male(14 )= 33146.0
   ages_male(15 )= 32229.0
   ages_male(16 )= 32521.0
   ages_male(17 )= 32300.0
   ages_male(18 )= 31684.0
   ages_male(19 )= 32696.0
   ages_male(20 )= 34517.0
   ages_male(21 )= 34794.0
   ages_male(22 )= 34159.0
   ages_male(23 )= 35129.0
   ages_male(24 )= 36127.0
   ages_male(25 )= 36264.0
   ages_male(26 )= 36383.0
   ages_male(27 )= 36912.0
   ages_male(28 )= 38223.0
   ages_male(29 )= 38820.0
   ages_male(30 )= 39360.0
   ages_male(31 )= 39255.0
   ages_male(32 )= 39000.0
   ages_male(33 )= 37580.0
   ages_male(34 )= 37893.0
   ages_male(35 )= 37324.0
   ages_male(36 )= 36743.0
   ages_male(37 )= 37027.0
   ages_male(38 )= 37036.0
   ages_male(39 )= 35869.0
   ages_male(40 )= 36693.0
   ages_male(41 )= 36077.0
   ages_male(42 )= 35875.0
   ages_male(43 )= 34706.0
   ages_male(44 )= 35713.0
   ages_male(45 )= 36319.0
   ages_male(46 )= 37771.0
   ages_male(47 )= 38163.0
   ages_male(48 )= 39253.0
   ages_male(49 )= 39033.0
   ages_male(50 )= 38611.0
   ages_male(51 )= 39317.0
   ages_male(52 )= 38884.0
   ages_male(53 )= 37722.0
   ages_male(54 )= 37814.0
   ages_male(55 )= 37040.0
   ages_male(56 )= 36245.0
   ages_male(57 )= 34357.0
   ages_male(58 )= 33631.0
   ages_male(59 )= 32836.0
   ages_male(60 )= 32363.0
   ages_male(61 )= 32248.0
   ages_male(62 )= 31570.0
   ages_male(63 )= 30629.0
   ages_male(64 )= 30668.0
   ages_male(65 )= 29701.0
   ages_male(66 )= 28740.0
   ages_male(67 )= 28104.0
   ages_male(68 )= 27604.0
   ages_male(69 )= 26278.0
   ages_male(70 )= 26318.0
   ages_male(71 )= 26058.0
   ages_male(72 )= 26143.0
   ages_male(73 )= 25994.0
   ages_male(74 )= 26396.0
   ages_male(75 )= 22809.0
   ages_male(76 )= 21460.0
   ages_male(77 )= 18447.0
   ages_male(78 )= 16354.0
   ages_male(79 )= 13571.0
   ages_male(80 )= 13417.0
   ages_male(81 )= 12269.0
   ages_male(82 )= 11121.0
   ages_male(83 )= 9870.0
   ages_male(84 )= 8714.0
   ages_male(85 )= 7548.0
   ages_male(86 )= 6805.0
   ages_male(87 )= 6097.0
   ages_male(88 )= 5674.0
   ages_male(89 )= 4752.0
   ages_male(90 )= 4161.0
   ages_male(91 )= 3251.0
   ages_male(92 )= 2661.0
   ages_male(93 )= 2077.0
   ages_male(94 )= 1671.0
   ages_male(95 )= 1263.0
   ages_male(96 )= 893.0
   ages_male(97 )= 622.0
   ages_male(98 )= 405.0
   ages_male(99 )= 229.0
   ages_male(100)= 161.0
   ages_male(101)=  87.0
   ages_male(102)=  43.0
   ages_male(103)=  17.0
   ages_male(104)=  12.0
   ages_male(105)=  12.0
   ages_male(106)=  19.0
   ages_female(1  )= 26619.0
   ages_female(2  )= 27165.0
   ages_female(3  )= 28058.0
   ages_female(4  )= 29261.0
   ages_female(5  )= 29547.0
   ages_female(6  )= 29721.0
   ages_female(7  )= 30118.0
   ages_female(8  )= 30789.0
   ages_female(9  )= 31047.0
   ages_female(10 )= 32041.0
   ages_female(11 )=  32507.0
   ages_female(12 )=  32008.0
   ages_female(13 )=  31319.0
   ages_female(14 )=  31494.0
   ages_female(15 )=  30853.0
   ages_female(16 )=  30887.0
   ages_female(17 )=  30722.0
   ages_female(18 )=  30367.0
   ages_female(19 )=  30861.0
   ages_female(20 )=  32067.0
   ages_female(21 )=  32019.0
   ages_female(22 )=  32005.0
   ages_female(23 )=  32688.0
   ages_female(24 )=  33822.0
   ages_female(25 )=  33822.0
   ages_female(26 )=  34470.0
   ages_female(27 )=  35143.0
   ages_female(28 )=  36110.0
   ages_female(29 )=  37214.0
   ages_female(30 )=  38288.0
   ages_female(31 )=  37899.0
   ages_female(32 )=  37654.0
   ages_female(33 )=  36480.0
   ages_female(34 )=  35879.0
   ages_female(35 )=  35260.0
   ages_female(36 )=  35043.0
   ages_female(37 )=  34410.0
   ages_female(38 )=  34780.0
   ages_female(39 )=  34297.0
   ages_female(40 )=  34425.0
   ages_female(41 )=  33954.0
   ages_female(42 )=  33625.0
   ages_female(43 )=  32962.0
   ages_female(44 )=  33568.0
   ages_female(45 )=  34716.0
   ages_female(46 )=  36151.0
   ages_female(47 )=  35886.0
   ages_female(48 )=  37168.0
   ages_female(49 )=  37322.0
   ages_female(50 )=  36790.0
   ages_female(51 )=  37524.0
   ages_female(52 )=  37067.0
   ages_female(53 )=  36116.0
   ages_female(54 )=  35489.0
   ages_female(55 )=  34958.0
   ages_female(56 )=  34348.0
   ages_female(57 )=  33208.0
   ages_female(58 )=  32098.0
   ages_female(59 )=  31634.0
   ages_female(60 )=  31179.0
   ages_female(61 )=  31057.0
   ages_female(62 )=  30961.0
   ages_female(63 )=  30212.0
   ages_female(64 )=  30547.0
   ages_female(65 )=  29630.0
   ages_female(66 )=  29002.0
   ages_female(67 )=  28550.0
   ages_female(68 )=  27818.0
   ages_female(69 )=  26227.0
   ages_female(70 )=  26631.0
   ages_female(71 )=  26516.0
   ages_female(72 )=  26497.0
   ages_female(73 )=  27035.0
   ages_female(74 )=  27605.0
   ages_female(75 )=  24399.0
   ages_female(76 )=  22981.0
   ages_female(77 )=  20104.0
   ages_female(78 )=  18353.0
   ages_female(79 )=  15780.0
   ages_female(80 )=  15915.0
   ages_female(81 )=  15010.0
   ages_female(82 )=  13950.0
   ages_female(83 )=  12765.0
   ages_female(84 )=  11770.0
   ages_female(85 )=  10698.0
   ages_female(86 )=  10004.0
   ages_female(87 )=  9301.0
   ages_female(88 )=  9106.0
   ages_female(89 )=  8328.0
   ages_female(90 )=  7537.0
   ages_female(91 )=  6336.0
   ages_female(92 )=  5499.0
   ages_female(93 )=  4620.0
   ages_female(94 )=  3966.0
   ages_female(95 )=  3106.0
   ages_female(96 )=  2542.0
   ages_female(97 )=  1813.0
   ages_female(98 )=  1328.0
   ages_female(99 )=  980.0
   ages_female(100)=  688.0
   ages_female(101)=  358.0
   ages_female(102)=  227.0
   ages_female(103)=  166.0
   ages_female(104)=  73.0
   ages_female(105)=  51.0
   ages_female(106)=  54.0

end subroutine
end module
