module m_agegroups
integer, parameter :: na=11
real, save         :: agegroup(0:na-1)
contains
subroutine agegroups
use mod_parameters
integer, parameter :: nrages=105
real               :: ages_male(0:nrages)
real               :: ages_female(0:nrages)
integer i,k



! Group 1:  0-5    years old   Kindergarden
! Group 2:  6-12   years old   Children school
! Group 3:  13-19  years old   Juvenile and highschool
! Group 4:  20-29  years old
! Group 5:  30-39  years old
! Group 6:  40-49  years old
! Group 7:  50-59  years old
! Group 8:  60-69  years old
! Group 9:  70-79  years old   Retired and vounerable
! Group 10: 80-89  years old   Retired and vounerable
! Group 11: 90-105 years old   Retired and vounerable 

  integer ia(0:na-1)
  integer ib(0:na-1)

  ia( 0)=0 ; ib( 0)=5
  ia( 1)=6 ; ib( 1)=12
  ia( 2)=13; ib( 2)=19
  ia( 3)=20; ib( 3)=29
  ia( 4)=30; ib( 4)=39
  ia( 5)=40; ib( 5)=49
  ia( 6)=50; ib( 6)=59
  ia( 7)=60; ib( 7)=69
  ia( 8)=70; ib( 8)=79
  ia( 9)=80; ib( 9)=89
  ia(10)=90; ib(10)=105

! Numbers from https://www.ssb.no/statbank/table/07459/
  ages_male(0  )= 28208.0
  ages_male(1  )= 28847.0
  ages_male(2  )= 29823.0
  ages_male(3  )= 31261.0
  ages_male(4  )= 31274.0
  ages_male(5  )= 31375.0
  ages_male(6  )= 31274.0
  ages_male(7  )= 32462.0
  ages_male(8  )= 32834.0
  ages_male(9  )= 33634.0
  ages_male(10 )= 34323.0
  ages_male(11 )= 33903.0
  ages_male(12 )= 32987.0
  ages_male(13 )= 33146.0
  ages_male(14 )= 32229.0
  ages_male(15 )= 32521.0
  ages_male(16 )= 32300.0
  ages_male(17 )= 31684.0
  ages_male(18 )= 32696.0
  ages_male(19 )= 34517.0
  ages_male(20 )= 34794.0
  ages_male(21 )= 34159.0
  ages_male(22 )= 35129.0
  ages_male(23 )= 36127.0
  ages_male(24 )= 36264.0
  ages_male(25 )= 36383.0
  ages_male(26 )= 36912.0
  ages_male(27 )= 38223.0
  ages_male(28 )= 38820.0
  ages_male(29 )= 39360.0
  ages_male(30 )= 39255.0
  ages_male(31 )= 39000.0
  ages_male(32 )= 37580.0
  ages_male(33 )= 37893.0
  ages_male(34 )= 37324.0
  ages_male(35 )= 36743.0
  ages_male(36 )= 37027.0
  ages_male(37 )= 37036.0
  ages_male(38 )= 35869.0
  ages_male(39 )= 36693.0
  ages_male(40 )= 36077.0
  ages_male(41 )= 35875.0
  ages_male(42 )= 34706.0
  ages_male(43 )= 35713.0
  ages_male(44 )= 36319.0
  ages_male(45 )= 37771.0
  ages_male(46 )= 38163.0
  ages_male(47 )= 39253.0
  ages_male(48 )= 39033.0
  ages_male(49 )= 38611.0
  ages_male(50 )= 39317.0
  ages_male(51 )= 38884.0
  ages_male(52 )= 37722.0
  ages_male(53 )= 37814.0
  ages_male(54 )= 37040.0
  ages_male(55 )= 36245.0
  ages_male(56 )= 34357.0
  ages_male(57 )= 33631.0
  ages_male(58 )= 32836.0
  ages_male(59 )= 32363.0
  ages_male(60 )= 32248.0
  ages_male(61 )= 31570.0
  ages_male(62 )= 30629.0
  ages_male(63 )= 30668.0
  ages_male(64 )= 29701.0
  ages_male(65 )= 28740.0
  ages_male(66 )= 28104.0
  ages_male(67 )= 27604.0
  ages_male(68 )= 26278.0
  ages_male(69 )= 26318.0
  ages_male(70 )= 26058.0
  ages_male(71 )= 26143.0
  ages_male(72 )= 25994.0
  ages_male(73 )= 26396.0
  ages_male(74 )= 22809.0
  ages_male(75 )= 21460.0
  ages_male(76 )= 18447.0
  ages_male(77 )= 16354.0
  ages_male(78 )= 13571.0
  ages_male(79 )= 13417.0
  ages_male(80 )= 12269.0
  ages_male(81 )= 11121.0
  ages_male(82 )= 9870.0
  ages_male(83 )= 8714.0
  ages_male(84 )= 7548.0
  ages_male(85 )= 6805.0
  ages_male(86 )= 6097.0
  ages_male(87 )= 5674.0
  ages_male(88 )= 4752.0
  ages_male(89 )= 4161.0
  ages_male(90 )= 3251.0
  ages_male(91 )= 2661.0
  ages_male(92 )= 2077.0
  ages_male(93 )= 1671.0
  ages_male(94 )= 1263.0
  ages_male(95 )= 893.0
  ages_male(96 )= 622.0
  ages_male(97 )= 405.0
  ages_male(98 )= 229.0
  ages_male(99 )= 161.0
  ages_male(100)=  87.0
  ages_male(101)=  43.0
  ages_male(102)=  17.0
  ages_male(103)=  12.0
  ages_male(104)=  12.0
  ages_male(105)=  19.0
  ages_female(0  )= 26619.0
  ages_female(1  )= 27165.0
  ages_female(2  )= 28058.0
  ages_female(3  )= 29261.0
  ages_female(4  )= 29547.0
  ages_female(5  )= 29721.0
  ages_female(6  )= 30118.0
  ages_female(7  )= 30789.0
  ages_female(8  )= 31047.0
  ages_female(9  )= 32041.0
  ages_female(10 )=  32507.0
  ages_female(11 )=  32008.0
  ages_female(12 )=  31319.0
  ages_female(13 )=  31494.0
  ages_female(14 )=  30853.0
  ages_female(15 )=  30887.0
  ages_female(16 )=  30722.0
  ages_female(17 )=  30367.0
  ages_female(18 )=  30861.0
  ages_female(19 )=  32067.0
  ages_female(20 )=  32019.0
  ages_female(21 )=  32005.0
  ages_female(22 )=  32688.0
  ages_female(23 )=  33822.0
  ages_female(24 )=  33822.0
  ages_female(25 )=  34470.0
  ages_female(26 )=  35143.0
  ages_female(27 )=  36110.0
  ages_female(28 )=  37214.0
  ages_female(29 )=  38288.0
  ages_female(30 )=  37899.0
  ages_female(31 )=  37654.0
  ages_female(32 )=  36480.0
  ages_female(33 )=  35879.0
  ages_female(34 )=  35260.0
  ages_female(35 )=  35043.0
  ages_female(36 )=  34410.0
  ages_female(37 )=  34780.0
  ages_female(38 )=  34297.0
  ages_female(39 )=  34425.0
  ages_female(40 )=  33954.0
  ages_female(41 )=  33625.0
  ages_female(42 )=  32962.0
  ages_female(43 )=  33568.0
  ages_female(44 )=  34716.0
  ages_female(45 )=  36151.0
  ages_female(46 )=  35886.0
  ages_female(47 )=  37168.0
  ages_female(48 )=  37322.0
  ages_female(49 )=  36790.0
  ages_female(50 )=  37524.0
  ages_female(51 )=  37067.0
  ages_female(52 )=  36116.0
  ages_female(53 )=  35489.0
  ages_female(54 )=  34958.0
  ages_female(55 )=  34348.0
  ages_female(56 )=  33208.0
  ages_female(57 )=  32098.0
  ages_female(58 )=  31634.0
  ages_female(59 )=  31179.0
  ages_female(60 )=  31057.0
  ages_female(61 )=  30961.0
  ages_female(62 )=  30212.0
  ages_female(63 )=  30547.0
  ages_female(64 )=  29630.0
  ages_female(65 )=  29002.0
  ages_female(66 )=  28550.0
  ages_female(67 )=  27818.0
  ages_female(68 )=  26227.0
  ages_female(69 )=  26631.0
  ages_female(70 )=  26516.0
  ages_female(71 )=  26497.0
  ages_female(72 )=  27035.0
  ages_female(73 )=  27605.0
  ages_female(74 )=  24399.0
  ages_female(75 )=  22981.0
  ages_female(76 )=  20104.0
  ages_female(77 )=  18353.0
  ages_female(78 )=  15780.0
  ages_female(79 )=  15915.0
  ages_female(80 )=  15010.0
  ages_female(81 )=  13950.0
  ages_female(82 )=  12765.0
  ages_female(83 )=  11770.0
  ages_female(84 )=  10698.0
  ages_female(85 )=  10004.0
  ages_female(86 )=  9301.0
  ages_female(87 )=  9106.0
  ages_female(88 )=  8328.0
  ages_female(89 )=  7537.0
  ages_female(90 )=  6336.0
  ages_female(91 )=  5499.0
  ages_female(92 )=  4620.0
  ages_female(93 )=  3966.0
  ages_female(94 )=  3106.0
  ages_female(95 )=  2542.0
  ages_female(96 )=  1813.0
  ages_female(97 )=  1328.0
  ages_female(98 )=  980.0
  ages_female(99 )=  688.0
  ages_female(100)=  358.0
  ages_female(101)=  227.0
  ages_female(102)=  166.0
  ages_female(103)=  73.0
  ages_female(104)=  51.0
  ages_female(105)=  54.0

  do i=0,na-1
     agegroup(i)=sum(ages_male(ia(i):ib(i)))    + sum(ages_female(ia(i):ib(i)))
     print '(a,i2,a,i2,a,i3,a,f9.0)','Population in agegroup: ',i,' agerange(',ia(i),'--',ib(i),')= ',agegroup(i)
  enddo
  N=sum(agegroup(:))
  print '(a,2f13.0)','Total Norwegan population: ',sum(ages_male(:)) + sum(ages_female(:)) , N
  print *

end subroutine
end module
