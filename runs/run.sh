dir=~/Dropbox/Apps/Overleaf/Corona

for country in *   # for each country
do
   if [ -d $country ]
   then
      for exp in $country/Case*
      do
         pushd $exp
         if [ -f "infile.in" ]
         then
            seir
         fi
         popd
#         tec360 -b plotsD.mcr
#         rm batch.log
#         mv CR.eps $dir/CRO_0$i.eps
#         mv HD.eps $dir/HDO_0$i.eps
#         mv IE.eps $dir/IEO_0$i.eps
#         mv BIG.eps $dir/BIGO$i.eps
#         mv bigdump1.dat bigdumpO_$i.dat
#         mv RENS.eps $dir/RENS$i.eps
      done
   fi 
done


