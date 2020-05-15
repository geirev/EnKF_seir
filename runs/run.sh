dir=~/Dropbox/Apps/Overleaf/Corona

for country in Norway   # for each country
do
   if [ -d $country ]
   then
      for exp in $country/Case*
      do
         pushd $exp
         if [ -f "infile.in" ] && [ -f "corona.in" ]
         then
            seir | tee out.dat
            startdate=$(grep "Relative start day" out.dat | cut -c36-38)
            echo startdate $startdate
            date=$(($startdate+43466))
            echo "Days from December 30, 1899: " ${date}
            cat ../../rensD.lay     | sed -e "s/XXXXX/${date}/g"  > ./rens.lay
            cat ../../solutionsD.lay | sed -e "s/XXXXX/${date}/g"  > ./solutions.lay
            cat ../../plotsD.mcr    | sed -e "s/XXXXX/${date}/g" -e "s/YYYYY/44000/g"   > ./plots.mcr
            tec360 -b plots.mcr
            rm -f batch.log
            case=${PWD##*/}          

            mv CR.eps ${dir}/${country}_${case}_CR.eps
            mv HD.eps ${dir}/${country}_${case}_HD.eps
            mv IE.eps ${dir}/${country}_${case}_IE.eps
            mv RENS.eps ${dir}/${country}_${case}_RENS.eps

         fi
         popd
      done
   fi 
done


