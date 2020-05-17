dir=~/Dropbox/Apps/Overleaf/Corona

for country in Norway Brazil France   # for each country
do
   if [ -d $country ]
   then
      for exp in $country/Case*
      do
         [ ! -f ${exp}/ylimits.txt ] && cp ylimits.txt ${exp}
         pushd $exp
         if [ -f "infile.in" ] && [ -f "corona.in" ]
         then
            seir | tee out.dat
            startdate=$(grep "Relative start day" out.dat | cut -c36-38)
            echo startdate $startdate
            date=$(($startdate+43466))
            echo "Days from December 30, 1899: " ${date}

            ymaxD=$(grep "D" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxC=$(grep "C" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxI=$(grep "I" ylimits.txt | tr -s " " | cut -f2 -d" ")

            cat ../../rensD.lay     | sed -e "s/XXXXA/${date}/g"  > ./rens.lay
            cat ../../solutionsD.lay | sed -e "s/XXXXA/${date}/g" > ./solutions.lay
            cat ../../solutionsL.lay | sed -e "s/XXXXA/${date}/g" > ./solutionslog.lay
            cat ../../plotsD.mcr    | sed -e "s/XXXXA/${date}/g"   \
                                          -e "s/XXXXB/44060/g"     \
                                          -e "s/YYYYD/${ymaxD}/g"  \
                                          -e "s/YYYYI/${ymaxI}/g"  \
                                          -e "s/YYYYC/${ymaxC}/g"  > ./plots.mcr
            tec360 -b plots.mcr
            rm -f batch.log
            case=${PWD##*/}          

            mv CR.eps ${dir}/${country}_${case}_CR.eps
            mv HD.eps ${dir}/${country}_${case}_HD.eps
            mv HDlog.eps ${dir}/${country}_${case}_HDlog.eps
            mv IE.eps ${dir}/${country}_${case}_IE.eps
            mv RENS.eps ${dir}/${country}_${case}_RENS.eps

         fi
         popd
      done
   fi 
done


