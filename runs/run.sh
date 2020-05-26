dir=~/Dropbox/Apps/Overleaf/Corona

prior=0
runseir=0

for country in Quebec # Netherlands #Brazil #Argentina   Norway Brazil France   # for each country
do
   if [ -d $country ]
   then
    
      for exp in $country/Case*
      do
         [ ! -f ${exp}/ylimits.txt ] && cp ylimits.txt ${exp}
         pushd $exp
         if [ -f "infile.in" ] && [ -f "corona.in" ]
         then
            [ ${runseir} -eq 1 ] && seir | tee out.dat
            [ ! -f out.dat ] && exit
            [ ! -f dead_1.dat ] && exit
            startdate=$(grep "Relative start day" out.dat | cut -c36-38)
            echo startdate $startdate
            date=$(($startdate+43466))
            echo "Days from December 30, 1899: " ${date}

            ymaxD=$(grep "D" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxC=$(grep "C" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxI=$(grep "I" ylimits.txt | tr -s " " | cut -f2 -d" ")

            cp ../../solutionsD.lay ./solutions.lay
            cat ../../plotsD.mcr    | sed -e "s/XXXXA/${date}/g"   \
                                          -e "s/XXXXB/44060/g"     \
                                          -e "s/YYYYD/${ymaxD}/g"  \
                                          -e "s/YYYYI/${ymaxI}/g"  \
                                          -e "s/XXPRIXX/${prior}/g"  \
                                          -e "s/YYYYC/${ymaxC}/g"  > ./plots.mcr
            tec360 -b plots.mcr
            rm -f batch.log
            case=${PWD##*/}          

            cp HD.eps ${dir}/${country}_${case}_HD.eps
            cp HDlog.eps ${dir}/${country}_${case}_HDlog.eps
            cp RENS.eps ${dir}/${country}_${case}_RENS.eps

         fi
         popd
      done

      if [ -f ${country}/plots.mcr ]
      then
         pushd $country
         tec360 -b plots.mcr
         cp *.eps ${dir}
         popd
      fi

   fi 
done


