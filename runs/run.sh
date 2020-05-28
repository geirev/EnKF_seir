dir=~/Dropbox/Apps/Overleaf/Corona

prior=0
runseir=0
endday=44060

for country in Norway #US   #Netherlands France Argentina Netherlands Quebec Brazil
do
   if [ -d $country ]
   then
      for exp in $country/Case01
      do
         [ ! -f ${exp}/ylimits.txt ] && cp ylimits.txt ${exp}
         pushd $exp
         if [ -f "infile.in" ] && [ -f "corona.in" ]
         then
            if [ $country == "Quebec" ]
            then 
               executable=seirQuebec
            else
               executable=seir
            fi
            [ ${runseir} -eq 1 ] && ${executable} | tee out.dat
            [ ! -f out.dat ] && exit
            [ ! -f dead_1.dat ] && exit

            refday=$(grep "Relative start day" out.dat | cut -c36-38)
            echo Reference day $refday
            startday=$(($refday+43466))
            echo "Startday in days from December 30, 1899: " ${startday}
            [ -f xlimit.txt ] && endday=$(grep "MAXX" xlimit.txt | cut -f2 -d"=")
            echo "Endday   in days from December 30, 1899: " ${endday}

            cp ../../solutionsD.lay ./solutions.lay


            [ ! -f ylimits.txt ] && cp ../../ylimits.txt .
            ymaxD=$(grep "D" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxC=$(grep "C" ylimits.txt | tr -s " " | cut -f2 -d" ")
            ymaxI=$(grep "I" ylimits.txt | tr -s " " | cut -f2 -d" ")

            [ ! -f legend.txt ] && cp ../../legend.txt .
            legX=$(grep "LEGXHDlog" legend.txt | cut -f2 -d"=")
            legY=$(grep "LEGYHDlog" legend.txt | cut -f2 -d"=")

            cat ../../plotsD.mcr    | sed -e "s/XXXXA/${startday}/g"   \
                                          -e "s/XXXXB/${endday}/g"     \
                                          -e "s/YYYYD/${ymaxD}/g"  \
                                          -e "s/YYYYI/${ymaxI}/g"  \
                                          -e "s/XXPRIXX/${prior}/g"  \
                                          -e "s/LEGXX/${legX}/g"  \
                                          -e "s/LEGYY/${legY}/g"  \
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


