dir=~/Dropbox/Apps/Overleaf/Corona
src=~/Dropbox/EnKF_seir/src

rdim=400
prior=0
runseir=0
endday=44060
executable=seir

active_branch=$(git rev-parse --abbrev-ref HEAD)
if [ $active_branch != "paper_version" ]
then
   echo "You need to stash or commit everything and check out the branch paper_version"
   exit
fi


# Recompile if rdim has been set different from 400
if [ ${runseir} -eq 1 ]
then
   if ! grep -q "rdim=${rdim}" ${src}/mod_dimensions.F90
   then
      pushd $src
      sed -i "s/rdim=.*/rdim=${rdim}/" mod_dimensions.F90
      git checkout paper_version -- m_enkfprep.F90
      make
      popd
   fi
fi

for country in France #US Quebec Netherlands Brazil Argentina France England Norway 
do
   if [ -d $country ]
   then
      for exp in $country/xase*
      do
         [ -d $exp ] && pushd $exp
         [ ! -f ylimits.txt ] && cp ../../ylimits.txt ${exp}
         if [ -f "infile.in" ] && [ -f "corona.in" ]
         then

##################################################################
# Quebec specifics using different rdim values (constant R in predictions)
            if [ $country == "Quebec" ] && [ ${runseir} -eq 1 ]
            then 
               [ "${exp}" == "${country}/Case01" ] && newrdim=82
               [ "${exp}" == "${country}/Case02" ] && newrdim=82
               [ "${exp}" == "${country}/Case03" ] && newrdim=82
               [ -f "rdim.txt" ] && newrdim=$(grep "rdim=" rdim.txt | cut -f2 -d= | cut -f1 -d-)
               echo DIM = ${newrdim}
               pushd $src
               sed -i "s/rdim=.*/rdim=${newrdim}/" mod_dimensions.F90
               git checkout Quebec -- m_enkfprep.F90
               make  || (echo "make B failed $$?"; exit 1)
               git checkout paper_version -- m_enkfprep.F90
               popd
            fi

##################################################################
# Running the seir DA system
            [ ${runseir} -eq 1 ] && ${executable}  | tee out.dat 
            [ ! ${PIPESTATUS[0]} ] && exit
            [ ! -f out.dat ] && exit
            [ ! -f dead_1.dat ] && exit

            refday=$(grep "Relative start day" out.dat | cut -c36-38)
            echo Reference day $refday
            startday=$(($refday+43465))
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


            [ "$exp" == "Netherlands/Case01I" ] && sed -i 's/Hospitalized/ICU patients/g' ./plots.mcr
            [ "$country" == "Quebec" ]          && sed -i 's/#Q//g' ./plots.mcr
            [ "$country" == "US" ]              && sed -i 's/#US//g' ./plots.mcr

            tec360 -b plots.mcr
            rm -f batch.log
            case=${PWD##*/}          

            cp HD.eps ${dir}/${country}_${case}_HD.eps
            cp HDlog.eps ${dir}/${country}_${case}_HDlog.eps
            cp RENS.eps ${dir}/${country}_${case}_RENS.eps

         fi
         popd
      done

##################################################################
# Scenario plots for ${country} if ${country}/plots.mcr exists
      if [ -f ${country}/plots.mcr ]
      then
         pushd $country
         tec360 -b plots.mcr
         cp *.eps ${dir}
         popd
      fi

##################################################################
# Quebec specifics (fix the messup from before)
      if [ $country == "Quebec" ] && [ ${runseir} -eq 1 ]
      then 
         pushd $src
         sed -i "s/rdim=.*/rdim=400/" mod_dimensions.F90
         git checkout paper_version -- m_enkfprep.F90
         make  || (echo "make C failed $$?"; exit 1)
         popd
      fi
##################################################################


   fi 
done


