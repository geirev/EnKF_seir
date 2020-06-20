for i in Norway/Case*
do
  pushd $i > /dev/null
  xx=$(grep "XDetail 1 {RangeMax =" plots.mcr  | cut -f2 -d= | sort -u  | sed -e "s/ //g" -e "s/}//g")
  echo $(pwd) $xx
  [ ! -f xlimit.txt ] && echo MAXX=${xx}   >> xlimit.txt
  popd > /dev/null
done
