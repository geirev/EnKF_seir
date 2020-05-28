for i in Case01_MDA*/out.dat; do   echo "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%";   echo $i;   tail -18 $i;   echo; done  > mda.out
