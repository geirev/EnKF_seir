for i in Case*
do
   sed -i '2s/226/213/' $i/infile.in
   sed -i '3s/225/212/' $i/infile.in
done
