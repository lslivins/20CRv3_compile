for file in *orig; do
   file2=`basename $file .orig`
   diff -urN $file $file2
done
