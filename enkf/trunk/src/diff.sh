for file in *orig; do
   file2=`basename $file .orig`
   echo "$file $file2"
   diff -urN $file $file2
done
