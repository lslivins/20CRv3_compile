for file in *.f90; do
   file2=`basename $file`
   diff -urN ../../../branches/EXP-hybens_reorg/util/src/$file2 $file
done
