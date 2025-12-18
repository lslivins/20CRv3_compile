module load cray-hdf5
set date=`incdate $1 -3`
set date2=`incdate $1 +3`
set fileout=$2
#setenv obsdirh5 /scratch2/scratchdirs/whitaker/HDF5/v321/
/bin/rm -f $fileout
touch $fileout
while ($date <= $date2)
   echo $date
   $HOMEGLOBAL/bin/h5ftotxt $date >> $fileout
   set date=`incdate $date 1`
end
